%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 12:20 PM
%%%-------------------------------------------------------------------
-module(lldp_handler).
-author("vdasari").

-behaviour(gen_server).

-include("logger.hrl").
-include("lldp_api.hrl").
%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([info/2, rx_packet/2, interface/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(MaxEntities, 1).
-define(HoldTimer, 30).
-define(ReinitTimer, 2).
-define(RTimer, 3).
-define(SERVER, ?MODULE).

-record(state, {
    name,
    module,
    callback_state,
    ets_tab,
    timer_ref,
    if_map = #{}
}).

-record(lldp_handler_intf_t, {
    if_name,
    if_info :: #lldp_entity_t{},
    rx_pkts = 0,
    tx_pkts = 0,
    ageouts = 0,
    inserted = 0,
    deleted = 0,
    tx_data :: binary(),
    nbr_list = #{}
}).
-define(call(A),gen_server:call(?MODULE, A)).
-define(cast(A),gen_server:cast(?MODULE, A)).

%%%===================================================================
%%% API
%%%===================================================================

info(Pid, Args) ->
    gen_server:call(Pid, Args).

interface(Op, IfName, IfInfo) ->
    gen_server:cast(self(), {interface, Op, IfName, IfInfo}).

rx_packet(IfName, Data) ->
    gen_server:cast(self(), {rx_packet,IfName,Data}).

start_link(Name, Cfg) ->
    gen_server:start_link(?MODULE, [Name, Cfg], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([CallbackModule, CallbackState]) ->
    ?INFO("Starting ~s with ~p, ~p, pid ~p", [?MODULE_STRING, CallbackModule, CallbackState, self()]),
    self() ! {init},
    {ok, #state{
        name = CallbackModule,
        module = CallbackModule,
        callback_state = CallbackState
    }}.

handle_call(Request, From, State) ->
    try process_call(Request, State) of
        {reply, ok, _} = Return ->
            ?DEBUG("call: Request From ~p, Returns ~p~n~p", [From, ok, Request]),
            Return;
        {reply, NotOk, _} = Return when is_atom(NotOk) ->
            ?INFO("call: Request From ~p, Returns ~p~n~p", [From, NotOk, Request]),
            Return;
        Return ->
            Return
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            ?ERROR("Failed:~n    Request ~p~n    From ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Request, From, Error, Reason, lldp_utils:pretty_print(StackTrace)]),
            {reply, Error, State}
    end.

handle_cast(Request, State) ->
    ?DEBUG("cast: Request ~p", [Request]),
    try process_cast(Request, State) of
        Return ->
            Return
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            ?ERROR("Failed:~n    Request ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Request, Error, Reason, lldp_utils:pretty_print(StackTrace)]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    ?DEBUG("info: Request ~p", [Info]),
    try process_info_msg(Info, State) of
        Return ->
            Return
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            ?ERROR("Failed:~n    Request ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Info, Error, Reason, lldp_utils:pretty_print(StackTrace)]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ?INFO("~s going down: ~p", [?MODULE, _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
process_call(Request, State) when element(1, Request) == info ->
    {reply, lists:flatten(do_info(Request, State)), State};
process_call(Request, State) ->
    ?INFO("call: Unhandled Request ~p", [Request]),
    {reply, ok, State}.

process_cast({interface,Op, IfName, IfInfo}, State) ->
    {noreply, do_interface(Op, IfName, IfInfo, State)};

process_cast({rx_packet, IfName, Data}, #state{if_map = IfMap} = State) ->
    #{IfName := IfInfo} = IfMap,
    {noreply, State#state{if_map = IfMap#{IfName => do_rx_packet(IfInfo, Data, State)}}};

process_cast(Request, State) ->
    {noreply, State#state{callback_state = dispatch(Request, State)}}.

process_info_msg({init}, #state{name = Name} = State) ->
    {noreply, State#state{
        callback_state = dispatch(started, State),
        ets_tab = ets:new(Name, [named_table, public, set]),
        timer_ref = erlang:send_after(?HoldTimer * 1000, self(), hold_timer_expired)
    }};

process_info_msg(hold_timer_expired, State) ->
    NewState = do_tx_packet(State),
    {noreply, State#state{
        if_map = do_hold_timer_expired(NewState),
        timer_ref = erlang:send_after(?HoldTimer * 1000, self(), hold_timer_expired)
    }};

process_info_msg({reinit_timer_expired, IfName}, #state{if_map = IfMap} = State) ->
    #{IfName := #lldp_handler_intf_t{
        tx_data = TxData, tx_pkts = TxPkts
    } = IfInfo} = IfMap,
    {noreply, State#state{
        callback_state = dispatch({tx_packet, IfName, TxData}, State),
        if_map = #{IfName => IfInfo#lldp_handler_intf_t{
            tx_pkts = TxPkts+1
        }}
    }};

process_info_msg(Request, State) ->
    {noreply, State#state{callback_state = dispatch(Request, State)}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
do_interface(create, IfName, IfInfo, #state{if_map = IfMap} = State) ->
    erlang:send_after(?ReinitTimer * 1000, self(), {reinit_timer_expired, IfName}),
    State#state{
        if_map = IfMap#{
            IfName => #lldp_handler_intf_t{
                if_name = IfName,
                if_info = IfInfo,
                tx_data = lldp_pdu:encode(IfInfo)
            }
        }
    }.

do_rx_packet(
        #lldp_handler_intf_t{
            if_name = IfName,
            nbr_list = NbrList,
            rx_pkts = RxPkts
        } = IfInfo, Data, #state{ets_tab = Table} = State) ->
    #lldp_entity_t{
        chassis_id = ChassisId, port_id = PortId, ttl = Ttl
    } = DecodeData = lldp_pdu:decode(Data),
    NbrKey = {ChassisId, PortId},

    case maps:get(NbrKey, NbrList, []) of
        #lldp_entity_t{} = E when E /= DecodeData ->
            dispatch(notify, {update, IfName, DecodeData}, State);
        [] ->
            dispatch(notify, {add, IfName, DecodeData}, State);
        _ ->
            ok
    end,

    ets:match_delete(Table, {{'_',NbrKey},'_'}),
    ets:insert(Table, {{qdate:add_seconds(Ttl), NbrKey}, IfName}),

    IfInfo#lldp_handler_intf_t{
        rx_pkts = RxPkts+1,
        nbr_list = NbrList#{
            NbrKey => DecodeData
        }
    }.

do_tx_packet(#state{if_map = IfMap} = State) ->
    NewIfMap = maps:map(fun
        (IfName, #lldp_handler_intf_t{tx_data = TxData, tx_pkts = TxPkts} = Value) ->
            dispatch({tx_packet, IfName, TxData}, State),
            Value#lldp_handler_intf_t{tx_data = TxData, tx_pkts = TxPkts+1}
    end, IfMap),
    State#state{if_map = NewIfMap}.

do_hold_timer_expired(#state{ets_tab = Table, if_map = IfMap} = State) ->
    CurrentTime = qdate:unixtime(),
    case ets:select(Table,
        ets:fun2ms(fun
            ({{ExpiryTime, L}, IfName}) when ExpiryTime < CurrentTime ->
                {IfName, {ExpiryTime,L}}
        end)) of
        Entities when is_list(Entities) ->
            lists:foldl(fun
                ({IfName, {_, NbrKey} = Key}, Acc) ->
                    #{IfName := #lldp_handler_intf_t{nbr_list = NbrList} = IfInfo} = Acc,
                    ets:delete(Table, Key),
                    dispatch(notify, {delete, IfName, NbrKey}, State),
            Acc#{IfName => IfInfo#lldp_handler_intf_t{nbr_list = maps:remove(NbrKey, NbrList)}}
            end, IfMap, Entities);
        _ ->
            IfMap
    end.

do_info({info,brief}, #state{ets_tab = Table} = State) ->
    io_lib:format("~16s ~16s ~4w ~4w", [
        State#state.name,
        State#state.module,
        map_size(State#state.if_map),
        proplists:get_value(size, ets:info(Table))
    ]);
do_info({info, _}, #state{if_map = IfMap}) ->
    Header =
        io_lib:format("~6c ~18c ~12c ~12c ~16c ~4c ~10c ~10c~n",[
            $-, $-, $-, $-,$-, $-, $-, $-
        ]) ++
        io_lib:format("~6s ~18s ~12s ~12s ~16s ~4s ~10s ~10s~n",[
            "IfName", "Chassis Id", "Port Id", "System Name", "Ip Address", "Nbrs", "Rx Pkts", "Tx Pkts"
        ]) ++
        io_lib:format("~6c ~18c ~12c ~12c ~16c ~4c ~10c ~10c~n",[
            $-, $-, $-, $-,$-, $-, $-, $-
        ]),
    Header ++ maps:fold(fun
        (IfName, #lldp_handler_intf_t{if_info = Entity} = IfInfo, Acc) ->
            io_lib:format("~6s ~18s ~12s ~12s ~16s ~4w ~10w ~10w~n", [
                IfName,
                inet_utils:convert_mac(to_string, Entity#lldp_entity_t.chassis_id),
                binary_to_list(Entity#lldp_entity_t.port_id),
                binary_to_list(Entity#lldp_entity_t.sys_name),
                inet_utils:convert_ip(to_string, Entity#lldp_entity_t.mgmt_ip),
                map_size(IfInfo#lldp_handler_intf_t.nbr_list),
                IfInfo#lldp_handler_intf_t.rx_pkts,
                IfInfo#lldp_handler_intf_t.tx_pkts
            ]) ++ Acc
    end, [], IfMap);

do_info({info, _, all, neighbors}, #state{if_map = IfMap} = State) ->
    maps:fold(fun
        (IfName, _, Acc) ->
            do_info({info, '_', IfName, neighbors}, State) ++ Acc
    end, [], IfMap);

do_info({info, _, IfName, neighbors}, #state{if_map = IfMap, ets_tab = Table}) ->
    Header =
        io_lib:format("~18c ~18c ~3c ~12c ~18c ~18c~n",[
            $-, $-, $-, $-,$-, $-
        ]) ++
        io_lib:format("~18s ~18s ~3s ~12s ~18s ~18s~n",[
            "Chassis Id", "Port Id", "Ttl", "System Name", "Ip Address", "Expires At"
        ]) ++
            io_lib:format("~18c ~18c ~3c ~12c ~18c ~18c~n",[
                $-, $-, $-, $-,$-, $-
            ]),

    #{IfName := #lldp_handler_intf_t{nbr_list = NbrsMap}} = IfMap,

    Header ++ maps:fold(fun
        (NbrKey, #lldp_entity_t{} = Entity, Acc) ->
            [{{ExpiresAt,_},_}] = ets:match_object(Table, {{'_', NbrKey},'_'}),
            io_lib:format("~18s ~18s ~3w ~12s ~18s ~18s~n", [
                inet_utils:convert_mac(to_string, Entity#lldp_entity_t.chassis_id),
                (Entity#lldp_entity_t.port_id),
                Entity#lldp_entity_t.ttl,
                (Entity#lldp_entity_t.sys_name),
                Entity#lldp_entity_t.mgmt_ip,
                qdate:to_string("n/j/Y g:ia", ExpiresAt)
            ]) ++ Acc
    end, [], NbrsMap);

do_info(Request, State) ->
    io_lib:format("Request ~p", [Request]),
    dispatch(info, {Request}, State).

%%%===================================================================
%%% Utility functions
%%%===================================================================

dispatch(Message, State) ->
    dispatch(handle_message, [Message], State).

dispatch(Function, Message, #state{module = CallbackModule, callback_state = CallbackState}) ->
    Args = case Message of
        _ when is_tuple(Message) ->
            tuple_to_list(Message) ++ [CallbackState];
        _ when is_list(Message) ->
            Message ++ [CallbackState];
        _ ->
            [Message, CallbackState]
    end,
    {ok, NewState} = erlang:apply(CallbackModule, Function, Args),
    NewState.