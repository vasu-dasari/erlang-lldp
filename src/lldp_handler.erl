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

-export([event/2, rx_packet/2]).

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
    tx_data :: binary(),
    nbr_list = #{}
}).
-define(call(A),gen_server:call(?MODULE, A)).
-define(cast(A),gen_server:cast(?MODULE, A)).

%%%===================================================================
%%% API
%%%===================================================================

event(Pid, Event) ->
    gen_server:cast(Pid, Event).

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
        } = IfInfo, Data, #state{ets_tab = Table}) ->
    #lldp_entity_t{
        chassis_id = ChassisId, port_id = PortId, ttl = Ttl
    } = DecodeData = lldp_pdu:decode(Data),
    NbrKey = {ChassisId, PortId},

    ets:match_delete(Table, {{'_',NbrKey},'_'}),
    ets:insert(Table, {{lldp_utils:get_timestamp() + Ttl, NbrKey}, IfName}),

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

do_hold_timer_expired(#state{ets_tab = Table, if_map = IfMap}) ->
    CurrentTime = lldp_utils:get_timestamp(),
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
                    Acc#{IfName => IfInfo#lldp_handler_intf_t{nbr_list = maps:remove(NbrKey, NbrList)}}
            end, IfMap, Entities);
        _ ->
            IfMap
    end.

%%%===================================================================
%%% Utility functions
%%%===================================================================

dispatch(Message, #state{module = CallbackModule, callback_state = CallbackState}) ->
    {ok, NewState} = CallbackModule:handle_message(Message, CallbackState),
    NewState.