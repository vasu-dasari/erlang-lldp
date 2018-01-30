%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 9:56 AM
%%%-------------------------------------------------------------------
-module(lldp_netlink).
-author("vdasari").

%%-behavior(gen_server).
-behavior(gen_lldp).

-include("lldp_api.hrl").
-include("logger.hrl").
-include_lib("gen_netlink/include/netlink.hrl").

%% API
-export([]).

-export([init/1, handle_message/2, terminate/2, scan_ifs/1, notify/4, info/2]).

-record(state, {
    exclude_intfs = [],
    include_intfs = [],
    gw_intf = [],
    chassis_id = <<>>,
    sys_name = <<>>,
    sys_descr = <<>>,
    if_map = #{},
    port_map = #{}
}).

-record(lldp_netlink_intf_t, {
    if_name,
    sock_fd,
    sock_port
}).
init(Config) ->
    ?INFO("Starting netlink process", []),
    {ok, #state{
        exclude_intfs = proplists:get_value(exclude_intfs, Config, []),
        include_intfs = proplists:get_value(include_intfs, Config, [])
    }}.

handle_message(started, State) ->
    netlink:subscribe(self(), [rt]),
    State1 = populate_system_info(State),
    {ok, scan_ifs(State1)};

handle_message(
        {rtnetlink, [
            #rtnetlink{type = newlink, msg = {_,_,_, _, _, PropList}}
        ]},
        #state{if_map = IfMap} = State) ->

    IfName = proplists:get_value(ifname, PropList),
    OperState = proplists:get_value(operstate, PropList),

    NewState = case maps:get(IfName, IfMap, []) of
        #lldp_netlink_intf_t{} when OperState == up ->
            lldp_handler:interface(update, IfName, up),
            setup_socket(open, IfName, State);
        #lldp_netlink_intf_t{} when OperState == down ->
            lldp_handler:interface(update, IfName, down),
            setup_socket(close, IfName, State);
        _ ->
            State
    end,
    {ok, NewState};

handle_message({rtnetlink, _} , State) ->
    {ok, State};

handle_message({Port,{data,Data}} , #state{port_map = PortMap} = State) ->
    case maps:get(Port, PortMap, []) of
        [] ->
            ok;
        #lldp_netlink_intf_t{if_name = IfName} ->
            lldp_handler:rx_packet(IfName, Data)
    end,
    {ok, State};

handle_message({tx_packet, IfName, TxData}, #state{if_map = IfMap} = State) ->
    #{IfName := #lldp_netlink_intf_t{sock_fd = SockFd}} = IfMap,
    procket:sendto(SockFd, TxData),
    {ok, State};

handle_message({info, IfName, TxData}, #state{if_map = IfMap} = State) ->
    #{IfName := #lldp_netlink_intf_t{sock_fd = SockFd}} = IfMap,
    procket:sendto(SockFd, TxData),
    {ok, State};

handle_message(Message, State) ->
    ?INFO("Message: ~p", [Message]),
    {ok, State}.

notify(Op, IfName, EntityInfo, State) ->
    ?INFO("~p Neighbor on ~p~n~s", [Op,IfName, lldp_utils:record_to_proplist(to_str, EntityInfo)]),
    {ok, State}.

info(_Request, _State) ->
    {ok, "Hello World"}.

terminate(Reason, State) ->
    ?INFO("Terminate: ~p, State ~p", [Reason, State]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
populate_system_info(State) ->
    GwIfName = case inet_ext:gateways() of
        [{Gw, _} | _] -> Gw;
        _ -> []
    end,

    {ok, RawIfList} = inet:getifaddrs(),
    ChassisId = list_to_binary(
        proplists:get_value(hwaddr,
            proplists:get_value(GwIfName, RawIfList, []), []
        )
    ),
    {ok, SysName} = inet:gethostname(),
    State#state{
        chassis_id = ChassisId,
        sys_name = list_to_binary(SysName),
        sys_descr = list_to_binary(os:cmd("uname -a"))
    }.

scan_ifs(#state{exclude_intfs = Excludes, include_intfs = Includes} = State) ->
    {ok, RawIfList} = inet:getifaddrs(),
    IfList = lists:filter(fun
        ({IfName, _}) ->
            is_lldp_enabled(IfName, Includes, Excludes)
    end, RawIfList),

    lists:foldl(fun
        ({IfName, IfPropList}, StateAcc) ->
            enable_lldp(IfName, IfPropList, State),
            setup_socket(open, IfName, StateAcc)
    end, State, IfList).

enable_lldp(IfName, IfPropList, State) ->
    IfInfo = #lldp_entity_t{
        src_mac = inet_utils:convert_mac(to_binary, proplists:get_value(hwaddr, IfPropList, 0)),
        chassis_id = State#state.chassis_id,
        port_id = list_to_binary(IfName),
        sys_descr = State#state.sys_descr,
        sys_name = State#state.sys_name,
        if_index = get_ifindex(IfName),
        mgmt_ip = inet_utils:convert_ip(to_binary, proplists:get_value(addr, IfPropList, 0)),
        if_state = lists:member(up, proplists:get_value(flags, IfPropList, []))
    },
    lldp_handler:interface(create, IfName, IfInfo).

setup_socket(open, IfName, #state{if_map = IfMap, port_map = PortMap} = State) ->
    {ok, SockFd} = procket:open(0,
        [{protocol, procket:ntohs(?LldpEtherType)}, {type, raw}, {family, packet}]),
    ok = packet:bind(SockFd, packet:ifindex(SockFd, IfName)),
    SockPort = erlang:open_port({fd, SockFd, SockFd}, [binary, stream]),

    case maps:get(IfName, IfMap, []) of
        [] ->
            IfInfo = #lldp_netlink_intf_t{if_name = IfName, sock_fd = SockFd, sock_port = SockPort},
            State#state{
                if_map = IfMap#{IfName => IfInfo},
                port_map = PortMap#{SockPort => IfInfo}
            };
        OldIfInfo ->
            IfInfo = OldIfInfo#lldp_netlink_intf_t{sock_fd = SockFd, sock_port = SockPort},
            State#state{
                if_map = IfMap#{IfName => IfInfo},
                port_map = PortMap#{SockPort => IfInfo}
            }
    end;
setup_socket(close, IfName, #state{if_map = IfMap, port_map = PortMap} = State) ->
    case maps:get(IfName, IfMap, []) of
        [] ->
            State;
        #lldp_netlink_intf_t{
            sock_fd = SockFd, sock_port = SockPort
        } = OldIfInfo  ->
            erlang:port_close(SockPort),
            procket:close(SockFd),
            State#state{
                if_map = IfMap#{IfName => OldIfInfo#lldp_netlink_intf_t{sock_fd = -1, sock_port = -1}},
                port_map = maps:remove(SockPort, PortMap)
            }
    end.

%%%===================================================================
%%% Utility functions
%%%===================================================================
get_ifindex(IfName) ->
    {ok, Socket} = packet:socket(),
    IfIndex = packet:ifindex(Socket, IfName),
    procket:close(Socket),
    IfIndex.

is_lldp_enabled(_, [], []) ->
    true;
is_lldp_enabled(IfName, [], Excludes) ->
    not lists:member(IfName, Excludes);
is_lldp_enabled(IfName, [H|R], Excludes) when is_list(H) ->
    case is_lldp_enabled(IfName, H, Excludes) of
        true ->
            true;
        false when R /= [] ->
            is_lldp_enabled(IfName, R, Excludes);
        _ ->
            false
    end;
is_lldp_enabled(IfName, Includes, Excludes) ->
    case re:run(IfName, Includes) of
        {match, [{0, _}]} when Excludes /= [] ->
            not lists:member(IfName, Excludes);
        {match, [{0, _}]} ->
            true;
        _ ->
            false
    end.
