%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 9:56 AM
%%%-------------------------------------------------------------------
-module(lldp_manager).
-author("vdasari").

-behaviour(gen_server).

-include("logger.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([info/0, info/1, info/2, info/3]).

-define(SERVER, ?MODULE).

-define(APPNAME, lldp).

-define(call(A),gen_server:call(?MODULE, A)).
-define(cast(A),gen_server:cast(?MODULE, A)).

-record(state, {
    handler_map = #{}
}).

%%%===================================================================
%%% API
%%%===================================================================
info() ->
    io:format("~s~n", [?call(info)]).

info(Name) ->
    io:format("~s~n", [?call({info, Name})]).

info(Name, IfName) ->
    io:format("~s~n", [?call({info, Name, IfName})]).

info(Name, IfName, Option) ->
    io:format("~s~n", [?call({info, Name, IfName, Option})]).

load_config() ->
    case application:get_env(?APPNAME, netlink) of
        {ok,NetlinkConfig} ->
            start_netlink(NetlinkConfig);
        _ ->
            ok
    end,
    case application:get_env(?APPNAME, lldp_handler) of
        {ok, Handlers} when Handlers /= none ->
            start_handler(Handlers);
        _ ->
            ok
    end,
    ok.

start_netlink(Config) ->
    ?cast({start_netlink, Config}).

start_handler(Handlers) ->
    ?cast({start_handler, Handlers}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    self() ! {init},
    {ok, #state{}}.

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
process_call(info, #state{handler_map = Map} = State) ->
    Header =
        io_lib:format("~16c ~16c ~4c ~4c~n", [$-, $-, $-, $-]) ++
        io_lib:format("~16s ~16s ~4s ~4s~n", ["Name", "Module", "Ifs", "Nbrs"]) ++
        io_lib:format("~16c ~16c ~4c ~4c~n", [$-, $-, $-, $-]),

    Return = maps:fold(fun
        (K, Pid, Acc) ->
            lldp_handler:info(Pid, {info, brief}) ++ Acc
    end, [], Map),
    {reply, lists:flatten(Header ++ Return), State};

process_call(Request, #state{handler_map = Map} = State) when element(1, Request) == info ->
    HandlerName = element(2, Request),
    #{HandlerName := Pid} = Map,
    {reply, lldp_handler:info(Pid, Request), State};
process_call(Request, State) ->
    ?INFO("call: Unhandled Request ~p", [Request]),
    {reply, ok, State}.

process_cast({start_netlink, Config}, #state{handler_map = Map} = State) ->
    case lldp_netlink:init(Config) of
        {ok,ChildState} ->
            {ok, Pid} = lldp_handler_sup:start_child(lldp_netlink, ChildState),
            true = erlang:link(Pid),
            {noreply, State#state{
                handler_map = Map#{lldp_netlink => Pid}
            }};
        R ->
            ?ERROR("Could not start lldp_netlink process", [R]),
            {noreply, State}
    end;
process_cast(Request, State) ->
    ?INFO("cast: Request~n~p", [Request]),
    {noreply, State}.

process_info_msg({init}, State) ->
    qdate:set_timezone(os:cmd("date +%Z")),
    qdate:set_timezone(os:cmd("EST")),
    load_config(),
    {noreply, State};

process_info_msg(Request, State) ->
    ?INFO("info: Request~n~p", [Request]),
    {noreply, State}.

%%%===================================================================
%%% Worker functions
%%%===================================================================