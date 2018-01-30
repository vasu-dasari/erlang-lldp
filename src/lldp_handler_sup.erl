%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 12:13 PM
%%%-------------------------------------------------------------------
-module(lldp_handler_sup).
-author("vdasari").

-behaviour(supervisor).

-include("logger.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, start_child/3, stop_child/1]).

-define(SERVER, ?MODULE).

-define(Process(Name, Type),
    {Name, {Name, start_link, []}, temporary, 2000, Type, [Name]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [
        ?Process(lldp_handler, worker)
    ]}}.

start_child(ProcName, CallbackModName, CallbackState) ->
    case whereis(ProcName) of
        undefined ->
            do_add_child(ProcName, CallbackModName, CallbackState);
        Pid ->
            do_delete_child(Pid),
            do_add_child(ProcName, CallbackModName, CallbackState)
    end.

stop_child(Pid) when is_pid(Pid) ->
    do_delete_child(Pid).

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_add_child(ProcName, CallbackModName, CallbackState) ->
    case supervisor:start_child(?SERVER, [ProcName, CallbackModName, CallbackState]) of
        {ok, Pid} = Ret -> erlang:register(ProcName, Pid), Ret;
        Ret -> Ret
    end.

do_delete_child(Pid) ->
    gen_server:stop(Pid),
    ok = supervisor:terminate_child(?SERVER, Pid).
