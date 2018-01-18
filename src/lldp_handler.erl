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

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).

-define(SERVER, ?MODULE).

-record(state, {
    module,
    state
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Cfg) ->
    gen_server:start_link(?MODULE, [Name, Cfg], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([CallbackModule, CallbackState]) ->
    ?INFO("Starting ~s with ~p, ~p, pid ~p", [?MODULE_STRING, CallbackModule, CallbackState, self()]),
    self() ! {init},
    {ok, #state{
        module = CallbackModule,
        state = CallbackState
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

process_cast(Request, State) ->
    ?INFO("cast: Request~n~p", [Request]),
    {noreply, State}.

process_info_msg({init}, State) ->
    {noreply, State};

process_info_msg(Request, State) ->
    ?INFO("info: Request~n~p", [Request]),
    {noreply, State}.
