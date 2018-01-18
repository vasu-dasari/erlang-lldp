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

-include("logger.hrl").

%% API
-export([]).

-export([init/0, handle_message/2, terminate/2]).

-record(state, {}).

init() ->
    ?INFO("Starting netlink process", []),
    {ok, #state{}}.

handle_message(Message, State) ->
    ?INFO("Message: ~p", [Message]),
    {ok, State}.

terminate(Reason, State) ->
    ?INFO("Terminate: ~p, State ~p", [Reason, State]),
    ok.


