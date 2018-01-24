%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 9:55 AM
%%%-------------------------------------------------------------------
-module(gen_lldp).
-author("vdasari").

-include("lldp_api.hrl").

-callback init(
        Config :: list()
) ->
    {'ok', State::term()} | {'error', Reason::term()}.

-callback handle_message(
        Request :: term(),
        State :: term()) ->
    {'ok', State::term()} | {'error', Reason::term()}.

-callback notify(
        Op :: (add | update | delete),
        IfName :: term(),
        EntityInfo :: #lldp_entity_t{},
        State :: term()) ->
    {'ok', State::term()} | {'error', Reason::term()}.

-callback info(
        Request :: term(),
        State :: term()) ->
    {'ok', State::term()} | {'error', Reason::term()}.

-callback terminate(
        Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: term()) ->
    term().
