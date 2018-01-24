%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 10:39 AM
%%%-------------------------------------------------------------------
-module(lldp_utils).
-author("vdasari").

%% API
-export([pretty_print/1, htons/1, ntohs/1, get_timestamp/0]).

pretty_print(Item) ->
    io_lib:format("~s",[io_lib_pretty:print(Item)]).

htons(Uint16) ->
    <<Y:16/unsigned-big-integer>> = binary:encode_unsigned(Uint16),
    Y.

ntohs(Uint16) ->
    <<Y:16/unsigned-native-integer>> = binary:encode_unsigned(Uint16),
    Y.

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).