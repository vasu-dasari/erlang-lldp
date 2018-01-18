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
-export([pretty_print/1]).

pretty_print(Item) ->
    io_lib:format("~s",[io_lib_pretty:print(Item)]).
