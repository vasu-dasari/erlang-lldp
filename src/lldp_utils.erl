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

-include("lldp_api.hrl").

%% API
-export([pretty_print/1, record_to_proplist/1, record_to_proplist/2]).

pretty_print(Item) ->
    io_lib:format("~s",[io_lib_pretty:print(Item)]).

record_to_proplist(to_str, R) ->
    pretty_print(record_to_proplist(R)).

-define(R2P(Record),
    record_to_proplist(#Record{} = Rec) ->
        List = [record_to_proplist(R) || R <- tuple_to_list(Rec)],
        ElemList = [{record, Record}] ++ lists:zip(record_info(fields, Record), tl(List)),
        PropList = [{K,V} || {K,V} <- ElemList, (V /= undefined) andalso (V /= []) andalso (V /= <<>>)],
        case PropList of
            [{record, _}] ->
                [];
            _ ->
                PropList
        end
).

record_to_proplist({}) -> [];
?R2P(lldp_entity_t);
record_to_proplist(List) when is_list(List) ->
    lists:foldr(fun
        (Entry, Acc) ->
            [record_to_proplist(Entry) | Acc]
    end, [], List);
record_to_proplist(Rec) -> Rec.
