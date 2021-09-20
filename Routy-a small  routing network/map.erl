%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2021 18:12
%%%-------------------------------------------------------------------
-module(map).
-author("Ziheng Zhang").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

update(Node, Links, Map) ->
    Temp = lists:keydelete(Node, 1, Map),
    [{Node, Links} | Temp].

reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {Node, List} ->
            List;
        false ->
            []
    end.

all_nodes(Map) ->
    lists:usort(lists:concat(lists:map(fun({N,L})->[N|L] end, Map))).