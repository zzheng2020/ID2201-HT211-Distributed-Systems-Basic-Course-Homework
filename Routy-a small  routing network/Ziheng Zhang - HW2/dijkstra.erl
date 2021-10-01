%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2021 22:51
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("Ziheng Zhang").

%% API
-export([route/2, table/2]).

%% returns the length of the shortest path to the
%% node or 0 if the node is not found.
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {_, Length_of_hops, _} ->
            Length_of_hops;
        false ->
            0
    end.

%% replaces the entry for Node in Sorted with a
%% new entry having a new length N and Gateway.
%% The resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
    TempList = lists:keydelete(Node, 1, Sorted),
    [{Node, N, Gateway} | TempList].

%% update the list Sorted given the information
%% that Node can be reached in N hops using Gateway.
%% If no entry is found then no new entry is added.
%% Only if we have a better (shorter) path should we
%% replace the existing entry.
update(Node, N, Gateway, Sorted) ->
    Length_of_hops = entry(Node, Sorted),
    if
        N >= Length_of_hops ->
            Sorted;
        true ->
            replace(Node, N, Gateway, Sorted)
    end.

%% construct a table given a sorted list of nodes,
%% a map and a table constructed so far.
iterate(Sorted, Map, Table) ->
%%    io:format("Sorted: ~w~n", [Sorted]),
%%    io:format("Map: ~w~n", [Map]),
%%    io:format("Table: ~w~n", [Table]),
    case Sorted of
        [] ->
            Table;
        [{_, inf, _} | _] ->
            Table;
        [Entry | Tail] ->
%%            io:format("Entry: ~w~n", [Entry]),
%%            io:format("Tail: ~w~n", [Tail]),
            {Node, Length, Gateway} = Entry,

            case lists:keyfind(Node, 1, Map) of
                {_, ToCity} ->
%%                    io:format("ToCity: ~w~n", [ToCity]),
                    NewSorted = lists:foldl(fun(City, TmpSorted) -> update(City, Length + 1, Gateway, TmpSorted) end,
                                              Tail, ToCity);
                false ->
%%                    io:format("false~n"),
                    NewSorted = Tail
            end,

%%            io:format("NewList: ~w~n", [NewSorted]),
%%            io:format("===========================~n"),
            iterate(NewSorted, Map, [{Node, Gateway} | Table])
    end.

%% search the routing table and return the gateway
%% suitable to route messages to a node. If a gateway
%% is found we should return {ok, Gateway}
%% otherwise we return notfound.
route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        {_, Gateway} ->
            {ok, Gateway};
        false ->
            notfound
    end.
%% construct a routing table given the gateways
%% and a map
table(Gateways, Map) ->
%%    io:format("Gateways: ~w~n", [Gateways]),
%%    io:format("Map: ~w~n", [Map]),
    All_Nodes = map:all_nodes(Map),
%%    io:format("All Nodes: ~w~n", [All_Nodes]),
    Dummy = lists:map(fun(Node) ->
            case lists:member(Node, Gateways) of
                true ->
                    {Node, 0, Node};
                false ->
                    {Node, inf, unknown}
            end
                      end, All_Nodes),

    Sorted = lists:keysort(2, Dummy),
%%    io:format("~w~n", [Sorted]),
    iterate(Sorted, Map, []).