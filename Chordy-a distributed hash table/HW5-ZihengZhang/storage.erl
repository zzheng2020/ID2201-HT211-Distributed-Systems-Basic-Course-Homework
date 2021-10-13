%%%-------------------------------------------------------------------
%%% @author zhangziheng
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 11. Otc. 2021 8:30 PM
%%%-------------------------------------------------------------------
-module(storage).
-author("zhangziheng").

%% API
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
    [].

add(Key, Value, Store) ->
    [{Key, Value} | lists:keydelete(Key, 1, Store)].

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    lists:partition(fun({Key, _}) -> key:between(Key, From, To) end, Store).

merge(Entries, Store) ->
    lists:keymerge(1, Entries, Store).