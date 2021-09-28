%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2021 12:35
%%%-------------------------------------------------------------------
-module(vector_time).
-author("Ziheng Zhang").

%% API
-export([zero/1, inc/3, merge/2, clock/2, update/3, leq/2,safe/2]).


zero(0) ->
    [];
zero(N) ->
    [0 | zero(N - 1)].

% increment a vector in the given process index
% input like: [0,0,0,...]
% N = 1
inc(_, [], _)->
    [];
inc(Index, [H | T], X) ->
    if X == Index ->
        [H + 1 | inc(Index, T, X + 1)];
        true ->
            [H | inc(Index, T, X + 1)]
    end.

% compare two vectors
leq(Vi, Vj) ->
    Comparison = lists:zipwith(fun(X, Y) -> (Y >= X) end, Vi, Vj),
    lists:all(fun(E) -> E == true end, Comparison).

% return the "max" of two vectors
merge(Vi, Vj) ->
    Res = leq(Vi, Vj),
    if Res -> Vj;
        true -> Vi
    end.



% input: a list of processes, N: number of processes
% return: [{john,[0,0,0,0]}, {paul,[0,0,0,0]}...]
clock([], _)->
    [];
clock([H | T], ZeroVec) ->
    % ZeroVec = zero(N),
    [{H, ZeroVec} | clock(T, ZeroVec)].


update(Node, TimeVector, Clock) ->
    lists:keyreplace(Node, 1, Clock, {Node, TimeVector}).


safe(_, []) ->
    true;
safe(TimeVector, [{_, MsgVector}|T]) ->
    Res = leq(MsgVector, TimeVector),
    if
        Res -> safe(TimeVector, T);
        true -> false
    end.
