%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2021 22:24
%%%-------------------------------------------------------------------
-module(time).
-author("Ziheng Zhang").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(Name, T) ->
    T + 1.

merge(Ti, Tj) ->
    if
        Ti < Tj -> Tj;
    true -> Ti
    end.

leq(Ti, Tj) ->
    if
        Ti > Tj -> false;
        true -> true
    end.

clock([]) ->
    [];
clock([H | T]) ->
    [{H, 0} | clock(T)].

update(Node, Time, Clock) ->
    lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(_, []) ->
    true;
safe(Time, [{_, MsgTime} | T]) ->
    if
        Time =< MsgTime -> safe(Time, T);
        true -> false
    end.