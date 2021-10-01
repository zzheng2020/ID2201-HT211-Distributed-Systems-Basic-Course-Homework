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

%% return an initial Lamport value
zero() ->
    0.

%% return the time T incremented by one
inc(Name, T) ->
    T + 1.

%% merge the two Lamport time stamps
merge(Ti, Tj) ->
    if
        Ti < Tj -> Tj;
    true -> Ti
    end.

%%  true if Ti is less than or equal to Tj
leq(Ti, Tj) ->
    if
        Ti > Tj -> false;
        true -> true
    end.

%% return a clock that can keep track of the nodes
clock([]) ->
    [];
clock([H | T]) ->
    [{H, 0} | clock(T)].

%% return a clock that has been updated
%% given that we have received a log message
%% from a node at a given time
update(Node, Time, Clock) ->
    lists:keyreplace(Node, 1, Clock, {Node, Time}).

%% is it safe to log an event that happened at
%% a given time, true or false
safe(_, []) ->
    true;
safe(Time, [{_, MsgTime} | T]) ->
    if
        Time =< MsgTime -> safe(Time, T);
        true -> false
    end.