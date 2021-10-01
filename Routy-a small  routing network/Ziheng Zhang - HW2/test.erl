%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2021 1:05
%%%-------------------------------------------------------------------
-module(test).
-author("Ziheng Zhang").

%% API
-export([main/0, broadcast/0, update/0, stopAll/0]).

% erl -name ziheng@zhang -setcookie routy -connect_all false

main() ->
    routy:start(r1, test1),
    routy:start(r2, test2),
    routy:start(r3, test3),
    r1 ! {add, test2, {r2, 'ziheng@zhang'}},
    r2 ! {add, test3, {r3, 'ziheng@zhang'}},
    r3 ! {add, test1, {r1, 'ziheng@zhang'}},
    true.

broadcast() ->
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    broadcasttrue.

update() ->
    r1 ! update,
    r2 ! update,
    r3 ! update,
    updatetrue.

stopAll() ->
    routy:stop(r1),
    routy:stop(r2),
    routy:stop(r3),
    stoptrue.
