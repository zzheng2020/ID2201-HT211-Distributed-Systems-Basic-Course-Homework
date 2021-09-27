%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2021 16:21
%%%-------------------------------------------------------------------
-module(mylogger).
-author("Ziheng Zhang").

%% API
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(_) ->
    loop().

loop() ->
    receive
        {log, From, Time, Msg} ->
            log(From, Time, Msg),
            loop();
        stop ->
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).