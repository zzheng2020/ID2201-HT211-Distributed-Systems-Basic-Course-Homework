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

init(Nodes) ->
    Clock = time:clock(Nodes),
    loop(Clock, []).

loop(Clock, HoldBackQueue) ->
    receive
        {log, From, Time, Msg} ->
            MessageQueue = lists:keysort(3, MessageList = HoldBackQueue ++ [{log, From, Time, Msg}]),
            List = time:update(From, Time, Clock),
            case time:safe(Time, List) of
                true ->
                    TempList = logInfo(Time, MessageQueue),
                    loop(List, TempList);
                false ->
                    loop(List, MessageList)
            end;
        stop ->
            ok
    end.

logInfo(_, [])->
    [];
logInfo(Time, [{log, From, MsgTime, Msg}|T]) ->
    if
        MsgTime =< Time -> log(From, MsgTime, Msg),
            logInfo(Time, T);
        true -> [{log, From, MsgTime, Msg}|T]
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).