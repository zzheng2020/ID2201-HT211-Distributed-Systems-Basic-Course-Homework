%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2021 13:34
%%%-------------------------------------------------------------------
-module(vector_logger).
-author("Ziheng Zhang").

%% API
-export([start/1, stop/1, queue_push_back/3]).

% give a list of nodes that will send it messages
% here just ignore the nodes
start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).


stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = vector_time:clock(Nodes, vector_time:zero(length(Nodes))),
    loop(Clock, []).

queue_push_back([], Request, Flag)->
    if Flag -> [];
        true -> [Request]
    end;
queue_push_back([H | T], Request, Flag) ->
    case Request of {_, _, InsertTime, _} ->
        case H of {_, _, Time, _} ->
            Res = vector_time:leq(InsertTime, Time),
            if Res and (Flag == false) ->
                [Request, H | queue_push_back(T, Request, true)];
                true -> [H | queue_push_back(T, Request, Flag)]
            end
        end
    end.


% HoldBackQueue
% [{log, john, [0,2,1,2], 33},...]
loop(Clock, HoldBackQueue) ->
    receive
        {log, From, Time, Msg} ->
            List = vector_time:update(From, Time, Clock),

            MessageList = queue_push_back(HoldBackQueue, {log, From, Time, Msg}, false),
%%            io:format("============vector_logger==============~n"),
%%            io:format("~p~n", [From]),
%%            List = vector_time:update(From, Time, Clock),
%%            MessageList = [{log, From, Time, Msg} | HoldBackQueue],
%%            MessageList = [{log, From, Time, Msg} | HoldBackQueue],
%%            io:format("~p~n", [MessageList]),
%%            io:format("============vector_logger==============~n"),
%%            io:format("~p~n", [List]),

            Length = list_length(MessageList),
%%            io:format("~w~n", [Length]),
            if Length > 1 ->
                case vector_time:safe(Time, List) of
                    true ->
%%                        io:format("Time: ~p~n", [Time]),
    %%                    io:format("List: ~p~n", [List]),
                        TempList = logInfo(Time, MessageList),
                        loop(List, TempList);
                    false ->
                        loop(List, MessageList)
                end;
                true -> loop(List, MessageList)
            end;
    % log(From, Time, Msg),
    % loop();
        stop ->
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w~p~w~n", [From, Msg, Time]).


logInfo(_, [])->
    [];
logInfo(Time, [{log, From, MsgTime, Msg} | T]) ->
    Compare = vector_time:leq2(MsgTime, Time),
%%    io:format("MsgTime: ~p~n", [MsgTime]),
%%    io:format("Time: ~p~n", [Time]),
    if
        Compare == true ->
            log(From, MsgTime, Msg),
%%            MsgTime =< Time -> log(From, MsgTime, Msg),
            logInfo(Time, T);
        true -> [{log, From, MsgTime, Msg} | T]
    end.

list_length([]) ->
    0;
list_length([H | T]) ->
    1 + list_length(T).