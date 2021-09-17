%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 14. Sep 2021 13:38
%%%-------------------------------------------------------------------
-module(rudyPool).
-author("Ziheng Zhang").

%% API
-export([start/2, request/1]).

start(Port, N) ->
    register(rudyPool, spawn(fun() -> init(Port, N) end)).

init(Port, N) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handlerPool(Listen, N),
            stop();
        {error, Error} ->
            io:format("rudy: initialization failed: ~w~n", [Error]),
            error
    end.

stop() ->
    receive
        stop ->
            ok
    end.

handlerPool(Listen, N) ->
    case N of
        0 ->
            ok;
        N ->
            spawn(fun() -> handler(Listen, N) end),
            handlerPool(Listen, N-1)
    end.

handler(Listen, N) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            io:format("rudy ~w: received request~n", [N]),
            request(Client),
            handler(Listen, N);
        {error, Error} ->
            io:format("rudy: error1 ~w~n", [Error]),
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response),
            gen_tcp:close(Client);
        {error, Error} ->
            io:format("rudy: error2: ~w~n", [Error]),
            ok
    end.

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok("<html><head><title>Rudy</title></head><body>This is a test.<br/>" ++ URI ++ "</body></html>").
