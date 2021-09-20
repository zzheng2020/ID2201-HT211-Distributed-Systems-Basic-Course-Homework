%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2021 21:12
%%%-------------------------------------------------------------------

-module(routy).
-author("Ziheng Zhang").

%% API
-export([start/2, stop/1, get_status/1]).


start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

init(Name) ->
    Intf = interfaces:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    receive
        {add, Node, Pid} ->
            io:format("~w> Received add signal: ~w ~w ~n", [Name, Node, Pid]),
            Ref = erlang:monitor(process, Pid),
            Intf1 = interfaces:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {remove, Node} ->
            {ok, Ref} = interfaces:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = interfaces:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = interfaces:name(Ref, Intf),
            Start1 = erlang:system_time(),
            io:format("end printing: ~w~n", [Start1]),
            io:format("~w: exit received from ~w~n", [Name, Down]),
            Intf1 = interfaces:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {status, From} ->
            %io:fwrite("~w:called", From),
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            %io:format("~w: exit received from ~w~n", [Name, N,Hist,Intf,Table,Map]),
            router(Name, N, Hist, Intf, Table, Map);

        {links, Node, R, Links} ->
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    interfaces:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;

        update ->
            Table1 = dijkstra:table(interfaces:list(Intf), Map),
            io:format(" routing message: ~w ~n", [Table1]),
            router(Name, N, Hist, Intf, Table1, Map);

        broadcast ->
            Message = {links, Name, N, interfaces:list(Intf)},
            %io:format("~w> broadcast: ~w ~w ~n", [Message]),
            io:fwrite("Reached~n"),
            interfaces:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);

        {route, Name, _, Message} ->
            io:format("~w: received message ~w ~n", [Name, Message]),
            router(Name, N, Hist, Intf, Table, Map);

        {route, To, From, Message} ->
            io:format("~w: routing message (~w)", [Name, Message]),

            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case interfaces:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            io:format("~w: Found pid (~w)", [Name, Pid]),
                            Pid ! {route, To, From, Message};
                        notfound ->
                            ok
                    end;
                notfound ->
                    ok
            end,
            router(Name, N, Hist, Intf, Table, Map);

        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);



        stop ->
            io:format("~w> Received stop signal.~n", [Name]),
            ok


    end.


stop(Node)->
    Node! stop,
    unregister(Node),
    Start = erlang:system_time(),
    io:format("start printing: ~w~n", [Start]).



get_status(Pid) ->
    Pid ! {status, self()},
    receive
        {status, {Name, N, Hist, Intf, Table, Map}} ->
            io:format(" routing message: ~w ~w ~w ~w ~w ~w ~n", [Name, N, Hist, Intf, Table, Map])
    end.