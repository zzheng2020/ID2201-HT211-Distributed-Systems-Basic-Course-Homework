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
-export([start/2, stop/1,status/1]).


start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = interfaces:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    receive

        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);

        {add, Node, Pid} ->
            Ref = erlang:monitor(process, Pid),
            Intf1 = interfaces:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {remove, Node} ->
%%            {ok, Ref} = interfaces:ref(Node, Intf),
%%            erlang:demonitor(Ref),
%%            Intf1 = interfaces:remove(Node, Intf),
%%            router(Name, N, Hist, Intf1, Table, Map);
            case interfaces:ref(Node, Intf) of
                {ok, Ref} ->
                    erlang:demonitor(Ref),
                    Intf1 = interfaces:remove(Node, Intf),
                    router(Name, N, Hist, Intf1, Table, Map);
                notfound ->
                    io:format("Error! You are trying to remove an unexisted node!")
            end,
            router(Name, N, Hist, Intf, Table, Map);


        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = interfaces:name(Ref, Intf),
            io:format("~w: exit received from ~w~n", [Name, Down]),
            Intf1 = interfaces:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {links, Node, X,Links} ->
            case hist:update(Node, X, Hist) of
                {new, Hist1} ->
                    interfaces:broadcast({links, Node, X, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;

        {status, From} ->
            io:format("STATUS messages:"),
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);

        update ->
            Table1 = dijkstra:table(interfaces:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);

        broadcast ->
            Message = {links, Name, N, interfaces:list(Intf)},
            interfaces:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);

    % name is matched, message arrived.
        {route, Name, From, Message} ->
            io:format("~w: received message (~s) from ~w~n", [Name, Message, From]),
            router(Name, N, Hist, Intf, Table, Map);

    % name is not matched, need to find next hop.
        {route, To, From, Message} ->
            io:format("~w: routing message (~s) from ~w to ~w~n", [Name, Message, From, To]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case interfaces:lookup(Gw, Intf) of
                        notfound ->
                            io:format("~w: interface for gw ~w not found ~n", [Name, Gw]);
                        {ok, Pid} ->
                            io:format("~w: forward to ~w~n", [Name, Gw]),
                            Pid ! {route, To, From, Message}
                    end;
                notfound ->
                    io:format("~w: routing entry for ~w not found ~n", [Name, To])
            end,
            router(Name, N, Hist, Intf, Table, Map);

        stop ->
            ok
    end.

status(Router) ->
    Router ! {status, self()},
    receive
        {status, {Name, N, Msg, Intf, Table, Map}}->
            io:format("Status is shown below:~n"),
            io:format("name: ~w~n", [Name]),
            io:format("n: ~w~n", [N]),
            io:format("msgs: ~w~n", [Msg]),
            io:format("intf: ~w~n", [Intf]),
            io:format("table: ~w~n", [Table]),
            io:format("map: ~w~n", [Map]),
            ok
    end.