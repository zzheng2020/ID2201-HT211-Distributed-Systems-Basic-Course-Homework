%%%-------------------------------------------------------------------
%%% @author zhangziheng
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 13. Otc 2021 3:43 PM
%%%-------------------------------------------------------------------
-module(node3).
-author("zhangziheng").

%% API
-export([start/1, start/2, node/5]).
-define(Stabilize, 500).
-define(Timeout, 1000).

%% first node
start(Id) ->
    start(Id, nil).

%% add node to an existing ring
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    Store = storage:create(),
    node(Id, Predecessor, Successor, nil, Store).

connect(Id, nil) ->
    {ok, {Id, nil, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Ref = monitor(Peer),
            {ok, {Skey, Ref, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n", [])
    end.

%% key, predecessor, successor

node(Id, Predecessor, Successor, Next, Store) ->
    receive
    %% a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Next, Store);

    %% a new node informs us of its existence
        {notify, New} ->
            {Pred, S} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Next, S);

    %% a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Next, Store);

    %% our successor informs us about its predecessor
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Nxt, Store);

        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Next, Store);

        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Next, Store);

        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Next, Store);

    %% When a new node is added, we need to stabilize.
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Next, Store);

        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
                Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Added);

        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Store);

    %% When a new node wants to join the ring,
    %% we update the storage.
        {handover, Elements}->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Next, Merged);

        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Nxt, Store)
    end.

down(Ref, {_, Ref, _}, Successor, Next) ->
    io:format("predecessor die:~w~n", [Ref]),
    {nil, Successor,Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
    io:format("successor die:~w~n",[Ref]),
    self()! stabilize,
    %the next node becomes the successor
    Nref = monitor(Npid),
    {Predecessor, {Nkey, Nref, Npid}, nil}.

%% send a request message to its successor.
stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

%% Pred: ours successors current predecessor.
stabilize(Pred, Next, Id, Successor) ->
    %% Skey: Successor's key
    %% Spid: Successor's Pid
    {Skey, Sref, Spid} = Successor,
    case Pred of

        %% If the predecessor of node Id's successor is nil,
        %% then notify Id's successor.
        nil ->
            Spid ! {notify, {Id, self()}},
            {Successor, Next};

        %% If the predecessor of node Id's successor points to us,
        %% then do nothing.
        {Id, _} ->
            {Successor, Next};

        %% If the predecessor of node Id's successor points to itself,
        %% then notify Id's successor.
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            {Successor, Next};

        %% If the predecessor of node Id's successor points to another node(X_node),
        %% then we need to categorize the discussion.
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of

                %% If X_node is between Id and Skey,
                %% then node Id is before the X_node,
                %% therefore, we need to adopt X_node as Id's successor,
                %% and run stabilization again.
                true ->
                    Xpid ! {notify, {Id, self()}},
                    drop(Sref),


                    %monitor the predecessor as the successor
                    XNref = monitor(Xpid),
                    %Xpid ! {request, self()},
                    self() ! stabilize,
                    %stabilize(Pred, Next, Id, {Xkey,Xref,Xpid}),
                    %predecessor is the successor
                    %successor is the next
                    {{Xkey, XNref, Xpid},Successor};

                %% If X_node is not between Id and Skey,
                %% then it means Id is the predecessor of Skey,
                %% therefor, we notify the successor.
                false ->
                    Spid ! {notify, {Id, self()}},
                    {Successor, Next}
            end

    end.

%% The stabilize procedure must be done with regular intervals.
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor, Next) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, Next};
        {Pkey, _, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, Next}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            %% update the storage
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            %return new pred and storage
            {{Nkey,Nref,Npid},Keep};
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    %update the storage
                    Keep = handover(Id,Store,Nkey,Npid),
                    drop(Pref),
                    Nref = monitor(Npid),
                    {{Nkey,Nref,Npid},Keep};
                %new node is before the predecessor
                false->
                    %Npid!{status, Predecessor},
                    {Predecessor,Store}
            end
    end.

%% split the storage
%% one part is kept, another part is passed to the new node
handover(Id, Store, Nkey, Npid) ->
%%    {Keep, Rest} = storage:split(Id, Nkey, Store),
    {Keep, Rest} = storage:split(Nkey, Id, Store),
    Npid ! {handover, Rest},
    Keep.

create_probe(Id, Successor) ->
    {_, _, Pid} = Successor,
    CurrentT = erlang:now(),
    Pid ! {probe, Id, [Id], CurrentT}.

remove_probe(T, Nodes) ->

    ArrivedT = erlang:now(),
    Diff = timer:now_diff(ArrivedT, T),
    io:format("Node(s): ~w.~n Time (per round): ~w.~n",[Nodes, Diff]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
    {_, _, Pid} = Successor,
    Pid ! {probe, Ref, [Id | Nodes], T}.

%% check if we should take care of the key (predecessor, us]
%% if we should, then add the new key value
%% if we should not, give the key to our successor
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

%% check if we are responsible for the key
%% if so, we lookup in the store and send the reply
%% if not, we forward the request
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

%monitor the process
monitor(Pid) ->
    erlang:monitor(process, Pid).

%drop the process monitor
drop(nil) ->
    ok;
drop(Pid) ->
    erlang:demonitor(Pid, [flush]).
