%%%-------------------------------------------------------------------
%%% @author zhangziheng
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct. 2021 8:20 PM
%%%-------------------------------------------------------------------
-module(node2).
-author("zhangziheng").

%% API
-export([]).
-export([start/1, start/2, node/4]).
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
    node(Id, Predecessor, Successor, Store).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n", [])
    end.

%% key, predecessor, successor

node(Id, Predecessor, Successor, Store) ->
    receive
    %% a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);

    %% a new node informs us of its existence
        {notify, New} ->
            {Pred, S} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, S);

    %% a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);

    %% our successor informs us about its predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);

        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);

        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);

        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);

    %% When a new node is added, we need to stabilize.
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);

        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
                Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);

        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);

        %% When a new node wants to join the ring,
        %% we update the storage.
        {handover, Elements}->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged)
    end.

%% send a request message to its successor.
stabilize({_, Spid}) ->
    Spid ! {request, self()}.

%% Pred: ours successors current predecessor.
stabilize(Pred, Id, Successor) ->
    %% Skey: Successor's key
    %% Spid: Successor's Pid
    {Skey, Spid} = Successor,
    case Pred of

        %% If the predecessor of node Id's successor is nil,
        %% then notify Id's successor.
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;

        %% If the predecessor of node Id's successor points to us,
        %% then do nothing.
        {Id, _} ->
            Successor;

        %% If the predecessor of node Id's successor points to itself,
        %% then notify Id's successor.
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Successor;

        %% If the predecessor of node Id's successor points to another node(X_node),
        %% then we need to categorize the discussion.
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of

                %% If X_node is between Id and Skey,
                %% then node Id is before the X_node,
                %% therefore, we need to adopt X_node as Id's successor,
                %% and run stabilization again.
                true ->
                    self() ! stabilize,
                    Xpid ! {request, self()},
                    {Xkey, Xpid};

                %% If X_node is not between Id and Skey,
                %% then it means Id is the predecessor of Skey,
                %% therefor, we notify the successor.
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
            end

    end.

%% The stabilize procedure must be done with regular intervals.
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            %% update the storage
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    %% update the storage
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                %% new node is before the predecessor
                false ->
                    %% Npid!{status, Predecessor},
                    {Predecessor, Store}
            end
    end.

%% split the storage
%% one part is kept, another part is passed to the new node
handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

create_probe(Id, Successor) ->
    {_,Pid} = Successor,
    CurrentT = erlang:now(),
    Pid ! {probe, Id, [Id], CurrentT}.

remove_probe(T, Nodes) ->

    ArrivedT = erlang:now(),
    Diff = timer:now_diff(ArrivedT, T),
    io:format("Node(s): ~w.~n Time (per round): ~w.~n",[Nodes, Diff]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
    {_, Pid} = Successor,
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