%%%-------------------------------------------------------------------
%%% @author zhangziheng
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct. 2021 12:34 AM
%%%-------------------------------------------------------------------
-module(node1).
-author("zhangziheng").

%% API
-export([start/1, start/2, node/3]).
-define(Stabilize, 1000).
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
    node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
    receive
        %% a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);

        %% a new node informs us of its existence
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);

        %% a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);

        %% our successor informs us about its predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);

        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);

        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);

        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);

        %% When a new node is added, we need to stabilize.
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor)
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

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            Npid ! {status, {Nkey, Npid}},
            {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Npid ! {status, {Nkey, Npid}},
                    {Nkey, Npid};
                false ->
                    Npid ! {status, Predecessor},
                    Predecessor
            end
    end.

create_probe(Id, Successor) ->
    {_,Pid} = Successor,
    CurrentT = erlang:now(),
    Pid ! {probe, Id, [Id], CurrentT}.

remove_probe(T, Nodes) ->

    ArrivedT = erlang:now(),
    Diff = timer:now_diff(ArrivedT, T),
    io:format("The nodes are ~w.~n The time to pass around the ring is ~w.~n",[Nodes,Diff]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
    {_, Pid} = Successor,
    Pid ! {probe, Ref, [Id | Nodes], T}.