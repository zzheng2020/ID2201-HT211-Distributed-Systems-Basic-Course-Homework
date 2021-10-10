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
-export([]).

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
    end.

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
        {SKey, _} ->
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
