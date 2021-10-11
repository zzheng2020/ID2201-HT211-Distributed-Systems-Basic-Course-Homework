%%%-------------------------------------------------------------------
%%% @author zhangziheng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 10月 2021 下午6:02
%%%-------------------------------------------------------------------
-module(t_node).
-author("zhangziheng").

%% API
-export([node/3,start/1,start/2]).
-define(Stabilize,1000).
-define(Timeout,1000).

%we are the first node
start(Id)->
    start(Id,nil).

%we are connecting to an existing ring
start(Id,Peer)->
    timer:start(),
    spawn(fun()->init(Id,Peer) end).

%init function
%to stabilize the node
init(Id,Peer)->
    Predecessor = nil,
    {ok,Successor} = connect(Id,Peer),
    schedule_stabilize(),
    node(Id,Predecessor,Successor).

%myself is the successor
connect(Id,nil)->
    {ok,{Id,self()}};
%find the successor
connect(_Id,Peer)->
    Qref = make_ref(),
    %ask peer for the key
    Peer!{key,Qref,self()},
    receive
    %get the key
        {Qref,Skey}->
            {ok,{Skey,Peer}}
    after ?Timeout->
        io:format("Time out:no response~n",[])
    end.



%node function for listening
node(Id, Predecessor,Successor)->
    receive
    %a peer needs to know the key
        {key, Qref, Peer}->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
    %a new node informs us the existence
        {notify, New}->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
    %a predecessor needs to know our predecessor
        {request, Peer}->
            request(Peer,Predecessor),
            node(Id,Predecessor,Successor);
    %our successor informs us about its predecessor
        {status,Pred}->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor,Succ);
    %create a probe
        probe->
            create_probe(Id,Successor),
            node(Id,Predecessor,Successor);
    %we know that we send the probe since Id is equal
        {probe,Id,Nodes,T}->
            remove_probe(T,Nodes),
            node(Id,Predecessor,Successor);
    %pass the probe to next node
        {probe,Ref,Nodes,T}->
            forward_probe(Ref,T,Nodes,Id,Successor),
            node(Id,Predecessor,Successor);

    %to stabilize
    %use when a new node is added
        stabilize->
            stabilize(Successor),
            node(Id,Predecessor,Successor)
    end.

%send a request message to myself
%request for stabilizing
stabilize({_,Spid})->
    Spid!{request,self()}.


%Pred is the successor's current predecessor
stabilize(Pred, Id, Successor)->
    {Skey,Spid} = Successor,
    case Pred of
        %the successor do not know about us
        %notify it about us
        nil ->
            Spid!{notify,{Id,self()}},
            Successor;
        %point to ouselves
        %do nothing
        {Id,_}->
            Successor;
        %point to itself
        %notify it about us
        {Skey,_}->
            Spid!{notify,{Id,self()}},
            Successor;
        %point to another node
        {Xkey,Xpid}->
            case key:between(Xkey,Id,Skey) of
                %we are before the predecessor of the target node
                %adopt the successor and run this function again
                true->
                    self() ! stabilize,
                    Xpid ! {request, self()},
                    %stabilize(Pred, Id, {Xkey,Xpid}),
                    {Xkey,Xpid};
                %we are the predecessor of the target node than its original predecessor
                false->
                    Spid!{notify,{Id,self()}},
                    Successor
            end
    end.

%set a time period
schedule_stabilize()->
    timer:send_interval(?Stabilize,self(),stabilize).


%Peer wants to know our predecessor
request(Peer,Predecessor)->
    case Predecessor of
        nil->
            %send to peer that out predecessor is nil
            Peer ! {status,nil};
        {Pkey,Ppid}->
            %send to peer our predecessor
            Peer!{status,{Pkey,Ppid}}
    end.

%get the information of a possible new predecessor
%need to check by myself
notify({Nkey,Npid},Id,Predecessor)->
    case Predecessor of
        nil->
            Npid!{status, {Nkey,Npid}},
            {Nkey,Npid};
        {Pkey,_}->
            case key:between(Nkey,Pkey,Id) of
                true->
                    Npid!{status, {Nkey,Npid}},
                    {Nkey,Npid};
                false->
                    Npid!{status, Predecessor},
                    Predecessor
            end
    end.

%create a probe with time stamp
create_probe(Id,Successor)->
    {_,Pid} = Successor,
    CurrentT = erlang:now(),
    %io:format("create T:~w~n",[CurrentT]),
    Pid!{probe,Id,[Id],CurrentT}.

%the probe which is created by us return to us
remove_probe(T,Nodes)->
    %timer:sleep(1000),
    ArrivedT = erlang:now(),
    %io:format("remove T:~w~n",[ArrivedT]),
    Diff = timer:now_diff(ArrivedT, T),
    io:format("The nodes are ~w.~n The time to pass around the ring is ~w.~n",[Nodes,Diff]).

%pass the probe to next node
forward_probe(Ref,T,Nodes,Id,Successor)->
    {_,Pid} = Successor,
    Pid!{probe,Ref,[Id|Nodes],T}.
