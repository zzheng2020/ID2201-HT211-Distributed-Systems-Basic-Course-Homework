%a local storage
-module(node2).
-export([node/4,start/1,start/2]).
-define(Stabilize,500).
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
    S = storage:create(),
    node(Id,Predecessor,Successor,S).

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
node(Id, Predecessor,Successor,Store)->
    receive
    %a peer needs to know the key
        {key, Qref, Peer}->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor,Store);
    %a new node informs us the existence
        {notify, New}->
            {Pred,S} = notify(New, Id, Predecessor,Store),
            node(Id, Pred, Successor,S);
    %a predecessor needs to know our predecessor
        {request, Peer}->
            request(Peer,Predecessor),
            node(Id,Predecessor,Successor,Store);
    %our successor informs us about its predecessor
        {status,Pred}->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor,Succ,Store);
    %create a probe
        probe->
            create_probe(Id,Successor),
            node(Id,Predecessor,Successor,Store);
    %we know that we send the probe since Id is equal
        {probe,Id,Nodes,T}->
            remove_probe(T,Nodes),
            node(Id,Predecessor,Successor,Store);
    %pass the probe to next node
        {probe,Ref,Nodes,T}->
            forward_probe(Ref,T,Nodes,Id,Successor),
            node(Id,Predecessor,Successor,Store);
    %add a key value pair
        {add,Key,Value,Qref,Client}->
            Added = add(Key,Value,Qref,Client,Id,Predecessor,Successor,Store),
            node(Id,Predecessor,Successor,Added);
    %lookup for key value pair
        {lookup,Key,Qref,Client}->
            lookup(Key,Qref,Client,Id,Predecessor,Successor,Store),
            node(Id,Predecessor,Successor,Store);
    %new nodes want to join
    %update the storage
        {handover,Elements}->
            Merged = storage:merge(Store,Elements),
            node(Id,Predecessor,Successor,Merged);

    %to stabilize
    %use when a new node is added
        stabilize->
            stabilize(Successor),
            node(Id,Predecessor,Successor,Store)
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
                %?
                true->
                    self() ! stabilize,
                    Xpid ! {request, self()},
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
notify({Nkey,Npid},Id,Predecessor,Store)->
    case Predecessor of
        nil->
            %update the storage
            Keep = handover(Id,Store,Nkey,Npid),
            {{Nkey,Npid},Keep};
        {Pkey,_}->
            case key:between(Nkey,Pkey,Id) of
                true->
                    %update the storage
                    Keep = handover(Id,Store,Nkey,Npid),
                    {{Nkey,Npid},Keep};
                %new node is before the predecessor
                false->
                    %Npid!{status, Predecessor},
                    {Predecessor,Store}
            end
    end.

%split the storage
%one part is kept, another part is passed to the new node
handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.


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

%check if we should take care of the key (predecessor, us]
%if we should, then add the new key value
%if we should not, give the key to our successor
add(Key,Value,Qref,Client,Id,{Pkey,_},{_,Spid},Store)->
    case key:between(Key,Pkey,Id) of
        true->
            Client!{Qref,ok},
            storage:add(Key,Value,Store);
        false->
            Spid!{add,Key,Value,Qref,Client},
            Store
    end.

%check if we are responsible for the key
%if so, we lookup in the store and send the reply
%if not, we forward the request
lookup(Key,Qref,Client,Id,{Pkey,_},Successor,Store)->
    case key:between(Key,Pkey,Id) of
        true->
            Result = storage:lookup(Key,Store),
            Client!{Qref,Result};
        false->
            {_,Spid} = Successor,
            Spid!{lookup, Key,Qref,Client}
    end.
