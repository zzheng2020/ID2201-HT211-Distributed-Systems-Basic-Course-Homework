%%%-------------------------------------------------------------------
%%% @author zhangziheng
%%% @copyright (C) 2021, zhangziheng
%%% @doc
%%%
%%% @end
%%% Created : 04. Otc. 2021 11:00 AM
%%%-------------------------------------------------------------------
-module(gms1).
-author("zhangziheng").

%% API
-export([start/1, start/2]).

%% id    : a unique name of the node, only used for debugging.
%% master: the process identifier of the application layer.
%% slaves: an ordered list of the process identifiers of all slaves in the group.
%% group : a list of all application layer processes in the group.


%% Id = N
start(Id) ->
    %% Self is the PID of this process.
    Self = self(),
%%    io:format("Self: ~p, ", [Self]),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

%% Master = PID
init(Id, Master) ->
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader | Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    end.


%% At the begin, Master = PID, Slaves = [], Group = [Master]
leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
%%            io:format("Master: ~p, ", [Master]),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self() | Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop -> ok
    end.

slave(Id, Master, Leader, Slaves, Group) ->
%%    io:format("Leader, ~p, ", [Leader]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader | Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        stop -> ok end.

bcast(_Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).
