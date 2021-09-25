# Homework 2 - Roudy - A Small Routing Network

```erlang
%% list:keyfind(Key, N, TupleList) -> Tuple | false
%% example:
1> List = [{name, "Ziheng Zhang"}, {name, "Test"}].
[{name,"Ziheng Zhang"},{name,"Test"}]
2> lists:keyfind("Ziheng Zhang", 2, List).
{name,"Ziheng Zhang"}

%% lists:keydelete(Key, N, TupleList1) -> TupleList2
%% example:
1> List = [{name, "Ziheng Zhang"}, {name, "Test"}].
[{name,"Ziheng Zhang"},{name,"Test"}]
2> lists:keydelete("Test", 2, List).
[{name,"Ziheng Zhang"}]

%% lists:map(Fun, List1) -> List2
%% example:
1> lists:map(fun(X) -> [X, X] end, [1, 2, 3]).
[[1,1],[2,2],[3,3]]

%% lists:foldl(Fun, Acc0, List) -> Acc1
%% example:
1> lists:foldl(fun(X, Sum) -> Sum + X, 0, [1, 2, 3]).
6



[{Beilin, [London, Paris]}, {London, [Berlin, Beijing, Stockh]}]

%%            case interfaces:ref(Node, Intf) of
%%                {ok, Ref} ->
%%                    erlang:demonitor(Ref),
%%                    Intf1 = interfaces:remove(Node, Intf),
%%                    router(Name, N, Hist, Intf1, Table, Map);
%%                notfound ->
%%                    io:format("Remove unknown node!")
%%            end,
%%            router(Name, N, Hist, Intf, Table, Map);

---------------------------------- Test ---------------------------------------------

erl -name ziheng@zhang -setcookie routy -connect_all false

routy:start(r1, test1),
routy:start(r2, test2),
routy:start(r3, test3),
r1 ! {add, test2, {r2, 'ziheng@zhang'}},
r2 ! {add, test3, {r3, 'ziheng@zhang'}},
r3 ! {add, test1, {r1, 'ziheng@zhang'}},

r1 ! broadcast,
r2 ! broadcast,
r3 ! broadcast,

r1 ! update,
r2 ! update,
r3 ! update,

```

