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
```

