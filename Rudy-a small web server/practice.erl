-module(practice).

-export([server/1, client/1]).

server(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, LSock} = gen_tcp:listen(Port, Opt),
    io:format("~w~n", [LSock]),
    {ok, Sock}  = gen_tcp:accept(LSock),
    {ok, Bin}   = do_recv(Sock, []),
    ok = gen_tcp:close(Sock),
    ok = gen_tcp:close(LSock),
    Bin.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

client(Port) ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, Port, Opt),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).