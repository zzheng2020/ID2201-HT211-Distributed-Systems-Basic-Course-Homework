%%%-------------------------------------------------------------------
%%% @author Ziheng Zhang
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2021 20:12
%%%-------------------------------------------------------------------
-module(hist).
-author("Ziheng Zhang").

%% API
-export([new/1, update/3]).

%% return a new history, where messages from Name will
%% always be seen as old.
new(Name) ->
    [{Name, inf}].

%% Check if message number N from the Node
%% is old or new. If it is old then return old
%% but if it new return {new, Updated}
%% where Updated is the updated history.
update(Node, N, History)->
    case lists:keyfind(Node, 1, History) of
        {Node, MessageN} ->
            if
                N > MessageN ->
                    {new, [{Node, N} | lists:keydelete(Node, 1, History)]};
                true ->
                    old
            end;
        false ->
            {new, [{Node, N} | lists:keydelete(Node, 1, History)]}
    end.