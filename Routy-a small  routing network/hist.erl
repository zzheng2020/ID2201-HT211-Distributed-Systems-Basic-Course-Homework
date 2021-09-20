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
    Dict = dict:new(),
    dict:append(Name, inf, Dict).


update(Node, N, History) ->
    case dict:find(Node, History) of
        {ok, [Value | _ ]} ->
            if
                N > Value ->
                    AuxDict = dict:erase(Node, History),
                    Updated = dict:append(Node, N, AuxDict),
                    {new, Updated};
                true ->
                    old
            end;
        error ->
            {new, dict:append(Node, N, History)}
    end.