%%%-------------------------------------------------------------------
%%% @author zhangziheng
%%% @copyright (C) 2021, Ziheng Zhang
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct. 2021 12:56 AM
%%%-------------------------------------------------------------------
-module(key).
-author("zhangziheng").

%% API
-export([generate/0, between/3]).

generate() ->
    rand:uniform(1000).

%% e.g.
%% 8 nodes form a circle
%% 1. Key = 5, (From = 3, To = 6]:
%%    true: From < Key <= To
%%    else false
between(Key, From, To) when From < To ->
    From < Key andalso Key =< To;
%% 2. Key = 8, (From = 6, To = 2]
%%    Key = 1, (From = 6, To = 2]:
%%    true: Key <= To or Key > From
%%    else false
between(Key, From, To) when From > To ->
    Key =< To orelse From < Key;
between(_, From, To) when From == To ->
    true.
