%%%-------------------------------------------------------------------
%%% File    : acceptor.erl
%%% Author  :  <os@localhost>
%%% Description : 
%%%
%%% Created :  5 Aug 2011 by  <os@localhost>
%%%-------------------------------------------------------------------
-module(acceptor).


%% API
-export([start_link/1, accept/1]).



start_link(LSock) -> 
    Pid = spawn_link(fun() -> accept(LSock) end),
    {ok, Pid}.


accept(LSock) ->    
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = peer_sup:start_peer(Sock),
    ok = gen_tcp:controlling_process(Sock, Pid),
    accept(LSock).
