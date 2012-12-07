%%%-------------------------------------------------------------------
%%% File    : acceptor_sup.erl
%%% Author  :  <os@localhost>
%%% Description : 
%%%
%%% Created :  6 Aug 2011 by  <os@localhost>
%%%-------------------------------------------------------------------
-module(acceptor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    {ok, Port} = application:get_env(tcpproxy, port),
    {ok, LSock} = gen_tcp:listen(Port, [binary
					, {active, true}
					, {reuseaddr, true}
				       ]),
    {ok, A} = application:get_env(tcpproxy, acceptors),
    Acceptors = lists:map(fun(X) -> child_spec(X, LSock) end, lists:seq(1, A) ),
    {ok,{{one_for_one,5,1},Acceptors}}.


%%====================================================================
%% Internal functions
%%====================================================================
child_spec(Num, LSock) ->
    Ch = {{acceptor, Num},{acceptor,start_link,[LSock]},
	   permanent,2000,worker,[acceptor]},
    Ch.
    
