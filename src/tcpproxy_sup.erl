
-module(tcpproxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    AcceptorSup = {acceptor_sup, {acceptor_sup, start_link, []},
                   permanent, infinity, supervisor, [accepter_sup]},
    ClientSup = {peer_sup, {peer_sup, start_link, []},
                 permanent, infinity, supervisor, [peer_sup]},
    Config = {config, {config, start_link, []},
              permanent, 2000, worker, [config]},

    {ok, { {one_for_one, 5, 10}, [AcceptorSup, ClientSup, Config]} }.

