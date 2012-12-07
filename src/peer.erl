%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2012, greg
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2012 by greg <>
%%%-------------------------------------------------------------------
-module(peer).

-behaviour(gen_server).

-include_lib("alog/include/alog.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  socket
	  , target_socket
	  , get_bytes=0
	  , send_bytes=0
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Sock]) ->
    %process_flag(trap_exit),
    gen_server:cast(self(), setup_connection),
    {ok, #state{socket=Sock}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(setup_connection, State) ->
    {ok, Host} = config:get_param(target_host),
    {ok, Port} = config:get_param(target_port),
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary
					      , {active, true}
					      %% , {reuseaddr, true}
					     ]),
    ?DBG("Target socket: ~p", [Sock]),
    {noreply, State#state{target_socket=Sock}};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, State) when Socket == State#state.socket ->
    ok = gen_tcp:send(State#state.target_socket, Data),
    {noreply, State#state{send_bytes=State#state.send_bytes+byte_size(Data)}};

handle_info({tcp, Socket, Data}, State) when Socket == State#state.target_socket ->
    ok = gen_tcp:send(State#state.socket, Data),
    {noreply, State#state{get_bytes=State#state.get_bytes+byte_size(Data)}};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    ?DBG("Handle info: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    gen_tcp:close(State#state.target_socket),
    ?INFO("Peer was closed by socket [~w]", [State#state.socket]),
    ?INFO("Peer gets ~p bytes and send ~p bytes", [State#state.get_bytes,
						   State#state.send_bytes]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
