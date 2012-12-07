%%% @author greg <>
%%% @copyright (C) 2012, greg
%%% @doc
%%%
%%% @end
%%% Created : 30 Nov 2012 by greg <>

-module(config).

-behaviour(gen_server).

%% API
-export([start_link/0, get_param/1, reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {file, options}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-------------------------------------------------------------------
%% Func: get_param(Key) -> {ok,  Value} | {error, Reason}.
%%                             Value :: term()
%% Desc: Reads config from State using Key and returs Value.
%%-------------------------------------------------------------------
get_param(Key) ->
    gen_server:call(?SERVER, {get_param, Key}).

%%-------------------------------------------------------------------
%% Func: reload() -> ok | Error.
%% Desc: Читает конфиг файл global.config, записывает параметры в #state{}
%%       и посылает всем нодам список конфига через rpc:cast.
%%-------------------------------------------------------------------
reload() ->
    gen_server:call(?SERVER, reload).


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
init([]) ->
    {ok, ConfFile} = application:get_env(tcpproxy, conf_file),
    {ok, List} = file:consult(ConfFile),
    {ok, #state{file = ConfFile, options = dict:from_list(List)}}.

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
handle_call({get_param, Key}, _From, State) ->
    Reply = dict:find(Key, State#state.options),
    {reply, Reply, State};

handle_call(reload, _Form, State) ->
    {NewState, Reply} = 
	case file:consult(State#state.file) of
	    {ok, List} ->
		{State#state{options = dict:from_list(List)}, ok};
	    {error, Reason} -> {State, Reason}
	end,
    {reply, Reply, NewState};

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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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
