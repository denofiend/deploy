%%%-------------------------------------------------------------------
%%% @author zhaoxu-b
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十一月 2013 上午9:51
%%%-------------------------------------------------------------------
-module(deploy_report).
-author("zhaoxu-b").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(server, {name, total, ok, error}).
-record(state, {servers}).

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
    {ok, #state{servers = []}}.

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
handle_call({Sign, ServerName, Data}, _From, State) ->
    #state{servers = Servers} = State,

    NewState = case Sign of
                   report ->
                       {TotalCount, OkCount, ErrorList} = Data,
                       case lists:keyfind(ServerName, 2, Servers) of
                           #server{name = _Name, total = Total, ok = Ok, error = Error} ->
                               NewServer = #server{name = ServerName, total = Total, ok = Ok + OkCount, error = Error ++ ErrorList},
                               io:format("server:~p, total files:(~p), ok:(~p), error:(~p).~n", [ServerName, Total, Ok + OkCount, Error ++ ErrorList]),
                               #state{servers = lists:keyreplace(ServerName, 2, Servers, NewServer)};
                           _ ->
                               io:format("server:~p, total files:(~p), ok:(~p), error:(~p).~n", [ServerName, TotalCount, OkCount, ErrorList]),
                               #state{servers = lists:append(#server{name = ServerName, total = TotalCount, ok = OkCount, error = ErrorList})}
                       end;
                   reset ->
                       {ResetTotal} = Data,
                       io:format("server:~p, total files:(~p), ok:(~p), error:(~p).~n", [ServerName, ResetTotal, 0, []]),
                       #state{servers = lists:keystore(ServerName, 2, Servers, #server{name = ServerName, total = ResetTotal, ok = 0, error = []})}
               end,
    Reply = ok,
    {reply, Reply, NewState}.

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
