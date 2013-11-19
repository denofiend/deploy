%%%-------------------------------------------------------------------
%%% @author zhaoxu-b
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十一月 2013 上午9:23
%%%-------------------------------------------------------------------
-module(deploy_pool).
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
    code_change/3,
    init_server_pool/0,
    scp_files/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

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
    gen_server:start_link(?MODULE, [], []).


%%
%% @doc create ets to hold server pool
%%
init_server_pool() ->
    case ets:info(task_pool) of
        undefined ->
            task_pool = ets:new(task_pool, [named_table, public, set, {keypos, 2}, {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.


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
    true = ets:insert(task_pool, {server, self()}),
    {ok, #state{}}.

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
handle_cast({scp_file, ServerName, NewFileList}, State) ->
    %%io:format("hash_cast App:~p, NewFileList:~p~n", [App, NewFileList]),

    {OkCount, ErrorList} = deploy:scp_files(ServerName, NewFileList),
    gen_server:call(deploy_report, {report, {ServerName, OkCount, ErrorList}}),
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
    true = ets:delete(task_pool, self()),
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

scp_files(ServerName, FileList) ->
    ServerList = ets:match_object(task_pool, '$1'),
    Size = erlang:length(ServerList),
    if
        (Size =< 0) ->
            {error, server_pool_is_empty};
        true ->
            RSize = erlang:length(FileList),
            gen_server:call(deploy_report, {reset, {ServerName, RSize}}),
            SizePerServer = compute_msg_size_per_server(Size, RSize),
            Result = lets_go(ServerName, ServerList, Size, FileList, 1, SizePerServer, RSize),
            Result
    end.

compute_msg_size_per_server(ServerPoolSize, Receiver_Size) ->
    SizePerServer = Receiver_Size div ServerPoolSize,
    Remaining = Receiver_Size rem ServerPoolSize,
    case Remaining of
        0 ->
            SizePerServer;
        _ ->
            SizePerServer + 1
    end.

lets_go(ServerName, ServerList, ServerLen, FileList, NServer, SizePerServer, RSize) ->
    Start = (NServer - 1) * SizePerServer + 1,
    if
        (Start > RSize) ->
            ok;
        true ->
            NewFileList = lists:sublist(FileList, Start, SizePerServer),
            %%io:format("scp local files:~p to remote files by ~p group.~n", [NewFileList, NServer]),
            ok = gen_server:cast(element(2, lists:nth(NServer, ServerList)), {scp_file, ServerName, NewFileList}),
            case NServer of
                ServerLen ->
                    ok;
                _ ->
                    lets_go(ServerName, ServerList, ServerLen, FileList, NServer + 1, SizePerServer, RSize)
            end
    end.