%%%-------------------------------------------------------------------
%%% @author zhaoxu-b
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 十一月 2013 下午3:56
%%%-------------------------------------------------------------------
-module(deploy_sup).
-author("zhaoxu-b").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    deploy_pool:init_server_pool(),
    TaskPoolSize = case application:get_env(deploy, task_pool_size) of
                       undefined ->
                           10;
                       Any ->
                           Any
                   end,
    KvSpecs = contact_kv_specs(TaskPoolSize),
    ReportSpecs = [{deploy_report, {deploy_report, start_link, []}, permanent, 5000, worker, []}],
    {ok, {{one_for_one, 5, 10}, KvSpecs ++ ReportSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


contact_kv_specs(Amount) ->
    lists:foldl(fun(I, Acc) ->
        [{I,
            {deploy_pool,
                start_link, []},
            permanent,
            5000,
            worker,
            []
        }] ++ Acc

    end, [], lists:seq(1, Amount)).
