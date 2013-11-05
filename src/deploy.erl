%%%-------------------------------------------------------------------
%%% @author zhaoxu-b
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十月 2013 下午4:21
%%%-------------------------------------------------------------------
-module(deploy).
-author("zhaoxu-b").

-behaviour(application).

-export([
    start/2,
    stop/1, deploy_file/2, restart_server/1, deploy_dir/2, hot_upgrade/2
]).


-record(app_config, {
    server_list,
    app_dir,
    restart_command,
    cookie,
    nodes
}).

%% --------------------------------------------------------------------
%% Function: start_ssh_config/0
%% Description: start ct_run, ct_config, ct_util
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
start_ssh_config() ->
    io:format("start_ssh_config"),
    ct_run:install([]),
    ct_config:start(interactive),
    ct_util:start(),

    Configs = case application:get_env(deploy, apps) of
                  {ok, AppConfigs} ->
                      AppConfigs;
                  _ ->
                      []
              end,
    io:format("Configs:~p~n", [Configs]),

    lists:foreach(
        fun(Conf) ->
            io:format("Conf:~p~n", [Conf]),
            {_App, #app_config{server_list = ServerList}} = Conf,
            lists:foreach(
                fun(OneConfig) ->
                    io:format("Name:~p, OneConfig:~p~n", ['_UNDEF', [OneConfig]]),
                    ct_config:set_default_config('_UNDEF', [OneConfig], undefined)
                end, ServerList)
        end, Configs).


start(_Type, []) ->
    start_ssh_config(),
    deploy_sup:start_link().

%% @spec stop(State::term()) -> any()
%%
%% @doc This is callback after application shut down.
%% @private
stop(State) ->
    ct_util:stop(State),
    ok.


%% --------------------------------------------------------------------
%% Function: get_config/1
%% Description: get app config from application env.
%% Returns: {ok, Config}
%% --------------------------------------------------------------------
get_config(App) ->
    case application:get_env(deploy, apps) of
        {ok, Configs} ->
            case lists:keyfind(App, 1, Configs) of
                false ->
                    {error, lists:connect([App, " is undefined"])};
                Config ->
                    {ok, Config}
            end;
        _ ->
            {error, "apps is undfined"}
    end.

%% --------------------------------------------------------------------
%% Function: send_dir/4
%% Description: scp directory.
%% Returns: ok
%% --------------------------------------------------------------------
send_dir(_AppDir, _CH, _Dir, []) ->
    ok;

send_dir(AppDir, CH, Dir, [File | T]) ->
    FullName = filename:join([Dir, File]),
    case filelib:is_file(FullName) of
        true ->
            %% sync file to server
            {ok, FileData} = file:read_file(FullName),
            ct_ssh:write_file(CH, lists:concat([AppDir, filename:basename(FullName)]), FileData);
        false ->
            send_dir(AppDir, CH, FullName, file:list_dir(FullName))
    end,
    send_dir(AppDir, CH, Dir, T).

%% --------------------------------------------------------------------
%% Function:deploy_dir/2
%% Description: deploy directory to remote servers for app.
%% Returns: ok
%% --------------------------------------------------------------------
deploy_dir(App, Dir) ->
    io:format("deploy dir ~p servers, Directory:~p~n", [App, Dir]),

    try
        case get_config(App) of
            {ok, {App, Config}} ->
                #app_config{server_list = ServerList, app_dir = AppDir} = Config,
                {ok, FileNames} = file:list_dir(Dir),

                lists:foreach(
                    fun(ServerConfig) ->
                        case ServerConfig of
                            {Name, _} ->
                                {ok, CH} = ct_ssh:connect(Name, ssh),

                                send_dir(AppDir, CH, Dir, FileNames),

                                ct_ssh:disconnect(CH);
                            _ ->
                                throw({error, "ssh config error"})
                        end
                    end, ServerList),
                ok;
            _ ->
                io:format("~p is undefined App in deploy.app file.~n", [App]),
                throw({error, "app is undefined"})
        end
    catch
        Any ->
            Any
    end.

%% --------------------------------------------------------------------
%% Function:deploy_file/2
%% Description: deploy file to remote servers for app.
%% Returns: ok
%% --------------------------------------------------------------------
deploy_file(App, File) ->
    io:format("deploy file ~p servers, file:~p~n", [App, File]),
    try
        case get_config(App) of
            {ok, {App, Config}} ->
                #app_config{server_list = ServerConfigList, app_dir = AppDir} = Config,
                %% sync file to server
                case file:read_file(File) of
                    {ok, FileData} ->
                        lists:foreach(
                            fun(ServerConfig) ->
                                case ServerConfig of
                                    {Name, _} ->
                                        %%io:format("Name:~p, AppDir:~p, RemoteRelPath:~p~n", [Name, AppDir, lists:concat([AppDir, filename:basename(File)])]),
                                        {ok, CH} = ct_ssh:connect(Name, sftp),
                                        ct_ssh:write_file(CH, lists:concat([AppDir, filename:basename(File)]), FileData),
                                        ct_ssh:disconnect(CH);
                                    _ ->
                                        throw({error, "ssh config error"})
                                end
                            end, ServerConfigList);
                    _ ->
                        throw({error, "file is not exists."})
                end,
                ok;
            Error ->
                io:format("err:~p.~n", [Error]),
                Error
        end
    catch
        Any ->
            Any
    end.

%% --------------------------------------------------------------------
%% Function:restart_server/1
%% Description: restart all servers of App.
%% Returns: ok
%% --------------------------------------------------------------------
restart_server(App) ->
    io:format("restart ~p servers~n", [App]),
    try
        case get_config(App) of
            {ok, {App, Config}} ->
                #app_config{server_list = ServerList, restart_command = RestartCommand} = Config,
                lists:foreach(
                    fun(ServerConfig) ->
                        case ServerConfig of
                            {Name, _} ->
                                {ok, CH1} = ct_ssh:connect(Name, ssh),
                                ct_ssh:exec(CH1, RestartCommand),
                                ct_ssh:disconnect(CH1);
                            _ ->
                                throw({error, "ssh config error"})
                        end
                    end, ServerList),
                ok;
            Error ->
                io:format("err:~p.~n", [Error]),
                Error
        end
    catch
        Any ->
            Any
    end.

%% --------------------------------------------------------------------
%% Function:hot_upgrade/2
%% Description: hot upgrade one Module in all servers of App.
%% Returns: ok
%% --------------------------------------------------------------------
hot_upgrade(App, Module) ->
    io:format("hot upgrade ~p servers, Module:~p~n", [App, Module]),
    try
        case get_config(App) of
            {ok, {App, Config}} ->
                #app_config{cookie = Cookie, nodes = Nodes} = Config,

                %% set cookie
                erlang:set_cookie(node(), Cookie),
                %% ping other nodes.
                lists:foreach(fun(ServerNode) -> net_adm:ping(ServerNode) end, Nodes),

                io:format("server nodes:~p~n", [nodes()]),

                %% hot upgrade the Module
                {Mod, Bin, File} = code:get_object_code(Module),
                {ResL, BadNodes} = rpc:multicall(code, load_binary, [Mod, File, Bin]),
                io:format("Res:~p, BadNodes:~p~n", [ResL, BadNodes]),
                ok;
            Error ->
                io:format("err:~p.~n", [Error]),
                Error
        end
    catch
        Any ->
            Any
    end.

