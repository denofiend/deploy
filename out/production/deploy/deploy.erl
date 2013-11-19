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
    start/2, help/0,
    stop/1, scp_file/3, restart_server/1, scp_dir/3, hot_upgrade_file/2, hot_upgrade_dir/2, scp_files/2
]).


-record(app_config, {
    server_list,
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
    %%io:format("start_ssh_config"),
    ct_run:install([]),
    ct_config:start(interactive),
    ct_util:start(),

    Configs = case application:get_env(deploy, apps) of
                  {ok, AppConfigs} ->
                      AppConfigs;
                  _ ->
                      []
              end,
    %%io:format("Configs:~p~n", [Configs]),

    lists:foreach(
        fun(Conf) ->
            io:format("Conf:~p~n", [Conf]),
            {_App, #app_config{server_list = ServerList}} = Conf,
            lists:foreach(
                fun(OneConfig) ->
                    %%io:format("Name:~p, OneConfig:~p~n", ['_UNDEF', [OneConfig]]),
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
                    {error, lists:concat([App, " is undefined"])};
                Config ->
                    {ok, Config}
            end;
        _ ->
            {error, "apps is undfined"}
    end.



mkdir_basename(AppDir, CH, Dir) ->
    %%io:format("mkdir_basename AppDir:~p, CH:~p, Dir:~p~n", [AppDir, CH, Dir]),
    NewAppDir = filename:join([AppDir, filename:basename(Dir)]),
    case ct_ssh:read_file_info(CH, NewAppDir) of
        {error, no_such_file} ->
            ct_ssh:make_dir(CH, NewAppDir);
        _ ->
            ok
    end.

%% --------------------------------------------------------------------
%% Function: get_files/3
%% Description: get all need scp files in directory.
%% Returns: ok
%% --------------------------------------------------------------------
get_files(_CH, [], ResultFiles) ->
    ResultFiles;

get_files(CH, [{LocalDirs, RemoteDir} | T], ResultFiles) ->
    io:format("get_files: LocalDirs:~p, RemoteDir:~p CH:~p, ResultFiles:~p~n", [LocalDirs, RemoteDir, CH, ResultFiles]),

    {SubDirs, TmpResultFiles} = lists:foldl(
        fun(File, {InSubDirs, InResultFiles}) ->

            case filelib:is_dir(File) of
                false ->
                    {InSubDirs, [{File, RemoteDir} | InResultFiles]};
                true ->
                    mkdir_basename(RemoteDir, CH, File),

                    case file:list_dir(File) of
                        {ok, SubFiles} ->
                            NewAppDir = filename:join([RemoteDir, filename:basename(File)]),

                            TmpSubDirs = lists:foldl(
                                fun(SubFile, InnSubDirs) ->
                                    [{filename:join([File, SubFile]), NewAppDir} | InnSubDirs]
                                end, InSubDirs, SubFiles),

                            {TmpSubDirs, InResultFiles};
                        Any ->
                            throw(Any)
                    end
            end

        end, {[], ResultFiles}, LocalDirs),

    get_files(CH, T ++ SubDirs, TmpResultFiles).



%% --------------------------------------------------------------------
%% Function:scp_dir/3
%% Description: deploy directory to remote servers for app.
%% Returns: ok
%% --------------------------------------------------------------------
scp_dir(App, LocalDir, RemoteDir) ->
%%io:format("deploy dir ~p servers, LocalDir:~p, RemoteDir:~p~n", [App, LocalDir, RemoteDir]),

    try
        case get_config(App) of
            {ok, {App, Config}} ->
                #app_config{server_list = ServerList} = Config,

                lists:foreach(
                    fun(ServerConfig) ->
                        case ServerConfig of
                            {Name, _} ->
                                {ok, CH} = ct_ssh:connect(Name, sftp),
                                Files = get_files(CH, [{LocalDir, RemoteDir}], []),
                                ct_ssh:disconnect(CH),

                                deploy_pool:scp_files(App, Files);
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
%% Function:scp_file/3
%% Description: deploy file to remote servers for app.
%% Returns: ok
%% --------------------------------------------------------------------
scp_file(App, LocalFile, RemotePath) ->
    io:format("deploy file ~p servers, LocalFile:~p, RemotePath:~p~n", [App, LocalFile, RemotePath]),
    try
        case get_config(App) of
            {ok, {App, Config}} ->
                #app_config{server_list = ServerConfigList} = Config,
                %% sync file to server
                case file:read_file(LocalFile) of
                    {ok, FileData} ->
                        lists:foreach(
                            fun(ServerConfig) ->
                                case ServerConfig of
                                    {Name, _} ->
                                        {ok, CH} = ct_ssh:connect(Name, sftp),
                                        RemoteFile = filename:join([RemotePath, filename:basename(LocalFile)]),
                                        case ct_ssh:write_file(CH, RemoteFile, FileData) of
                                            {error, Reason} ->
                                                io:format("scp local file(~p) to remote(~p) file(~p) return: ~p~n", [LocalFile, Name, RemoteFile, {error, Reason}]),
                                                throw({error, Reason});
                                            Any ->
                                                io:format("scp local file(~p) to remote(~p) file(~p) return: ~p~n", [LocalFile, Name, RemoteFile, Any]),
                                                ok
                                        end,
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
%% Function:scp_files/2
%% Description: deploy file to remote servers for app.
%% Returns: ok
%% --------------------------------------------------------------------
scp_files(App, NewFileList) ->
    %%io:format("deploy file ~p servers, NewFileList:~p~n", [App, NewFileList]),

    case get_config(App) of
        {ok, {App, Config}} ->
            #app_config{server_list = ServerConfigList} = Config,
            %% sync file to server
            lists:foldl(
                fun(ServerConfig, {OkCount, ErrorList}) ->
                    case ServerConfig of
                        {Name, _} ->
                            case ct_ssh:connect(Name, sftp) of
                                {ok, CH} ->
                                    {OutOkCount, OutErrorList} = lists:foldl(
                                        fun({LocalFile, RemotePath}, {InOkCount, InErrorList}) ->
                                            case file:read_file(LocalFile) of
                                                {ok, FileData} ->
                                                    RemoteFile = filename:join([RemotePath, filename:basename(LocalFile)]),
                                                    case ct_ssh:write_file(CH, RemoteFile, FileData) of
                                                        {error, Reason} ->
                                                            {InOkCount, [Reason | InErrorList]};
                                                        _ ->
                                                            {InOkCount + 1, InErrorList}
                                                    end;

                                                _ ->
                                                    {InOkCount, ["file is not exists." | InErrorList]}
                                            end
                                        end, {OkCount, ErrorList}, NewFileList),
                                    ct_ssh:disconnect(CH),
                                    {OutOkCount, OutErrorList};
                                Any ->
                                    {OkCount, [Any | ErrorList]}
                            end;
                        _ ->
                            {OkCount, ["ssh config error" | ErrorList]}
                    end
                end, {0, []}, ServerConfigList);
        Error ->
            {0, Error}
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
                                case ct_ssh:exec(CH1, RestartCommand) of
                                    {ok, _Data} ->
                                        io:format("restart server [~p]ok~n", [ServerConfig]);
                                    ErrorAny ->
                                        throw(ErrorAny)
                                end,
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
%% Function:reload_file/2
%% Description: reload module from file.
%% Returns: ok
%% --------------------------------------------------------------------
reload_file(Nodes, ModuleFile) ->
    io:format("reload file: ModuleFile:~p~n", [ModuleFile]),
    case is_beam_file(ModuleFile) of
        true ->
            case code:soft_purge(erlang:list_to_atom(filename:rootname(filename:basename(ModuleFile)))) of
                true ->
                    case code:load_abs(filename:rootname(ModuleFile)) of
                        {module, Module} ->
                            case code:add_path(filename:dirname(ModuleFile)) of
                                true ->
                                    {Mod, Bin, File} = code:get_object_code(Module),
                                    {ResL, BadNodes} = rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]),
                                    io:format("Res:~p, BadNodes:~p~n", [ResL, BadNodes]);
                                AddPathError ->
                                    throw(AddPathError)
                            end;
                        Any ->
                            throw(Any)
                    end;
                false ->
                    throw({error, 'code_purge_err'})
            end;
        false ->
            io:format("~p is not beam file~n", [ModuleFile])
    end.

%% --------------------------------------------------------------------
%% Function:is_beam_file/1
%% Description: is_beam_file
%% Returns: ok
%% --------------------------------------------------------------------
is_beam_file(File) ->
    ".beam" =:= filename:extension(File).

%% --------------------------------------------------------------------
%% Function:hot_upgrade_file/2
%% Description: hot upgrade one Module in all servers of App.
%% Returns: ok
%% --------------------------------------------------------------------
hot_upgrade_file(App, ModuleFile) ->
    io:format("hot upgrade ~p servers, ModuleFile:~p~n", [App, ModuleFile]),
    try
        case get_config(App) of
            {ok, {App, Config}} ->
                #app_config{cookie = Cookie, nodes = Nodes} = Config,

                %% set cookie
                erlang:set_cookie(node(), Cookie),

                %% hot upgrade the Module
                reload_file(Nodes, ModuleFile),
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
%% Function:hot_upgrade_dir/2
%% Description: hot upgrade Module directory in all servers of App. not support -R
%% Returns: ok
%% --------------------------------------------------------------------
hot_upgrade_dir(App, ModuleDir) ->
    io:format("hot upgrade ~p servers, ModuleDir:~p~n", [App, ModuleDir]),
    try
        case get_config(App) of
            {ok, {App, Config}} ->
                #app_config{cookie = Cookie, nodes = Nodes} = Config,

                %% set cookie
                erlang:set_cookie(node(), Cookie),

                case file:list_dir(ModuleDir) of
                    {ok, FileNames} ->
                        lists:foreach(
                            fun(ModuleFile) ->
                                %% hot upgrade the Module
                                reload_file(Nodes, filename:join([ModuleDir, ModuleFile]))
                            end, FileNames);
                    _ ->
                        throw({error, lists:concat([ModuleDir, " is not a directory"])})
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
%% Function:help/0
%% Description: how to use deploy tools.
%% Returns: ok
%% --------------------------------------------------------------------
help() ->
    io:format("scp_file(App, LocalFile, RemotePath):    -- scp LocalFile to RemotePath of all servers for App.~n~n", []),
    io:format("scp_dir(App, LocalDir, RemoteDir):       -- scp LocalDir to RemoteDir of all servers for App.~n~n", []),
    io:format("hot_upgrade_file(App, ModuleFile):       -- hot upgrade ModuleFile for App.~n~n", []),
    io:format("hot_upgrade_dir(App, ModuleDir):         -- hot upgrade all ModuleFile of ModuleDir for App.~n~n", []),
    io:format("restart_server(App, ModuleDir):          -- restart all servers for App.~n~n", []),

    io:format("------example-------~n~n", []),

    io:format("scp_file(App, LocalFile, RemotePath):    -- deploy:scp_file(deploy, \"/home/zhaoxu/file\", \"/data\").~n~n", []),
    io:format("scp_dir(App, LocalDir, RemoteDir):       -- deploy:scp_dir(deploy, \"/home/zhaoxu/dir\", \"/data\").~n~n", []),
    io:format("hot_upgrade_file(App, ModuleFile):       -- deploy:hot_upgrade_file(deploy, \"/home/zhaoxu/example.beam\").~n~n", []),
    io:format("hot_upgrade_dir(App, ModuleDir):         -- deploy:hot_upgrade_dir(deploy, \"/home/zhaoxu/dir\").~n~n", []),
    io:format("restart_server(App, ModuleDir):          -- deploy:restart_server(deploy).~n~n", []).
