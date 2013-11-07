Name
====

Hot Upgrade Erlang Clusters, scp directory, file to clusters.( all in config file)


Description
===========

If you have hundreds of erlang servers, How long it takes to upgrade the cluster? 

you can do these steps:

	1. scp file yourname@serverip:/your/app/dir
	2. ssh -l yourname server_ip
	3. restart your app server.
	
but now, you can use deploy tools to hot upgrade your Erlang Clusters, scp beam file, deploy new directory to new server.
enjoy it, and you can contribute code to Improve the project.


REQUIRED
========

	1. erlang


INSTALL
=======

	cd deploy
	sh rebuild
	cd rel/deploy
	bin/deploy console
	deploy:deploy_file(app, "file“).
	
Example
=======

compile the code, mkdir three deploy node.

	cd deploy
	sh rebuild
	cd rel/
	cp -r deply deploy1
	cp -r deply deploy2
	cp -r deply deploy3
	
configure app.config of deploy1, deploy2, deploy3(etc/app.config)

    [
        {kernel, [{start_pg2, true}]},
        {sasl, [
            {sasl_error_logger, false}
        ]
        },
        {deploy, [
            {apps, [
            	%% deploy app.
                {deploy,
                    %% ssh config of app servers.
                    {app_config, [
                        {code_173, [
                            {ssh, "your server ip"},
                            {port, 22},
                            {user, "your name"},
                            {password, "password~"}
                        ]}
                    ], 
                    	%% restart server command.
                        "/data/deploy/bin/deploy restart",
                        
                        %% cookie of Erlang cluster.
                        'test_deploy',
                        
                        %% name of Erlang cluster.
                        ['deploy01@127.0.0.1', 'deploy02@127.0.0.1', 'deploy03@127.0.0.1']
                    }
                }
            ]}
        ]
        }
    ].

configure vm.args of deploy1(deploy01/etc/vm.args)

	## Name of the node
	-name deploy01@127.0.0.1
	
	## Cookie for distributed erlang
	-setcookie test_deploy
	
	## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive
	## (Disabled by default..use with caution!)
	##-heart
	
	## Enable kernel poll and a few async threads
	+K true
	
	+A 64
	
	+P 250000
	
	+zdbbl 32768
	
	+S 12:12
	## Increase number of concurrent ports/sockets
	-env ERL_MAX_PORTS 64000
	-env ERL_MAX_ETS_TABLES 256000
	
	## Tweak GC to run more often
	-env ERL_FULLSWEEP_AFTER 0
	
	-env ERL_CRASH_DUMP_SECONDS 15

configure vm.args of deploy2(deploy02/etc/vm.args)

	## Name of the node
	-name deploy02@127.0.0.1
	
	## Cookie for distributed erlang
	-setcookie test_deploy
	
	## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive
	## (Disabled by default..use with caution!)
	##-heart
	
	## Enable kernel poll and a few async threads
	+K true
	
	+A 64
	
	+P 250000
	
	+zdbbl 32768
	
	+S 12:12
	## Increase number of concurrent ports/sockets
	-env ERL_MAX_PORTS 64000
	-env ERL_MAX_ETS_TABLES 256000
	
	## Tweak GC to run more often
	-env ERL_FULLSWEEP_AFTER 0
	
	-env ERL_CRASH_DUMP_SECONDS 15
	
configure vm.args of deploy3(deploy03/etc/vm.args)

	## Name of the node
	-name deploy03@127.0.0.1
	
	## Cookie for distributed erlang
	-setcookie test_deploy
	
	## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive
	## (Disabled by default..use with caution!)
	##-heart
	
	## Enable kernel poll and a few async threads
	+K true
	
	+A 64
	
	+P 250000
	
	+zdbbl 32768
	
	+S 12:12
	## Increase number of concurrent ports/sockets
	-env ERL_MAX_PORTS 64000
	-env ERL_MAX_ETS_TABLES 256000
	
	## Tweak GC to run more often
	-env ERL_FULLSWEEP_AFTER 0
	
	-env ERL_CRASH_DUMP_SECONDS 15

now, you can start three apps:

	cd deploy1
	bin/deploy console
	cd deploy2
	bin/deploy console
	cd deploy3
	bin/deploy console
	
new a function in deploy.erl:

	mmgg() ->
		io:format("this mmgg function", []).
		
then make, cp deploy.beam to lib directory of deploy1

	cd deploy
	make
	cp ebin/deploy.beam rel/deploy01/lib/deploy-1/ebin/deploy.beam
	
hot upgrade deploy module in deploy1 erlang node:

	cd deploy1
	bin/deploy console
	deploy:hot_upgrade(deploy, "/home/zhaoxu-b/deploy/rel/deploy01/lib/deploy-1/ebin/deploy.beam").
	
go to deploy2, check

	cd deploy2
	bin/deploy console
	deploy:mmgg().
