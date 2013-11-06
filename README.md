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

1. compile the code, mkdir three deploy node.

	cd deploy
	sh rebuild
	cd rel/
	cp -r deply deploy1
	cp -r deply deploy2
	cp -r deply deploy3
	
2. configure app.config of deploy1

    [
        {kernel, [{start_pg2, true}]},
        {sasl, [
            {sasl_error_logger, false}
        ]
        },
        {deploy, [
            {apps, [
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
                        %% beam directory of app.
                        "/home/zhaoxu-b/",
                        "/data/deploy/bin/deploy restart",
                        'test_deploy',
                        ['deploy01@127.0.0.1', 'deploy02@127.0.0.1', 'deploy03@127.0.0.1']
                    }
                }
            ]}
        ]
        }
    ].
	

	
