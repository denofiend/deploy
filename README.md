
Name
====

Hot Upgrade Erlang Clusters, scp directory, file to clusters.( all in config file)


Description
===========

If you have hundreds of erlang servers, How long it takes to upgrade the cluster? 

you can do these steps:
	1. ssh -l yourname server_ip
	2. scp file yourname@serverip:/your/app/dir
	3. restart your app server.


REQUIRED
========

1. erlang


INSTALL
=======

  cd deploy
  sh rebuild
  cd rel/deploy
  
