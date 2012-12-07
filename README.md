TCP Proxy
========

It connects two points using net. Can be used if direct connection
is not well enough. Another way to use tcpproxy to get somebody's tcp
traffic. TCP Proxy hasn't any analizer, but it is esay to add it.

TCP Proxy uses a dynamic configuration for the target host and port. It 
means you can change them in runtime.


### Dependencies

To build tcpproxy you will need a working installation of Erlang R14B03 
(or later).

Also tcpproxy uses alogger to log some events:
 * git://github.com/siberian-fast-food/alogger.git


### Download

git clone http://github.com/greggy/tcpproxy.git


### Configuration

All configurations are stored in ./rel/files/sys.config file. Section 'tcproxy'.
 * port: which port will be used to listen;
 * acceptors: how much acceptors will wait connection;
 * conf_file: path to the dynamic.config file;

If you change the target host and/or port you should reload it. Just connect
to the VM console and reload config:

# ./rel/bin/tcpproxy attach
> config:reload().

### Instalation

# cd ./tcpproxy
# ./rebar get-deps
# ./mkrel.sh

TCP Proxy is compiled and started!
