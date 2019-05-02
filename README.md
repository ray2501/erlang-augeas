erlang-augeas
=====

It is an Erlang bindings for [Augeas](http://augeas.net/).

Build
-----

To build erlang-augeas you need a C compiler, `make`, `pkg-config`, 
and the development packages for Augeas. Below are the commands to 
install these on several operating systems.

Debian/Ubuntu

    sudo apt install -y build-essential libaugeas-dev pkg-config

openSUSE

    sudo zypper install -y augeas-devel gcc make pkg-config

How to build this package:

    $ rebar3 compile

