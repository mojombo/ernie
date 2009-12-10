Ernie
=====

By Tom Preston-Werner (tom@mojombo.com)

Ernie is a BERT-RPC server implementation that uses an Erlang server to accept incoming connections, and then delegates the request to Ruby handlers.

See the full BERT-RPC specification at [bert-rpc.org](http://bert-rpc.org).

Ernie currently supports the following BERT-RPC features:

* `call` requests
* `cast` requests

Ernie was developed for GitHub and is currently in production use serving millions of RPC requests every day. The stability and performance have been exemplary.


Installation
------------

You must have Erlang installed before installing Ernie.

    $ gem install ernie -s http://gemcutter.org


Running
-------

    Usage: ernie [command] [options]
        -c, --config CONFIG              Config file
        -p, --port PORT                  Port
        -d, --detached                   Run as a daemon
        -P, --pidfile PIDFILE            Location to write pid file.

    Commands:
      <none>                Start an Ernie server.
      reload-handlers       Gracefully reload all of the the ruby handlers
                            and use the new code for all subsequent requests.
      stats                 Print a list of connection and handler statistics.

    Examples:
      ernie -d -p 9999 -c example.cfg
        Start the ernie server in the background on port 9999 using the
        example.cfg configuration file.

      ernie reload-handlers -p 9999
        Reload the handlers for the ernie server currently running on
        port 9999.


Configuration File
------------------

Ernie configuration files are written as a series of Erlang terms. Each term is a list of 2-tuples that specify options for a set of modules.

The form for native modules is:

    [{modules, Modules},
     {type, native},
     {codepaths, CodePaths}].

Where Modules is a list of atoms corresponding to the module names and
CodePaths is a list of strings representing the file paths that should be
added to the runtime's code path. These paths will be prepended to the code
path and must include the native module's directory and the directories of any
other dependencies.

The form for external modules is:

    [{modules, Modules},
     {type, extern},
     {command, Command},
     {count, Count}].

Where Modules is a list of atoms corresponding to the module names, Command is
a string specifying the command to be executed in order to start a worker, and
Count is the number of workers to spawn.


Example Configuration File
--------------------------

The following example config file informs Ernie of two modules. The first term
identifies a native module 'nat' that resides in a .beam file under the
'/path/to/app/ebin' directory. The second term specifies an external module
'ext' that will have 2 workers started with the command 'ruby
/path/to/app/ernie/ext.rb'.

    [{modules, [nat]},
     {type, native},
     {codepaths, ["/path/to/app/ebin"]}].

    [{modules, [ext]},
     {type, extern},
     {command, "ruby /path/to/app/ernie/ext.rb"},
     {count, 2}].


Example Native (Erlang) Handler
-------------------------------

    -module(nat).
    -export([add/2]).

    add(A, B) ->
      A + B.


Example External (Ruby) Handler
-------------------------------

Using a Ruby module and Ernie.expose:

    require 'ernie'
    
    module Calc
      def add(a, b)
        a + b
      end
    end
    
    Ernie.expose(:calc, Calc)


Logging
-------

You can have logging sent to a file by adding these lines to your handler:

    logfile('/var/log/ernie.log')
    loglevel(Logger::INFO)

This will log startup info, requests, and error messages to the log. Choosing
Logger::DEBUG will include the response (be careful, doing this can generate
very large log files).


Autostart
---------

Normally Ernie handlers will become active after the file has been loaded in.
you can disable this behavior by setting:

    Ernie.auto_start = false


Example BERT-RPC call for above example
---------------------------------------

    -> {call, calc, add, [1, 2]}

    <- {reply, 3}


Using the BERTRPC gem to make calls to Ernie
--------------------------------------------

You can make BERT-RPC calls from Ruby with the [BERTRPC gem](http://github.com/mojombo/bertrpc):

    require 'bertrpc'

    svc = BERTRPC::Service.new('localhost', 8000)
    svc.call.calc.add(1, 2)
    # => 3


Contribute
----------

If you'd like to hack on Ernie, start by forking my repo on GitHub:

    http://github.com/mojombo/ernie

To get all of the dependencies, install the gem first. To run ernie from
source, you must first build the Erlang code:

    rake ebuild

The best way to get your changes merged back into core is as follows:

1. Clone down your fork
1. Create a topic branch to contain your change
1. Hack away
1. Add tests and make sure everything still passes by running `rake`
1. If you are adding new functionality, document it in the README.md
1. Do not change the version number, I will do that on my end
1. If necessary, rebase your commits into logical chunks, without errors
1. Push the branch up to GitHub
1. Send me (mojombo) a pull request for your branch


Copyright
---------

Copyright (c) 2009 Tom Preston-Werner. See LICENSE for details.
