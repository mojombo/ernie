Ernie
=====

By Tom Preston-Werner (tom@mojombo.com)

WARNING: This software is alpha and should not be used in production without
extensive testing. You should not consider this project production ready until
it is released as 1.0.


Description
-----------

Ernie is a BERT-RPC server implementation that uses an Erlang server to accept incoming connections, and then delegates the request to a Ruby handler via Erlectricity.


Installation
------------

  You must have Erlang installed before installing Ernie.

    gem install mojombo-ernie -s http://gems.github.com


Running
-------

    Usage: ernie [options] <handler>
        -n, --name NAME                  Node name
        -p, --port PORT                  Port
        -d, --detached                   Run as a daemon
        -P, --pidfile PIDFILE            Location to write pid file.


Example Handler
---------------

    require 'ernie'
    
    mod(:calc) do
      fun(:add) do |a, b|
        a + b
      end
    end


Example BERT-RPC call for above example
---------------------------------------

    -> {call, calc, add, [1, 2]}
    
    <- {reply, 3}


Using the BERTRPC gem to make calls to Ernie
--------------------------------------------

You can make BERT-RPC calls from Ruby with the [BERTRPC gem](http://github.com/mojombo/bertrpc):

    require 'bertrpc'
    
    svc = BERTRPC::Service.new('localhost', 8000)
    svc.calc.add.call(1, 2)
    # => 3


Contribute
----------

If you'd like to hack on Ernie, start by forking my repo on GitHub:

http://github.com/mojombo/ernie

To get all of the dependencies, install the gem first. The best way to get
your changes merged back into core is as follows:

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