-record(state, {lsock = undefined,      % the listen socket
                listen = true,          % whether to listen for new connections
                hq = queue:new(),       % high priority queue
                lq = queue:new(),       % low priority queue
                count = 0,              % total request count
                zcount = 0,             % total completed request count
                map = undefined}).      % module map. tuples of {Mod, Id}

-record(request, {sock = undefined,     % connection socket
                  log = undefined,      % log information
                  infos = [],           % list of info binaries
                  action = undefined,   % action binary
                  priority = high}).    % priority [ high | low ]

-record(log, {taccept = erlang:now(),   % time that connection was accepted
              tprocess = erlang:now(),  % time that processing started
              tdone = erlang:now(),     % time that processing and response is done
              hq = 0,                   % size of high queue at acceptance
              lq = 0,                   % size of low queue at acceptance
              type = unk}).             % type [ unk | nat | ext ]