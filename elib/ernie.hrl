-record(state, {lsock = undefined,      % the listen socket
                hq = queue:new(),       % high priority queue
                lq = queue:new(),       % low priority queue
                count = 0,              % total request count
                map = undefined}).      % module map. tuples of {Mod, Id}

-record(request, {sock = undefined,     % connection socket
                  infos = [],           % list of info binaries
                  action = undefined,   % action binary
                  priority = high}).    % priority [ high | low ]