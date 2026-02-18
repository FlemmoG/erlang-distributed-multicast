-module(fileReplicationClient).
-export([start/1, write/2, read/1]).

start(ServerPid) ->
    spawn(fun() -> loop(ServerPid) end).

write(ClientPid, Data) ->
    ClientPid ! {write, Data}.

read(ClientPid) ->
    ClientPid ! read.

loop(ServerPid) ->
    receive
        {write, Data} ->
            file_replication_server:write(ServerPid, Data),
            loop(ServerPid);
        read ->
            ServerPid ! {self(), read},
            receive
                {ok, FileContent} ->
                    io:format("Current File Content: ~p~n", [FileContent])
            end,
            loop(ServerPid)
    end.
