-module(fileReplicationServer).
-export([start/0, stop/1, write/2, read/1]).

start() ->
    Pid = cbCast:init(),
    LoopPid = spawn(fun() -> loop(Pid, []) end),
    register(repServer, LoopPid),
    Pid.

stop(Pid) ->
    cbCast:stop(Pid).

write(Pid, Data) ->
    cbCast:send(Pid, {write, Data}).

read(Pid) ->
    cbCast:received(Pid).

loop(Pid, FileContent) ->
    receive
        {_, {castMessage, {write, Data}}} ->
            NewFileContent = FileContent ++ [Data],
            loop(Pid, NewFileContent);
        {_, read} ->
            self() ! {ok, FileContent},
            loop(Pid, FileContent);
        stop ->
            ok
    end.
