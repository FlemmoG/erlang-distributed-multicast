-module(towerCBC).
-export([init/0, init/1, stop/1, reset/1, listall/0, cbcast/2, run_all_tests/0]).

%---------------------------------------------------------Anwender API----------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

init() ->
	init(auto).

init(Mode) ->
    Pid = spawn(fun() -> loop(Mode, [], []) end),
    register(towerKLCcbc, Pid),
    Pid.

stop(PID) -> 
    PID ! {self(), stop},
    receive 
        {stopped} -> true
        after 5000 -> false
    end.

reset(PID) ->
    PID ! {self(), reset},
    receive 
        {reseted} -> true
        after 500 -> false
    end.

listall() -> 
    towerKLCcbc ! {self(), listall},
    receive 
        {listed, RegisteredComms} -> 
            log_all(RegisteredComms),
            true
        after 500 -> false
    end.

cbcast(ReceiverNumber, MessageNumber) -> 
    towerKLCcbc ! {self(), {multicastM, ReceiverNumber, MessageNumber}},
    receive 
        {ok, multicast} -> 
            true;
        {nok, multicast} -> 
            false
        after 500 -> 
            false
    end.

%---------------------------------------------------------Hilfsfunktionen-------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

log_all(RegisteredComms) ->
    LogContent = format_log_content(RegisteredComms),
    Prefix = "Registrierte Kommunikationseinheiten:\n",
    {ok, Hostname} = inet:gethostname(),
    LogFilename = "towerCBC@" ++ Hostname ++ ".log",
    util:logging(LogFilename, Prefix ++ LogContent).

format_log_content([]) -> "";
format_log_content([Comm | Rest]) ->
    CommsStr = format_communication_unit(Comm),
    RestStr = format_log_content(Rest),
    CommsStr ++ RestStr.

format_communication_unit(Comms) ->
    io_lib:format("~p~n", [Comms]).

nth(List, N) when N >= 0 ->
    nth(List, N, 0).

nth([], _, _) ->
    null;
nth([H | _], N, N) ->
    H;
nth([_ | T], N, Current) ->
    nth(T, N, Current + 1).

in_list(_, []) -> false;
in_list(H, [H | _]) -> true;
in_list(H, [_ | T]) -> in_list(H, T).

multicast([], _Message) -> 
    ok;
multicast([PID | T], Message) ->
    PID ! {self(), {castMessage, Message}},
    multicast(T, Message).

%---------------------------------------------------------Prozess loops---------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

loop(Mode, RegisteredComms, Messages) -> 
    receive
        {PID, stop} ->
            unregister(towerKLCcbc),
            PID ! {stopped};
        {PID, reset} -> 
            PID ! {reseted},
            loop(Mode, [], []);
        {PID, listall} -> 
            PID ! {listed, RegisteredComms},
            loop(Mode, RegisteredComms, Messages);
        {PID ,{register, RPID}} -> 
            case in_list(RPID, RegisteredComms) of
                true -> 
                    PID ! {replycbc, ok_existing},
                    loop(Mode, RegisteredComms, Messages);
                false -> 
                    RegisteredCommsUpdated = RegisteredComms ++ [RPID],
                    PID ! {replycbc, ok_registered},
                    loop(Mode, RegisteredCommsUpdated, Messages)
            end;
        {_PID,{multicastB,Message}} ->
            case Mode of
                manu -> 
                    MessagesUpdated = Messages ++ [Message],
                    loop(Mode, RegisteredComms, MessagesUpdated);
                auto ->
                    multicast(RegisteredComms, Message),
                    loop(Mode, RegisteredComms, Messages)
            end;
        {_PID,{multicastNB,Message}} ->
            case Mode of
                manu -> 
                    MessagesUpdated = Messages ++ [Message],
                    loop(Mode, RegisteredComms, MessagesUpdated);
                auto -> 
                    spawn(fun() -> multicast(RegisteredComms, Message) end),
                    loop(Mode, RegisteredComms, Messages)
            end;
        {PID, {multicastM, ReceiverNumber, MessageNumber}} -> 
            case Mode of
                manu -> 
                    Receiver = nth(RegisteredComms ,ReceiverNumber - 1),
                    Message = nth(Messages, MessageNumber - 1),
                    if 
                        Receiver =/= null andalso Message =/= null -> 
                            multicast([Receiver], Message),
                            PID ! {ok, multicast},
                            loop(Mode, RegisteredComms, Messages);
                        true -> 
                            PID ! {nok, multicast},
                            loop(Mode, RegisteredComms, Messages)
                    end;
                auto -> 
                    io:format("Tower is in auto mode and will ignore manual cast requests\n"),
                    PID ! {nok, multicast},
                    loop(Mode, RegisteredComms, Messages)
            end;
        kill -> 
            io:format("Kill\n"),
            ok;
        Any -> io:format("Received Unknown: ~p\n", [Any]),
            loop(Mode, RegisteredComms, Messages)
    end.

%---------------------------------------------------------Tests-----------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

run_all_tests() ->
    test_register(),
    test_cbcast(),
    ok.

test_register() ->
    Tower = init(manu),
    FirstPID = spawn(fun() -> Tower ! {self(), {register, self()}} end),
    vsutil:meinSleep(100),
    SecondPID = spawn(fun() -> Tower ! {self(), {register, self()}} end),
    vsutil:meinSleep(100),
    ThirdPID = spawn(fun() -> Tower ! {self(), {register, self()}} end),
    vsutil:meinSleep(100),
    Expected = [FirstPID, SecondPID, ThirdPID],
    Tower ! {self(), listall},
    receive 
        {listed, Actual} -> 
            case Expected == Actual of
                true -> io:format("test_register erfolgreich!\n");
                false -> io:format("test_register nicht erfolgreich!\n")
            end
    end,
    stop(Tower).
        
test_cbcast() ->
    Tower = init(manu),
    spawn(fun() -> Tower ! {self(), {register, self()}} end),
    vsutil:meinSleep(100),
    spawn(fun() -> Tower ! {self(), {register, self()}} end),
    vsutil:meinSleep(100),
    spawn(fun() -> Tower ! {self(), {register, self()}} end),
    vsutil:meinSleep(100),
    Tower ! {self(), {multicastNB, {message1, "Hello"}}},
    vsutil:meinSleep(100),
    Tower ! {self(), {multicastNB, {message2, "World"}}},
    vsutil:meinSleep(100),
    Result1 = cbcast(1, 1),
    Result2 = cbcast(2, 2),
    Result3 = cbcast(3, 2),
    Result4 = cbcast(4, 1),
    case {Result1, Result2, Result3, Result4} of
        {true, true, true, false} -> 
            io:format("test_cbcast erfolgreich!\n");
        _ -> 
            io:format("test_cbcast nicht erfolgreich!\n")
    end,
    stop(Tower).
       










