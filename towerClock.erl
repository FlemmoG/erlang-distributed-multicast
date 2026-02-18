-module(towerClock).
-export([init/0, stop/1, run_all_tests/0]).

%---------------------------------------------------------Anwender API----------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

init() ->
	Pid = spawn(fun() -> loop(1) end),
    register(vtKLCclockC,Pid),
	Pid.

stop(PID) -> 
    PID ! {stop},
    ok.

%---------------------------------------------------------Prozess loops---------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

loop(RegisterCount) -> 
    receive
        {getVecID,PID} -> 
            PID ! {vt, RegisterCount},
            loop(RegisterCount + 1);
        {stop} ->
            io:format("Stopped Tower"),
            unregister(vtKLCclockC); % Unregister the process before stopping
        Any -> io:format("Received Unknown: ~p\n", [Any]),
            loop(RegisterCount)
    end.

%---------------------------------------------------------Tests-----------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

run_all_tests() ->
    test_register(),
    ok.

test_register() ->
    Tower = init(),
    Tower ! {getVecID, self()},
    receive 
        {vt, Actual} -> 
            Expected = 1,
            case Actual == Expected of 
                true -> 
                    vsutil:meinSleep(100),
                    SecondExpected = 2,
                    Tower ! {getVecID, self()},
                    receive 
                        {vt, SecondActual} -> 
                            case SecondActual == SecondExpected of
                                true -> io:format("test_register erfolgreich!");
                                false -> io:format("test_register nicht erfolgreich! ~p\n", [SecondActual])
                            end
                    end;
                false -> io:format("test_register nicht erfolgreich! ~p\n", [Actual])
            end
    end,
    Tower ! {stop}.


