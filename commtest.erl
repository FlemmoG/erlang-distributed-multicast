-module(commtest).
-export([init/0]).

init() ->
	case file:consult("towerCBC.cfg") of
	{ok, ConfigListeCBC} ->
		io:format("testCBC.cfg erfolgreich in ~p gelesen.\n",[pid_to_list(self())]),
        ok;
   {error, {Line, Mod, Term}} ->
        io:format("testCBC.cfg Fehler in Zeile ~p Mod ~p Term ~p\n",
        [util:to_String(Line),util:to_String(Mod),util:to_String(Term)]),
        ConfigListeCBC = [];
    {error, Something} ->
        io:format("testCBC.cfg Fehler: ~p\n",[util:to_String(Something)]),
        ConfigListeCBC = [];
    _ ->
        io:format("testCBC.cfg unspezifizierter Fehler\n"),
        ConfigListeCBC = []
    end,
	{ok, Servername} = vsutil:get_config_value(servername, ConfigListeCBC),
	{ok, Servernode} = vsutil:get_config_value(servernode, ConfigListeCBC),
	Pid = spawn(fun() -> loop() end),
    register(cbCast,Pid),
	{Servername, Servernode} ! {self(), {register, Pid}},

	Pid.

loop() -> 
    receive
        Any -> io:format("Received Unknown: ~p\n", [Any]),
            loop()
    end.