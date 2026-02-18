-module(cbCast).
-export([init/0, stop/1, send/2, received/1, read/1, add_to_hbq/2]).


%-----------------------------------------------------Anwender API--------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

init() ->
	case file:consult("towerCBC.cfg") of
	{ok, ConfigListeCBC} ->
        ok;
   {error, {Line, Mod, Term}} ->
        io:format("towerCBC.cfg Fehler in Zeile ~p Mod ~p Term ~p\n",
        [util:to_String(Line),util:to_String(Mod),util:to_String(Term)]),
        ConfigListeCBC = [];
    {error, Something} ->
        io:format("towerCBC.cfg Fehler: ~p\n",[util:to_String(Something)]),
        ConfigListeCBC = [];
    _ ->
        io:format("towerCBC.cfg unspezifizierter Fehler\n"),
        ConfigListeCBC = []
    end,
	{ok, Servername} = vsutil:get_config_value(servername, ConfigListeCBC),
	{ok, Servernode} = vsutil:get_config_value(servernode, ConfigListeCBC),
	
	MyVT = vectorC:initVT(),
	Pid = spawn(fun() -> loop({Servername,Servernode}, [],[], MyVT) end),
	{Servername, Servernode} ! {self(), {register, Pid}},
	receive
		{replycbc, ok_registered} -> io:format("Registriert");
		{replycbc, ok_existing} -> io:format("Bereits registriert")
		after 5000 -> io:format("Failed to register at tower")
	end,

	Pid.

stop(Comm) ->
	Comm ! {self(), stop},
	receive
		ok -> done
	after 5000 -> nok
	end.

send(Comm, Message) ->
	Comm ! {self(), send, Message},
	receive
		ok -> ok
	after 5000 -> nok
	end.

received(Comm) -> 
	Comm ! {self(),received},
	receive
		{ok, Message} -> 
			Message
	end.

read(Comm) -> 
	Comm ! {self(),read},
	receive
		{ok, Message} ->
			Message
		after 50000 -> nok
	end.

%---------------------------------------------------------Hilfsfunktionen-------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

add_to_hbq(HBQ, {Message, MessageVT}) ->
    [{Message, MessageVT}] ++ HBQ.

add_to_dlq(DLQ, {Message, MessageVT}) -> 
	DLQ ++ [{Message, MessageVT}].

dequeue([]) -> 
    {null, []};
dequeue([H|T]) -> 
    {H, T}.

is_empty(List) ->
    List == [].

can_be_delivered(MyVT, MessageVT) -> 
	AfterEQVTJ = vectorC:aftereqVTJ(MyVT, MessageVT),
	case AfterEQVTJ of
		{aftereqVTJ, -1} -> 
			true;
		_ -> 
			false
	end.

update_queues([], DLQ, _VT) ->
    {[], DLQ};
update_queues([H = {_, MessageVT} | T], DLQ, VT) ->
    {NewHBQ, NewDLQ} = update_queues(T, DLQ, VT),
    IsDeliverable = can_be_delivered(VT, MessageVT),
    if
        IsDeliverable -> {NewHBQ, add_to_dlq(NewDLQ, H)};
        true -> {[H | NewHBQ], NewDLQ}
    end.

%---------------------------------------------------------Prozessloops----------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
loop(Tower, HBQ, DLQ, MyVT) ->
     receive
		{PID, send, Message} -> 
			MyNewVT = vectorC:tickVT(MyVT),
			NewDLQ = add_to_dlq(DLQ, {Message, MyNewVT}),
			Tower ! {self(), {multicastNB, {Message, MyNewVT}}},
			PID ! ok,
			loop(Tower, HBQ, NewDLQ, MyNewVT);	
		{PID, received} -> 
			case is_empty(DLQ) of
				true ->
					{NewHBQ, {Message, MessageVT}} = received_loop(HBQ, MyVT),
					PID ! {ok, Message},
					MyNewVT = vectorC:syncVT(MyVT, MessageVT),
					{UpdatedHBQ, UpdatedDLQ} = update_queues(NewHBQ, DLQ, MyNewVT),
					loop(Tower, UpdatedHBQ, UpdatedDLQ, MyNewVT);
				false -> 
					{{Message, MessageVT}, NewDLQ} = dequeue(DLQ),
					PID ! {ok, Message},
					MyNewVT = vectorC:syncVT(MyVT, MessageVT),
					{UpdatedHBQ, UpdatedDLQ} = update_queues(HBQ, NewDLQ, MyNewVT),
					loop(Tower, UpdatedHBQ, UpdatedDLQ, MyNewVT)
			end;
		{PID, read} -> 
			case is_empty(DLQ) of
				true ->
					PID ! {ok, null},
					loop(Tower, HBQ, DLQ, MyVT);
				false -> 
					{{Message, MessageVT}, NewDLQ} = dequeue(DLQ),
					PID ! {ok, Message},
					MyNewVT = vectorC:syncVT(MyVT, MessageVT),
					{UpdatedHBQ, UpdatedDLQ} = update_queues(HBQ, NewDLQ, MyNewVT),
					loop(Tower, UpdatedHBQ, UpdatedDLQ, MyNewVT)
			end;
		{PID, stop} -> 
			PID ! ok;
		{PID,{castMessage, {Message, MessageVT}}} when is_pid(PID) ->
			MessageVTID = vectorC:myVTid(MessageVT),
			MyVTID = vectorC:myVTid(MyVT),
			CanBeDelivered = can_be_delivered(MyVT, MessageVT),
            if
                % Wenn die Nachricht aus eigenem Comm-Modul stammt, dann nichts machen
                MessageVTID == MyVTID ->
					loop(Tower, HBQ, DLQ, MyVT);
				% nicht aus eigenem Comm-Modul und lieferbar -> direkt in DLQ
                CanBeDelivered ->
                    NewDLQ = add_to_dlq(DLQ, {Message, MessageVT}),
					loop(Tower, HBQ, NewDLQ, MyVT);
                % nicht aus eigenem Comm-Modul, aber noch nicht auslieferbar -> in HBQ
                true ->
                    NewHBQ = add_to_hbq(HBQ, {Message, MessageVT}),
					loop(Tower, NewHBQ, DLQ, MyVT)
            end;
	    Any -> io:format("Received Unknown: ~p\n", [Any]),
                    loop(Tower, HBQ, DLQ, MyVT)
     end.

received_loop(HBQ, MyVT) -> 
	receive
		{_PID, {castMessage, {Message, MessageVT}}} ->
            case can_be_delivered(MyVT, MessageVT) of
                true -> 
					{HBQ, {Message, MessageVT}};
                false -> 
					NewHBQ = add_to_hbq(HBQ, {Message, MessageVT}),
					received_loop(NewHBQ, MyVT)
        	end
	end.


