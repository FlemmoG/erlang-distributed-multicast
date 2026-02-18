-module(vectorC).
-export([initVT/0, myVTid/1, myVTvc/1, myCount/1, foCount/2, isVT/1, syncVT/2, tickVT/1, compVT/2, aftereqVTJ/2, test_all/0]).

%---------------------------------------------------------Anwender API----------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

initVT() -> 
    case file:consult("towerClock.cfg") of
	{ok, ConfigListeCBC} ->
		io:format("testClock.cfg erfolgreich in ~p gelesen.\n",[pid_to_list(self())]),
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
    Tower = {Servername, Servernode},
    Tower ! {getVecID, self()},
    receive
        {vt, Pnum} -> 
            VectorStamp = init_vector(Pnum),
            {Pnum, VectorStamp}
    end.

myVTid(VT) -> 
    {Pnum, _VectorStamp} = VT,
    Pnum.

myVTvc(VT) ->
    {_Pnum, VectorStamp} = VT,
    VectorStamp.

myCount(VT) -> 
    {Pnum, VectorStamp} = VT,
    nth(VectorStamp, Pnum - 1).

foCount(J, VT) -> 
    {_Pnum, VectorStamp} = VT,
    nth(VectorStamp, J - 1).

isVT(VT) -> 
    case VT of
        {Pnum, VectorStamp} when is_integer(Pnum), Pnum > 0, is_list(VectorStamp) -> 
            true;
        _ -> 
            false
    end.

syncVT(VT1, VT2) -> 
    {OwnPnum, VectorStamp1} = VT1,
    {_OtherPnum, VectorStamp2} = VT2,
    MergedVectorStamp = max_lists(VectorStamp1, VectorStamp2),
    {OwnPnum, MergedVectorStamp}.

tickVT(VT) -> 
    {Pnum, VectorStamp} = VT,
    IncrementedVectorStamp = increment_at(VectorStamp, Pnum-1),
    {Pnum, IncrementedVectorStamp}.

compVT(VT1, VT2) -> 
    {_OwnPnum, VectorStamp1} = VT1,
    {_OtherPnum, VectorStamp2} = VT2,
    StringResult = compare(VectorStamp1,VectorStamp2),
    case StringResult of
        "before" -> beforeVT;
        "after" -> afterVT;
        "equal" -> equalVT;
        "concurrent" -> concurrentVT
    end.

aftereqVTJ(VT,VTR) -> 
    {_OwnPnum, VTStamp} = VT,
    {J, VTRStamp} = VTR,
    NewVTStamp = remove_nth(VTStamp, J - 1),
    NewVTRStamp = remove_nth(VTRStamp, J - 1),
    StringResult = compare(NewVTStamp, NewVTRStamp),
    case StringResult of
        "after" -> {aftereqVTJ, nth(VTStamp, J - 1) - nth(VTRStamp, J - 1)};
        "equal" -> {aftereqVTJ, nth(VTStamp, J - 1) - nth(VTRStamp, J - 1)};
        _ -> false
    end.

%---------------------------------------------------------Hilfsfunktionen-------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

%gibt einen N langen Vektor zurück, welcher mit Nullen initialisiert ist
init_vector(N) when N >= 0 ->
    init_vector(N, []).

init_vector(0, Acc) ->
    Acc;
init_vector(N, Acc) when N > 0 ->
    init_vector(N - 1, [0 | Acc]).

%gibt das Element zurück, welches an nter stelle liegt (wenn index > länge der Liste, dann 0 zurückgeben)
nth(List, N) when N >= 0 ->
    nth(List, N, 0).

nth([], _, _) ->
    0;
nth([H | _], N, N) ->
    H;
nth([_ | T], N, Current) ->
    nth(T, N, Current + 1).

%inkrementiert List and stelle des Index um 1
increment_at(List, Index) ->
    increment_at(List, Index, 0).

increment_at([H | T], Index, Index) ->
    [H + 1 | T];
increment_at([H | T], Index, Current) ->
    [H | increment_at(T, Index, Current + 1)];
increment_at([], _Index, _Current) ->
    [].

remove_nth(List, Index) ->
    remove_nth(List, Index, 0).

remove_nth([_H | T], Index, Index) ->
    T;
remove_nth([H | T], Index, Current) ->
    [H | remove_nth(T, Index, Current + 1)];
remove_nth([], _Index, _Current) ->
    [].

%merged zwei Listen, indem diese Elementweise verglichen werden und jeweils des größere Element genommen wird
%wenn eine Liste kleiner, als die andere ist, dann werden für diese jeweiligen Stellen Nullen angenommen
%die resultierende Liste hat also die Länge der größeren Liste
max_lists(List1, List2) ->
    max_lists(List1, List2, []).

max_lists([], [], Acc) ->
    reverse(Acc);
max_lists([H1 | T1], [], Acc) ->
    max_lists(T1, [], [max_elem(H1, 0) | Acc]);
max_lists([], [H2 | T2], Acc) ->
    max_lists([], T2, [max_elem(0, H2) | Acc]);
max_lists([H1 | T1], [H2 | T2], Acc) ->
    max_lists(T1, T2, [max_elem(H1, H2) | Acc]).

max_elem(X, Y) when X >= Y -> X;
max_elem(_X, Y) -> Y.

reverse(List) ->
    reverse(List, []).

reverse([], Acc) ->
    Acc;
reverse([H | T], Acc) ->
    reverse(T, [H | Acc]).

compare(VT1, VT2) -> 
    compare(VT1, VT2, "equal").

compare([], [], Flag) -> 
    Flag;
compare([H1 | T1], [H2 | T2], Flag) when H1 > H2 -> 
    case Flag of 
        "after" -> compare(T1, T2, "after");
        "equal" -> compare(T1, T2, "after");
        _ -> "concurrent"
    end;
compare([H1 | T1], [H2 | T2], Flag) when H2 > H1 -> 
    case Flag of 
        "before" -> compare(T1, T2, "before");
        "equal" -> compare(T1, T2, "before");
        _ -> "concurrent"
    end;
compare([H1 | T1], [H2 | T2], Flag) when H2 == H1 -> 
    compare(T1, T2, Flag);
compare([H1 | T1], [], Flag) when H1 > 0 -> 
    case Flag of 
        "after" -> compare(T1, [], "after");
        "equal" -> compare(T1, [], "after");
        _ -> "concurrent"
    end;
compare([_H1 | T1], [], Flag) -> 
    compare(T1, [], Flag);
compare([], [H2 | T2], Flag) when H2 > 0 -> 
    case Flag of 
        "before" -> compare(T2, [], "before");
        "equal" -> compare(T2, [], "before");
        _ -> "concurrent"
    end;
compare([], [_H2 | T2], Flag) -> 
    compare(T2, [], Flag).

%---------------------------------------------------------Tests-----------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------------------------------%

test_all() -> 
    test_compare(),
    test_nth(),
    test_remove_nth(),
    test_increment_at(),
    test_max_lists().

test_compare() ->
    Y = [0,0,0,0,0,4],
    XY = [0,0,0,2,0,4],
    ZY = [0,0,0,0,0,4,6],
    XY_ZY = compare(XY,ZY),
    XY_Y = compare(XY, Y),
    Y_ZY = compare(Y, ZY),
    Y_Y = compare(Y,Y),
    if 
        XY_ZY == "concurrent", XY_Y == "after", Y_ZY == "before", Y_Y == "equal" -> io:format("test_compare erfolgreich!\n");
        true -> io:format("test_nth nicht erfolgreich!\n")
    end.

test_nth() -> 
    %    0 1 2 3 4 5
    Y = [0,1,2,3,4,5],
    IndexFirst = nth(Y,0),
    IndexMid = nth(Y,2),
    IndexLast = nth(Y,5),
    IndexOutOfBounds = nth(Y, 6),
    if 
        IndexFirst == 0 , IndexMid == 2, IndexLast == 5, IndexOutOfBounds == 0 -> io:format("test_nth erfolgreich!\n");
        true -> io:format("test_nth nicht erfolgreich!\n")
    end.

test_remove_nth() -> 
    %    0 1 2 3 4 5
    Y = [0,1,2,3,4,5],
    
    Test1 = remove_nth(Y, 0), 
    Test2 = remove_nth(Y, 2), 
    Test3 = remove_nth(Y, 5),
    Test4 = remove_nth(Y, 6), 
    
    Expected1 = [1,2,3,4,5],
    Expected2 = [0,1,3,4,5],
    Expected3 = [0,1,2,3,4],
    Expected4 = [0,1,2,3,4,5],
    
    if 
        Test1 == Expected1,
        Test2 == Expected2,
        Test3 == Expected3,
        Test4 == Expected4 -> 
            io:format("test_remove_nth erfolgreich!\n");
        true -> 
            io:format("test_remove_nth nicht erfolgreich!\n")
    end.

test_increment_at() -> 
    %    0 1 2 3 4 5
    Y = [0,1,2,3,4,5],
    
    Test1 = increment_at(Y, 0), 
    Test2 = increment_at(Y, 2), 
    Test3 = increment_at(Y, 5), 
    Test4 = increment_at(Y, 6), 
    
    Expected1 = [1,1,2,3,4,5],
    Expected2 = [0,1,3,3,4,5],
    Expected3 = [0,1,2,3,4,6],
    Expected4 = [0,1,2,3,4,5],
    
    if 
        Test1 == Expected1,
        Test2 == Expected2,
        Test3 == Expected3,
        Test4 == Expected4 -> 
            io:format("test_increment_at erfolgreich!\n");
        true -> 
            io:format("test_increment_at nicht erfolgreich!\n")
    end.

test_max_lists() -> 
    List1 = [1, 2, 3],
    List2 = [3, 2, 1],
    List3 = [1, 4, 5, 6],
    List4 = [2, 3],
    
    Test1 = max_lists(List1, List2), 
    Test2 = max_lists(List1, List3), 
    Test3 = max_lists(List4, List1), 
    Test4 = max_lists(List3, []),
    
    Expected1 = [3, 2, 3],
    Expected2 = [1, 4, 5, 6],
    Expected3 = [2, 3, 3],
    Expected4 = [1, 4, 5, 6],
    
    if 
        Test1 == Expected1,
        Test2 == Expected2,
        Test3 == Expected3,
        Test4 == Expected4 -> 
            io:format("test_max_lists erfolgreich!\n");
        true -> 
            io:format("test_max_lists nicht erfolgreich!\n")
    end.




        
