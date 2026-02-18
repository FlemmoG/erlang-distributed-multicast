-module(colabDoc).
-export([init/0, append/2, delete/1, view_content/1]).


init() ->
    CommUnit = cbCast:init(),
    AnwenderPid = spawn(fun() -> loop(CommUnit, []) end),
    AnwenderPid.

append(AnwenderPid, Message) -> 
    AnwenderPid ! {append, Message}.

delete(AnwenderPid) -> 
    AnwenderPid ! {delete}.


view_content(AnwenderPid) -> 
    AnwenderPid ! {self(), view_content},
    receive
        FileContent -> 
            io:format("\n\nUpdated File Contens:\n\n"),
            io:format(FileContent),
            io:format("\n\n")
    end.



loop(CommUnit, FileContent) -> 
    receive
        {append, Message} -> 
            cbCast:send(CommUnit, {append, Message}),
            loop(CommUnit, FileContent);
        {delete} -> 
            cbCast:send(CommUnit, {delete}),
            loop(CommUnit, FileContent);
        {From, view_content} -> 
            From ! FileContent,
            loop(CommUnit, FileContent)          
        after 500 -> 
            case cbCast:read(CommUnit) of
                null ->
                    loop(CommUnit, FileContent);
                {delete} -> 
                    UpdatedFileContent = delete_last(FileContent),
                    io:format("\n\nUpdated File Contens:\n\n"),
                    io:format(UpdatedFileContent),
                    io:format("\n\n"),
                    loop(CommUnit, UpdatedFileContent);
                {append, Message} -> 
                    UpdatedFileContent = FileContent ++ [Message],
                    io:format("\n\nUpdated File Contens:\n\n"),
                    io:format(UpdatedFileContent),
                    io:format("\n\n"),
                    loop(CommUnit, UpdatedFileContent)
            end

    end.

delete_last(List) when is_list(List) ->
    case List of
        [] -> [];  % Wenn die Liste leer ist, bleibt sie leer.
        _ -> lists:reverse(tl(lists:reverse(List)))
    end.



