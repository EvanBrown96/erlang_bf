-module(interactive).
-export([main/0]).

main() ->
    process_flag(trap_exit, true),
    main_loop().

load_new_interpreter(InputTokens) ->
    try
        [CellSizeToken, MemSizeToken, Filename] = InputTokens,
        CellSize = erlang:list_to_integer(CellSizeToken),
        MemSize = erlang:list_to_integer(MemSizeToken),
        ReadFileAttempt = file:read_file(Filename),
        case ReadFileAttempt of
            {ok, Data} ->
                TextProgram = erlang:binary_to_list(Data),
                Interpreter = interpreter:start_link(CellSize, MemSize, TextProgram),
                {ok, Interpreter};
            {error, _} ->
                io:format("Could not open file ~s~n", Filename),
                undefined
        end
    catch
        _ -> 
            io:format("Expected \"l [cellsize] [memorysize] [filename]\""),
            undefined
    end.

main_loop() ->
    [Cmd | Params] = string:lexemes(io:get_line("> "), " \n"),

    case Cmd of
        "l" ->
            InterpreterAttempt = load_new_interpreter(Params),
            case InterpreterAttempt of
                {ok, InterpreterPid} ->
                    main_loop(InterpreterPid);
                _ ->
                    main_loop()
            end;
        "e" ->
            io:format("Bye!~n");
        _ ->
            io:format("Unknown command entered!~n"),
            main_loop()
    end.

main_loop(InterpreterPid) ->
    [Cmd | Params] = string:lexemes(io:get_line("> "), "\n"),

    case Cmd of
        "r" ->
            interpreter:run(InterpreterPid),
            interpreter:get_state(InterpreterPid);
        _ ->
            io:format("Unknown command entered!~n"),
            main_loop(InterpreterPid)
    end.