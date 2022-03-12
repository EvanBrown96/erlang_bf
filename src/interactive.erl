-module(interactive).
-include("../include/bf_records.hrl").
-export([main/0]).

main() ->
    process_flag(trap_exit, true),
    main_loop().


parse_load_args(InputTokens) ->
    try
        [CellSizeToken, MemSizeToken, Filename] = InputTokens,
        CellSize = erlang:list_to_integer(CellSizeToken),
        MemSize = erlang:list_to_integer(MemSizeToken),
        { ok, CellSize, MemSize, Filename }
    catch
        error:_ -> 
            {error, "Expected \"l [cellsize] [memorysize] [filename]\""}
    end.

load_new_interpreter(InputTokens) ->
    case parse_load_args(InputTokens) of
        {ok, CellSize, MemSize, Filename} ->
            ReadFileAttempt = file:read_file(Filename),
            case ReadFileAttempt of
                {ok, Data} ->
                    TextProgram = erlang:binary_to_list(Data),
                    Interpreter = interpreter:start_link(CellSize, MemSize, TextProgram),
                    {ok, Interpreter};
                {error, _} ->
                    {error, "Could not open file " ++ Filename ++ "\n"}
            end;
        Other -> Other
    end.

parse_memory_args(InputTokens) ->
    try
        [FormatToken, StartToken, LengthToken] = InputTokens,
        Start = case string:prefix(StartToken, "~") of
            nomatch -> {absolute, erlang:list_to_integer(StartToken)};
            Other   -> {relative, erlang:list_to_integer(Other)}
        end,
        Length = erlang:list_to_integer(LengthToken),
        Format = case FormatToken of
            "d" -> decimal;
            "b" -> binary;
            "x" -> hex
        end,
        {ok, Format, Start, Length}
    catch
        error:_ ->
            {error, "Expected \"m [format] [start] [length]\""}
    end.

format_memory(InterpreterPid, InputTokens) ->
    case parse_memory_args(InputTokens) of
        {ok, Format, Start, Length} ->
            State = interpreter:get_state(InterpreterPid),
            Memory = State#bf_state.memory,
            Position = length(Memory#bf_memory.prev_mem) + 1,
            StartPos = case Start of
                {absolute, L} -> L;
                {relative, L} -> max(Position + L, 1)
            end,
            ConcatMem = Memory#bf_memory.prev_mem ++ [Memory#bf_memory.current_cell | Memory#bf_memory.next_mem],
            MemoryArr = case StartPos > length(ConcatMem) of
                true -> [];
                false -> lists:sublist(ConcatMem, StartPos, Length)
            end,
            PaddedMem = MemoryArr ++ [<<0>> || _ <- lists:seq(1, Length - length(MemoryArr))],
            StringedMem = lists:map(fun (M) -> lists:flatten(string:pad(erlang:integer_to_list(binary:decode_unsigned(M)), 3, leading)) end, PaddedMem),
            LinedMem = [ lists:sublist(StringedMem, Line, 16) || Line <- lists:seq(1, length(StringedMem), 16)],
            CellNumbers = [ lists:map(fun (M) -> lists:flatten(string:pad(case M of Position -> "*"; _ -> erlang:integer_to_list(M) end, 3, leading)) end, lists:seq(StartPos + Index * 16 - 16, StartPos + Index * 16 - 17 + length(Line))) || {Index, Line} <- lists:zip(lists:seq(1, length(LinedMem)), LinedMem) ],
            PrintLines = lists:zipwith(fun (Numbers, Values) -> string:join(Numbers, " ") ++ "\n" ++ string:join(Values, " ") end, CellNumbers, LinedMem),
            {ok, string:join(PrintLines, "\n\n")};
        Other -> Other
    end.

parse_step_args(InputTokens) ->
    try
        case InputTokens of
            [] -> {ok, 1};
            [CountToken] -> 
                case erlang:list_to_integer(CountToken) of
                    Count when Count > 0 ->
                        {ok, Count};
                    _ ->
                        {error, "Non-positive step count given"}
                end
        end
    catch
        error:_ ->
            {error, "Expected \"s {count}\""}
    end.

main_loop() ->
    [Cmd | Params] = string:lexemes(io:get_line("> "), " \n"),

    case Cmd of
        "l" ->
            InterpreterAttempt = load_new_interpreter(Params),
            case InterpreterAttempt of
                {ok, InterpreterPid} ->
                    io:format("Loaded New Program.~n"),
                    main_loop(InterpreterPid);
                {error, Error} ->
                    io:format("~s~n", [Error]),
                    main_loop()
            end;
        "e" ->
            io:format("Bye!~n");
        _ ->
            io:format("Unknown command entered!~n"),
            main_loop()
    end.

main_loop(InterpreterPid) ->
    [Cmd | Params] = string:lexemes(io:get_line("> "), " \n"),

    case Cmd of
        "r" ->
            run_loop(InterpreterPid),
            main_loop(InterpreterPid);
        "s" ->
            case parse_step_args(Params) of
                {ok, Count} ->
                    Response = step_count(InterpreterPid, Count),
                    case Response of
                        { input } ->
                            Input = io:get_line("Enter Input: "),
                            interpreter:input(InterpreterPid, Input);
                        { done } ->
                            io:format("Program Completed.~n");
                        { ok } ->
                            ok;
                        { output, Output } ->
                            io:format("Program Output: ~s~n", [Output])
                    end;
                {error, Error} ->
                    io:format("~s~n", [Error])
            end,
            main_loop(InterpreterPid);
        "k" ->
            interpreter:stop(InterpreterPid),
            io:format("Program Terminated.~n"),
            main_loop();
        "o" ->
            interpreter:reset(InterpreterPid),
            io:format("Program Reset.~n"),
            main_loop(InterpreterPid);
        "m" ->
            Response = format_memory(InterpreterPid, Params),
            case Response of
                {ok, MemoryString} ->
                    io:format("~s~n", [MemoryString]);
                {error, Error} ->
                    io:format("~s~n", [Error])
            end,
            main_loop(InterpreterPid);
        "e" ->
            io:format("Bye!~n");
        _ ->
            io:format("Unknown command entered!~n"),
            main_loop(InterpreterPid)
    end.

step_count(InterpreterPid, Count) ->
    Response = interpreter:step(InterpreterPid),
    case { Count, Response } of
        { 1, _ }      -> Response;
        { _, { ok } } -> step_count(InterpreterPid, Count-1);
        _             -> Response
    end.
            

run_loop(InterpreterPid) ->
    Response = interpreter:run(InterpreterPid),
    case Response of
        { done, Output } ->
            io:format("~s~nProgram Completed.~n", [Output]);
        { input, Output } ->
            io:format("~s", [Output]),
            Input = io:get_line(""),
            interpreter:input(InterpreterPid, Input),
            run_loop(InterpreterPid)
    end.
    