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
        [FormatToken | BoundsTokens] = InputTokens,
        Bounds = lists:map(
            fun (BoundsToken) ->
                case string:prefix(BoundsToken, "~~") of
                    nomatch -> {absolute, erlang:list_to_integer(BoundsToken)};
                    Other ->   {relative, erlang:list_to_integer(Other)}
                end
            end,
            BoundsTokens),
        {Lower, Upper} = case Bounds of
            [U] -> {{absolute, 1}, U};
            [L, U] -> {L, U}
        end,
        Format = case FormatToken of
            "d" -> decimal;
            "b" -> binary;
            "x" -> hex
        end,
        {ok, Format, Lower, Upper}
    catch
        error:_ ->
            {error, "Expected \"m [format] {start} [end]\""}
    end.

format_memory(InterpreterPid, InputTokens) ->
    case parse_memory_args(InputTokens) of
        {ok, Format, Lower, Upper} ->
            State = interpreter:get_state(InterpreterPid),
            Memory = State#bf_state.memory,
            Position = length(Memory#bf_memory.prev_mem) + 1,
            Start = case Lower of
                {absolute, L} -> L;
                {relative, L} -> max(Position + L, 1)
            end,
            End = case Upper of
                {absolute, U} -> U;
                {relative, U} -> max(Position + U, 1)
            end,
            Length = End - Start + 1,
            MemoryArr = lists:sublist(Memory#bf_memory.prev_mem ++ [Memory#bf_memory.current_cell | Memory#bf_memory.next_mem], Start, Length),
            PaddedMem = MemoryArr ++ [<<0>> || _ <- lists:seq(1, Length - length(MemoryArr))],
            {ok, string:join(lists:map(fun (M) -> lists:flatten(string:pad(erlang:integer_to_list(binary:decode_unsigned(M)), 3, leading)) end, PaddedMem), " ")};
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
    