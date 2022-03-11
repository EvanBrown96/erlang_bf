-module(interpreter).
-include("../include/bf_records.hrl").
-export([init/3, start/3, start_link/3, step/1, run/1, stop/1, get_state/1, input/2, reset/1]).

init( CellSize, MemorySize, TextProgram ) ->
    Interpreter = #interpreter{ cell_size = CellSize, memory = MemorySize },
    loop(Interpreter, bf:default_state(Interpreter, TextProgram), [] ).


loop( Interpreter, State, BufferedInput ) ->
    receive
        { FromPid, Ref, step } ->
            NewState = bf:step(Interpreter, State),
            Status = NewState#bf_state.status,
            case Status of
                ok ->
                    FromPid ! { Ref, ok },
                    loop(Interpreter, NewState, BufferedInput);
                done ->
                    FromPid ! { Ref, done },
                    loop(Interpreter, NewState, []);
                input ->
                    case BufferedInput of
                        [] ->
                            FromPid ! { Ref, input },
                            loop(Interpreter, NewState, []);
                        [FirstChar | Rest ] ->
                            FromPid ! { Ref, ok },
                            NewState2 = bf:got_input(NewState, [FirstChar]),
                            loop(Interpreter, NewState2, Rest)
                    end;
                { output, OutputChar } ->
                    FromPid ! { Ref, output, OutputChar },
                    NewState2 = bf:performed_output(NewState),
                    loop(Interpreter, NewState2, BufferedInput)
            end;
        { FromPid, Ref, run } ->
            { NewState2, BufferedOutput } = run_until_input_or_done(Interpreter, State, BufferedInput, []),
            FromPid ! { Ref, output, BufferedOutput },
            NewState = NewState2#bf_state.status,
            case NewState of
                done ->
                    FromPid ! { Ref, done },
                    loop(Interpreter, NewState2, []);
                input ->
                    FromPid ! { Ref, input },
                    loop(Interpreter, NewState2, [])
                end;
        { FromPid, Ref, stop } ->
            FromPid ! { Ref, stopped };
        { FromPid, Ref, reset } ->
            FromPid ! { Ref, reset },
            loop(Interpreter, bf:reset_state(Interpreter, State), []);
        { FromPid, Ref, get_state } ->
            FromPid ! { Ref, state, State },
            loop(Interpreter, State, BufferedInput);
        { FromPid, Ref, input, [ FirstChar | Rest ] } -> 
            FromPid ! { Ref, ok },
            NewState = bf:got_input(State, [FirstChar]),
            loop(Interpreter, NewState, Rest);
        Unknown ->
            io:format("Unknown message passed: ~p~n", Unknown)
    end.

run_until_input_or_done( Interpreter, State, BufferedInput, BufferedOutput) ->
    Status = State#bf_state.status,
    case Status of
        ok ->
            % regular operation - step program and keep running
            NewState = bf:step(Interpreter, State),
            run_until_input_or_done(Interpreter, NewState, BufferedInput, BufferedOutput);
        done ->
            % program completed, return current state and any output waiting
            { State, BufferedOutput };
        input ->
            case BufferedInput of
                % no buffered input characters - return to wait for input from client
                [] -> { State, BufferedOutput };
                % if there is a buffered character, update memory and keep running
                [FirstChar | Rest] ->
                    NewState = bf:got_input(State, [FirstChar]),
                    run_until_input_or_done(Interpreter, NewState, Rest, BufferedOutput)
            end;
        { output, OutputChar } ->
            % add output character to the buffer and keep running
            NewState = bf:performed_output(State),
            run_until_input_or_done(Interpreter, NewState, BufferedInput, BufferedOutput ++ OutputChar)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start( CellSize, MemorySize, TextProgram ) ->
    spawn(?MODULE, init, [CellSize, MemorySize, TextProgram]).

start_link( CellSize, MemorySize, TextProgram ) ->
    spawn_link(?MODULE, init, [CellSize, MemorySize, TextProgram]).

step( Pid ) ->
    Ref = make_ref(),
    Pid ! { self(), Ref, step },
    receive
        { Ref, Status } ->
            { Status };
        { Ref, output, Output } ->
            { output, Output }
    end.

run( Pid ) ->
    Ref = make_ref(),
    Pid ! { self(), Ref, run },
    receive
        { Ref, output, OutputString } ->
            receive
                { Ref, done } ->
                    { done, OutputString };
                { Ref, input } ->
                    { input, OutputString }
            end
    end.

stop( Pid ) ->
    Ref = make_ref(),
    Pid ! { self(), Ref, stop },
    receive
        { Ref, stopped } ->
            ok
    end.

get_state( Pid ) ->
    Ref = make_ref(),
    Pid ! { self(), Ref, get_state },
    receive
        { Ref, state, State } ->
            State
    end.

input( Pid, InputString ) ->
    Ref = make_ref(),
    Pid ! { self(), Ref, input, InputString },
    receive
        { Ref, ok } ->
            ok
    end.

reset( Pid ) ->
    Ref = make_ref(),
    Pid ! { self(), Ref, reset },
    receive
        { Ref, reset } ->
            ok
    end.
