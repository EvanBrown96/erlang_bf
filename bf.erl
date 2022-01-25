-module(bf).
-export([interpreter/4, run/2]).
-compile(export_all).

%% create an interpreter with the given parameters
%% CellSize - integer, number of bits per cell
%% MemorySize - integer or 'unlimited', number of cells available in memory
%% Input - function/0 which returns a single character
%% Output - function/1 which takes a single character
interpreter( CellSize, MemorySize, Input, Output ) ->
    {interpreter, CellSize, MemorySize, Input, Output}.

run( Interpreter, TextProgram ) ->
    step(Interpreter, new_program(TextProgram), startState(Interpreter)).

startState( { interpreter, CellSize, _, _, _} ) ->
    { bf_state, [], <<0:CellSize>>, [] }.

new_program( [First | Rest] ) ->
    { bf_prog, [First], Rest, []}.

advance_program( { bf_prog, [Command], [], Executed } ) ->
    { bf_prog, fin, [], [Command | Executed] };
advance_program( { bf_prog, [Command], [Next | Todo], Executed } ) ->
    { bf_prog, [Next], Todo, [Command | Executed] }.

jump_forward( Program ) ->
    jump_forward( 1, advance_program(Program) ).
jump_forward( 0, Program ) ->
    Program;
jump_forward( Count, Program = { bf_prog, "]", _, _ } ) ->
    jump_forward( Count-1, advance_program(Program) );
jump_forward( Count, Program = { bf_prog, "[", _, _ } ) ->
    jump_forward( Count+1, advance_program(Program) );
jump_forward( Count, Program ) ->
    jump_forward( Count, advance_program(Program) ).

undo_program( { bf_prog, [Command], Todo, [Prev | Executed] } ) ->
    { bf_prog, [Prev], [Command | Todo], Executed }.

jump_backward( Program ) ->
    jump_backward( 1, undo_program(Program) ).
jump_backward( 0, Program ) ->
    advance_program(advance_program(Program));
jump_backward( Count, Program = { bf_prog, "]", _, _ } ) ->
    jump_backward( Count+1, undo_program(Program) );
jump_backward( Count, Program = { bf_prog, "[", _, _ } ) ->
    jump_backward( Count-1, undo_program(Program) );
jump_backward( Count, Program ) ->
    jump_backward( Count, undo_program(Program) ).

step( _, { bf_prog, fin, _, _ }, State ) ->
    State;

step( Interpreter = { interpreter, CellSize, MemorySize, _, _ }
    , Program = { bf_prog, ">", _, _ }
    , _State = { bf_state, PrevMem, CellValue, NextMem }
) ->
    [NewCell | UpdatedMem] = case NextMem of
        [] ->
            case length(PrevMem) of
                Length when Length >= (MemorySize - 1) ->
                    erlang:exit(memory_exceeded);
                _ -> [<<0:CellSize>>]
            end;
        Other -> Other
    end,
    step(Interpreter, advance_program(Program), { bf_state, [CellValue | PrevMem], NewCell, UpdatedMem });

step( Interpreter = { interpreter, _, _, _, _ }
    , Program = { bf_prog, "<", _, _ }
    , _State = { bf_state, PrevMem, CellValue, NextMem }
) ->        
    [NewCell | UpdatedMem] = case PrevMem of
        [] ->
            erlang:exit(memory_exceeded);
        Other -> Other
    end,
    step(Interpreter, advance_program(Program), { bf_state, UpdatedMem, NewCell, [ CellValue | NextMem ]});

step( Interpreter = {interpreter, _, _, _, _ }
    , Program = { bf_prog, "+", _, _ }
    , _State = { bf_state, PrevMem, <<CellInt>>, NextMem }
) ->
    step(Interpreter, advance_program(Program), { bf_state, PrevMem, <<(CellInt + 1)>>, NextMem });

step( Interpreter = {interpreter, _, _, _, _ }
    , Program = { bf_prog, "-", _, _ }
    , _State = { bf_state, PrevMem, <<CellInt>>, NextMem }
) ->
    step(Interpreter, advance_program(Program), { bf_state, PrevMem, <<(CellInt - 1)>>, NextMem });

step( Interpreter
    , Program = { bf_prog, "[", _, _ }
    , State = { bf_state, _, <<CellInt>>, _ }
) ->
    case CellInt of
        0 -> step(Interpreter, jump_forward(Program), State);
        _ -> step(Interpreter, advance_program(Program), State)
    end;

step( Interpreter
    , Program = { bf_prog, "]", _, _ }
    , State = { bf_state, _, <<CellInt>>, _ }
) ->
    case CellInt of
        0 -> step(Interpreter, advance_program(Program), State);
        _ -> step(Interpreter, jump_backward(Program), State)
    end;

step( Interpreter = {interpreter, _, _, Input, _ }
    , Program = { bf_prog, ",", _, _ }
    , _State = { bf_state, PrevMem, _, NextMem }
) ->
    InputChar = io:get_chars(Input, "", 1),
    step(Interpreter, advance_program(Program), { bf_state, PrevMem, erlang:list_to_binary(InputChar), NextMem });

step( Interpreter = {interpreter, _, _, _, Output }
    , Program = { bf_prog, ".", _, _ }
    , State = { bf_state, _, CellValue, _ }
) ->
    io:write(Output, erlang:binary_to_atom(CellValue, latin1)),
    step(Interpreter, advance_program(Program), State).