-module(bf).
-export([interpreter/4, run/2]).
-include("../include/bf_records.hrl").

%% create an interpreter with the given parameters
%% CellSize - integer, number of bits per cell
%% MemorySize - integer or 'unlimited', number of cells available in memory
%% Input - function/0 which returns a single character
%% Output - function/1 which takes a single character
interpreter( CellSize, MemorySize, Input, Output ) ->
    #interpreter{ cell_size = CellSize
                , memory    = MemorySize
                , input     = Input
                , output    = Output 
                }.

%% run given brainfuck program (as string)
%% Interpreter - parameters to use in run (memory info, io streams)
%% TextProgram - brainfuck program as a string
run( Interpreter, TextProgram ) ->
    run(Interpreter, new_program(TextProgram), start_state(Interpreter)).

run( _, #bf_prog{ command = fin }, State ) ->
    State;
run( Interpreter, Program, State ) ->
    { UpdatedProgram, UpdatedState } = step(Interpreter, Program, State),
    run(Interpreter, UpdatedProgram, UpdatedState).

%% get default initial state to run a program over
%% ie. empty memory, pointer at cell 0
%% Interpreter - initialize memory using interpreter memory parameters
start_state( #interpreter{ cell_size = CellSize } ) ->
    #bf_state{ prev_mem     = []
             , current_cell = <<0:CellSize>>
             , next_mem     = [] 
             }.

%% convert a string program into a Program representation
%% Program - the program to use
new_program( [First | Rest] ) ->
    #bf_prog{ command  = [First]
            , todo     = Rest
            , executed = []
            }.

%% move program to next command
%% Program - the program to advance by one command
advance_program( #bf_prog{ command = [Command], todo = [], executed = Executed } ) ->
    #bf_prog{ command = fin, todo = [], executed = [Command | Executed] };
advance_program(#bf_prog{ command = [Command], todo = [Next | Todo], executed = Executed } ) ->
    #bf_prog{ command = [Next], todo = Todo, executed = [Command | Executed] }.

%% when encountered an open bracket with break condition met
%% advance program to after the corresponding close bracket
%% Program - the program to jump in
jump_forward( Program = #bf_prog{ command = "[" } ) ->
    jump_forward( 1, advance_program(Program) ).

jump_forward( 0, Program ) ->
    Program;
jump_forward( Count, Program = #bf_prog{ command = "]" } ) ->
    jump_forward( Count-1, advance_program(Program) );
jump_forward( Count, Program = #bf_prog{ command = "[" } ) ->
    jump_forward( Count+1, advance_program(Program) );
jump_forward( Count, Program ) ->
    jump_forward( Count, advance_program(Program) ).

%% move program back one command
%% Program - the program to move back in
undo_program( #bf_prog{ command = [Command], todo = Todo, executed = [Prev | Executed] } ) ->
    #bf_prog{ command = [Prev], todo = [Command | Todo], executed = Executed }.

%% when encountered a closed bracket with loop condition met
%% move program back to directly after the corresponding open bracket
%% Program - the program to jump in
jump_backward( Program ) ->
    jump_backward( 1, undo_program(Program) ).

jump_backward( 0, Program ) ->
    advance_program(advance_program(Program));
jump_backward( Count, Program = #bf_prog{ command = "]" } ) ->
    jump_backward( Count+1, undo_program(Program) );
jump_backward( Count, Program = #bf_prog{ command = "[" } ) ->
    jump_backward( Count-1, undo_program(Program) );
jump_backward( Count, Program ) ->
    jump_backward( Count, undo_program(Program) ).


%% perform logic of updating program representation and program state
%% based on the current command
step( _, Program = #bf_prog{ command = fin }, State ) ->
    { Program, State };

step( _Interpreter = #interpreter{ cell_size = CellSize, memory = MemorySize }
    , Program = #bf_prog{ command = ">" }
    , _State = #bf_state{ prev_mem = PrevMem, current_cell = CellValue, next_mem = NextMem }
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
    { advance_program(Program), #bf_state{ prev_mem = [CellValue | PrevMem], current_cell = NewCell, next_mem = UpdatedMem } };

step( _
    , Program = #bf_prog{ command = "<" }
    , _State = #bf_state{ prev_mem = PrevMem, current_cell = CellValue, next_mem = NextMem }
) ->        
    [NewCell | UpdatedMem] = case PrevMem of
        [] ->
            erlang:exit(memory_exceeded);
        Other -> Other
    end,
    { advance_program(Program), #bf_state{ prev_mem = UpdatedMem, current_cell = NewCell, next_mem = [ CellValue | NextMem ] } };

step( _Interpreter = #interpreter{ cell_size = CellSize }
    , Program = #bf_prog{ command = "+" }
    , State = #bf_state{ current_cell = CellValue }
) ->
    <<CellInt:CellSize>> = CellValue,
    { advance_program(Program), State#bf_state{ current_cell = <<(CellInt + 1):CellSize>> } };

step( _Interpreter = #interpreter{ cell_size = CellSize }
    , Program = #bf_prog{ command = "-" }
    , State = #bf_state{ current_cell = CellValue }
) ->
    <<CellInt:CellSize>> = CellValue,
    { advance_program(Program), State#bf_state{ current_cell = <<(CellInt - 1):CellSize>> } };

step( _
    , Program = #bf_prog{ command = "[" }
    , State = #bf_state{ current_cell = <<CellInt>> }
) ->
    case CellInt of
        0 -> { jump_forward(Program), State };
        _ -> { advance_program(Program), State }
    end;

step( _
    , Program = #bf_prog{ command = "]" }
    , State = #bf_state{ current_cell = <<CellInt>> }
) ->
    case CellInt of
        0 -> { advance_program(Program), State };
        _ -> { jump_backward(Program), State }
    end;

step( _Interpreter = #interpreter{ input = Input }
    , Program = #bf_prog{ command = "," }
    , State
) ->
    InputChar = io:get_chars(Input, "", 1),
    { advance_program(Program), State#bf_state{ current_cell = erlang:list_to_binary(InputChar) } };

step( _Interpreter = #interpreter{ output = Output }
    , Program = #bf_prog{ command = "." }
    , State = #bf_state{ current_cell = CellValue }
) ->
    io:write(Output, erlang:binary_to_atom(CellValue, latin1)),
    { advance_program(Program), State }.