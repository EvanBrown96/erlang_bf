-module(bf).
-export([interpreter/2, shell_run/2, step/2, default_state/2, performed_output/1, got_input/2]).
-include("../include/bf_records.hrl").

%% create an interpreter with the given parameters
%% CellSize - integer, number of bits per cell
%% MemorySize - integer or 'unlimited', number of cells available in memory
interpreter( CellSize, MemorySize ) ->
    #interpreter{ cell_size = CellSize
                , memory    = MemorySize
                }.

%% run a given brainfuck program simply, using the erlang shell
%% Interpreter - memory parameters to use in run
%% TextProgram - brainfuck program as a string
shell_run( Interpreter, TextProgram ) ->
    run(Interpreter, default_state(Interpreter, TextProgram)).

%% internal procedure to run brainfuck program in the erlang shell
%% Interpreter - memory parameters to use in run
%% State - current state of the brainfuck program & memory
run( _, State = #bf_state{ status = done } ) ->
    State;
run( Interpreter, State = #bf_state{ status = input } ) ->
    InputChar = io:get_chars(standard_io, "", 1),
    NewState = got_input(State, InputChar),
    run(Interpreter, NewState);
run( Interpreter, State = #bf_state{ status = { output, OutputChar } } ) ->
    io:write(standard_io, erlang:list_to_atom(OutputChar)),
    NewState = performed_output(State),
    run(Interpreter, NewState);
run( Interpreter, State ) ->
    NewState = step(Interpreter, State),
    run(Interpreter, NewState).

%% transition state after performing an output operation
%% State - current state of the brainfuck program & memory
performed_output( State = #bf_state{ status = { output, _ } } ) ->
    State#bf_state{ status = ok }.

%% transition state after perfoming an input operation
%% State - current state of the brainfuck program & memory
%% Input - the character received as input
got_input( State = #bf_state{ status = input 
                            , memory = Memory }
         , Input ) ->
    State#bf_state{ status = ok, memory = Memory#bf_memory{ current_cell = erlang:list_to_binary(Input) } }.
        %InputChar = Input(), %InputChar = io:get_chars(Input, "", 1),
    %{ advance_program(Program), State#bf_memory{ current_cell = erlang:list_to_binary(InputChar) } };

%% get default initial state to run a program over
%% Interpreter - memory parameters to initialize with
%% TextProgram - brainfuck program as a string
default_state( Interpreter, TextProgram ) ->
    #bf_state{ program = new_program(TextProgram), memory = start_memory(Interpreter) }.

%% get empty memory, pointer at cell 0
%% Interpreter - memory parameters to initialize with
start_memory( #interpreter{ cell_size = CellSize } ) ->
    #bf_memory{ prev_mem     = []
              , current_cell = <<0:CellSize>>
              , next_mem     = [] 
              }.

%% convert a string program into a Program representation
%% TextProgram - brainfuck program, as a string
new_program( [First | Rest] ) ->
    #bf_prog{ command  = [First]
            , todo     = Rest
            , executed = []
            }.

%% move program to next command
%% Program - the program to advance by one command
advance_program( #bf_prog{ command = [Command], todo = [], executed = Executed } ) ->
    #bf_prog{ command = []
            , todo = []
            , executed = [Command | Executed] 
            };
advance_program(#bf_prog{ command = [Command], todo = [Next | Todo], executed = Executed } ) ->
    #bf_prog{ command = [Next]
            , todo = Todo
            , executed = [Command | Executed] 
            }.

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
    #bf_prog{ command = [Prev]
            , todo = [Command | Todo]
            , executed = Executed 
            }.

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
%% Interpreter - memory parameters to use in run
%% State - current brainfuck program & memory state
step( _, State = #bf_state{ status = done } ) ->
    State;

step( _, State = #bf_state{ status = ok 
                          , program = #bf_prog{ command = [] } } ) ->
    State#bf_state{ status = done };

step( _Interpreter = #interpreter{ cell_size = CellSize
                                 , memory = MemorySize 
                                 }
    , State        = #bf_state{ status  = ok
                              , program = (Program = #bf_prog{ command = ">" })
                              , memory  = #bf_memory{ prev_mem = PrevMem, current_cell = CellValue, next_mem = NextMem }
                              }
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
    State#bf_state{ program = advance_program(Program), memory = #bf_memory{ prev_mem = [CellValue | PrevMem], current_cell = NewCell, next_mem = UpdatedMem } };

step( _
    , State = #bf_state{ status  = ok
                       , program = (Program = #bf_prog{ command = "<" })
                       , memory  = #bf_memory{ prev_mem = PrevMem, current_cell = CellValue, next_mem = NextMem }
                       }
) ->        
    [NewCell | UpdatedMem] = case PrevMem of
        [] ->
            erlang:exit(memory_exceeded);
        Other -> Other
    end,
    State#bf_state{ program = advance_program(Program), memory = #bf_memory{ prev_mem = UpdatedMem, current_cell = NewCell, next_mem = [ CellValue | NextMem ] } };

step( _Interpreter = #interpreter{ cell_size = CellSize }
    , State        = #bf_state{ status  = ok
                              , program = (Program = #bf_prog{ command = "+" })
                              , memory  = (Memory = #bf_memory{ current_cell = CellValue })
                              }
) ->
    <<CellInt:CellSize>> = CellValue,
    State#bf_state{ program = advance_program(Program), memory = Memory#bf_memory{ current_cell = <<(CellInt + 1):CellSize>> } };

step( _Interpreter = #interpreter{ cell_size = CellSize }
    , State        = #bf_state{ status  = ok
                              , program = (Program = #bf_prog{ command = "-" })
                              , memory  = (Memory = #bf_memory{ current_cell = CellValue })
                              }
) ->
    <<CellInt:CellSize>> = CellValue,
    State#bf_state{ program = advance_program(Program), memory = Memory#bf_memory{ current_cell = <<(CellInt - 1):CellSize>> } };

step( _
    , State = #bf_state{ status  = ok
                       , program = (Program = #bf_prog{ command = "[" })
                       , memory  = #bf_memory{ current_cell = <<CellInt>> }
                       }
) ->
    case CellInt of
        0 -> State#bf_state{ program = jump_forward(Program) };
        _ -> State#bf_state{ program = advance_program(Program) }
    end;

step( _
    , State = #bf_state{ status  = ok
                       , program = (Program = #bf_prog{ command = "]" })
                       , memory  = #bf_memory{ current_cell = <<CellInt>> }
                       }
) ->
    case CellInt of
        0 -> State#bf_state{ program = advance_program(Program) };
        _ -> State#bf_state{ program = jump_backward(Program) }
    end;

step( _
    , State = #bf_state{ status  = ok
                       , program = (Program = #bf_prog{ command = "," })
                       }
) ->
    State#bf_state{ status = input, program = advance_program(Program) };
    %InputChar = Input(), %InputChar = io:get_chars(Input, "", 1),
    %{ advance_program(Program), State#bf_memory{ current_cell = erlang:list_to_binary(InputChar) } };

step( _
    , State = #bf_state{ status  = ok
                       , program = (Program = #bf_prog{ command = "." })
                       , memory  = #bf_memory{ current_cell = CellValue }
                       } 
) ->
    State#bf_state{ status = { output, erlang:binary_to_list(CellValue) }, program = advance_program(Program) }.
    %Output(erlang:binary_to_list(CellValue)),
    %{ advance_program(Program), State }.