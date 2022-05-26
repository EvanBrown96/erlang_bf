-module(brainf_program).

-include("brainf.hrl").

handle_command($+, EnvSettings, Program, Memory) ->
    NewMem = brainf_memory:increment(Memory, EnvSettings),
    { Program, NewMem };
handle_command($-, EnvSettings, Program, Memory) ->
    NewMem = brainf_memory:decrement(Memory, EnvSettings),
    { Program, NewMem };
handle_command($<, EnvSettings, Program, Memory) ->
    NewMem = brainf_memory:ptr_left(Memory, EnvSettings),
    { Program, NewMem };
