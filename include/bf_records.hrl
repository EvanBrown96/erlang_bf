-record(interpreter, { cell_size = 8
                     , memory    = 30000
                     }).

-record(bf_prog, { command, todo, executed }).

-record(bf_memory, { prev_mem, current_cell, next_mem }).

-record(bf_state, { status = ok, program, memory }).