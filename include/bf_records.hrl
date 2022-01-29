-record(interpreter, { cell_size = 8
                     , memory    = 30000
                     , input
                     , output 
                     }).

-record(bf_prog, { command, todo, executed }).

-record(bf_state, { prev_mem, current_cell, next_mem }).