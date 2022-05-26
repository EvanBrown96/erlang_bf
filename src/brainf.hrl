-record(program_state, { prev
                       , next
                       }).

-record(memory_state, { prev    :: list(binary())
                      , current :: binary()
                      , next    :: list(binary())
                      }).

-record(env_settings, { cell_bits       :: pos_integer()
                      , memory_settings :: { infinity, expand | fail} | { pos_integer(), wrap | fail }
                      }).
