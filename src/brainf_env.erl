-module(brainf_env).

-include("brainf.hrl").

-export([make_env/3]).

-spec make_env(CellBits :: pos_integer(), MemSize :: pos_integer() | infinity, BoundsBehavior :: fail | wrap | expand) -> #env_settings{}.
make_env(CellBits, MemSize, BoundsBehavior) ->
    #env_settings{ cell_bits = CellBits, memory_settings = { MemSize, BoundsBehavior } }.