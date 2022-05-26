-module(brainf_memory).

-include("brainf.hrl").

-export([empty_memory/1, ptr_left/2, ptr_right/2, increment/2, decrement/2, get_current/2, set_current/3]).

-spec empty_cell( #env_settings{} ) -> binary().
empty_cell( #env_settings{ cell_bits = CellBits } ) ->
    <<0:CellBits>>.

-spec empty_memory( #env_settings{} ) -> #memory_state{}.
empty_memory( EnvSettings ) ->
    #memory_state{ prev = []
                 , current = empty_cell(EnvSettings)
                 , next = []
                 }.

-spec ptr_left( #memory_state{}, #env_settings{} ) -> #memory_state{}.
ptr_left( #memory_state{ prev = [], current = Current, next = Next}
        , EnvSettings = #env_settings{ memory_settings = MemorySettings } 
        ) ->
    case MemorySettings of
        { _, fail } ->
            error(memory_access);
        { _, expand } ->
            #memory_state{ prev = [], current = empty_cell(EnvSettings), next = [Current | Next] };
        { _, wrap } ->
            [NewCurrent | NewPrev] = lists:reverse([Current | Next]),
            #memory_state{ prev = NewPrev, current = NewCurrent, next = [] }
    end;
ptr_left( #memory_state{ prev = [First | Rest], current = Current, next = Next }
        , _EnvSettings
        ) ->
    #memory_state{ prev = Rest, current = First, next = [Current | Next] }.

-spec ptr_right( #memory_state{}, #env_settings{} ) -> #memory_state{}.
ptr_right( #memory_state{ prev = Prev, current = Current, next = [] }
         , EnvSettings = #env_settings{ memory_settings = MemorySettings } 
         ) ->
    case MemorySettings of
        { infinity, _ } ->
            #memory_state{ prev = [ Current | Prev ], current = empty_cell(EnvSettings), next = [] };
        { _, fail } ->
            error(memory_access);
        { _, wrap } ->
            [NewCurrent | NewNext] = lists:reverse([Current | Prev]),
            #memory_state{ prev = [], current = NewCurrent, next = NewNext }
    end;
ptr_right( #memory_state{ prev = Prev, current = Current, next = [First | Rest]}
         , _EnvSettings
         ) ->
    #memory_state{ prev = [Current | Prev], current = First, next = Rest }.

-spec increment( #memory_state{}, #env_settings{} ) -> #memory_state{}.
increment( Memory = #memory_state{ current = Current }
         , #env_settings{ cell_bits = CellBits }
         ) ->
    <<X:CellBits>> = Current,
    Memory#memory_state{ current = <<(X+1):CellBits>>}.

-spec decrement( #memory_state{}, #env_settings{} ) -> #memory_state{}.
decrement( Memory = #memory_state{ current = Current }
         , #env_settings{ cell_bits = CellBits }
         ) ->
    <<X:CellBits>> = Current,
    Memory#memory_state{ current = <<(X-1):CellBits>>}.

-spec get_current( #memory_state{}, #env_settings{} ) -> non_neg_integer().
get_current( #memory_state{ current = Current } 
           , #env_settings{ cell_bits = CellBits }
           ) ->
    <<X:CellBits>> = Current,
    X.

-spec set_current( #memory_state{}, #env_settings{}, non_neg_integer() ) -> #memory_state{}.
set_current( Memory 
           , #env_settings{ cell_bits = CellBits }
           , Value
           ) ->
    Memory#memory_state{ current = <<Value:CellBits>> }.