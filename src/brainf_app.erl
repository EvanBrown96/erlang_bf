%%%-------------------------------------------------------------------
%% @doc brainf public API
%% @end
%%%-------------------------------------------------------------------

-module(brainf_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    brainf_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
