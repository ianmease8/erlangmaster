%%%-------------------------------------------------------------------
%% @doc package_tracking public API
%% @end
%%%-------------------------------------------------------------------

-module(package_tracking_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    package_tracking_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
