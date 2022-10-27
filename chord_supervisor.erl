-module(chord_supervisor).
% -mode(compile).
-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(NumNodes,NumRequests) ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, [NumNodes,NumRequests]).

init([NumNodes,NumRequests]) ->
  process_flag(trap_exit, true),
  RestartStrategy = one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 5,

  SubFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Chord_child =
    {chord_server, {chord_server, start_link, [NumNodes,NumRequests]}, permanent, 2000, worker, [
      chord_server
    ]},

  {ok, {SubFlags, [Chord_child]}}.


