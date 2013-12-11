-module(node_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Listener = {node, {node, start_link, []},
             permanent, 2000, worker, [node]},
  Procs = [Listener],
  {ok, {{one_for_one, 10, 10}, Procs}}.
