-module(pid).
-export([new/0, update/2, get/2, set/3]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("../shared/erl_pid", 0).

new() ->
  exit(nif_library_not_loaded).

update(__PID, __measurement) ->
  exit(nif_library_not_loaded).

get(__PID, __Attr) ->
  exit(nif_library_not_loaded).

set(__PID, __Attr, __SetVal) ->
  exit(nif_library_not_loaded).
