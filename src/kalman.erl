-module(kalman).
-export([new/0, update/2, get/2]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("../shared/erl_kalman", 0).

new() ->
  exit(nif_library_not_loaded).

update(_Kalman, _Measurement) ->
  exit(nif_library_not_loaded).

get(_Kalman, _Attr) ->
  exit(nif_library_not_loaded).
