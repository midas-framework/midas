-module(core_process_native).
-export([monitor/1]).

monitor(Pid) ->
  erlang:monitor(process, Pid).
