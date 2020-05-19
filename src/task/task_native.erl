-module(task_native).
-export([spawn/1, yield/1, monitor/1]).

spawn(Run) ->
  erlang:spawn_monitor(fun ()  ->
    erlang:exit({completed, Run()})
  end).

yield({Pid, Ref}) ->
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      case Reason of
        {completed, Value} ->
          Value
      end
  end.

monitor({Pid, _}) ->
  {Pid, erlang:monitor(process, Pid)}.
