-module(process_native).
-export([start_link/1,  do_receive/0, monitor/1]).

start_link(Task) ->
  spawn_link(fun () ->
      Task(fun () ->
          do_receive()
      end),
      io:fwrite("Done Processing~n", [])
  end).


do_receive() ->
    receive
        {'DOWN', Ref, process, _Pid2, _Reason} ->
            {down, Ref};
        Message ->
            {message, Message}
    end.

monitor(Pid) ->
  erlang:monitor(process, Pid).
