-module(core_task_native).
-export([spawn_link/1,  do_receive/0]).

spawn_link(Task) ->
  erlang:spawn_link(fun () ->
      Task(fun () ->
          do_receive()
      end)
  end).


do_receive() ->
    receive
        {'DOWN', Ref, process, _Pid2, _Reason} ->
            {down, Ref};
        Message ->
            {message, Message}
    end.
