-module(core_task_native).
-export([spawn_link/1, do_receive/1]).

spawn_link(Task) ->
  erlang:spawn_link(fun () ->
      Task(fun (Wait) ->
          do_receive(Wait)
      end)
  end).


do_receive({milliseconds, Milliseconds}) ->
    do_receive(Milliseconds);
do_receive(Wait) ->
    receive
        {'DOWN', Ref, process, _Pid2, _Reason} ->
            {down, Ref};
        Message ->
            {message, Message}
    after Wait ->
         timeout
    end.
