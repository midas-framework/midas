-module(core_supervisor_native).
-export([spawn_link/1,  do_receive/0]).

spawn_link(Task) ->
  erlang:spawn_link(fun () ->
      erlang:process_flag(trap_exit, true),
      Task(fun () ->
          do_receive()
      end)
  end).


do_receive() ->
    receive
        {'DOWN', Ref, process, _Pid2, _Reason} ->
            {down, Ref};
        {'EXIT', Pid, _Reason} ->
          {exit, Pid};
        Message ->
            {message, Message}
    end.
