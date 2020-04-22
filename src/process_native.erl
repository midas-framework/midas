-module(process_native).
-export([start/1]).

start(Task) ->
  Pid = spawn(fun () ->
      Task(fun () ->
          5
      end)
  end),
  {ok, Pid}.


do_receive() ->
    receive
        {'DOWN', Ref, process, Pid2, Reason} ->
            down;
        Message ->
            {inner, Message}
    end.
