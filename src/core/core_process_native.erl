-module(core_process_native).
-export([monitor/1, receive_reply/1]).

monitor(Pid) ->
  erlang:monitor(process, Pid).

receive_reply(Ref) ->
  receive
    {Ref, Message} ->
      {ok, Message}
      % TODO handle Down or Exir
  end.
