-module(net_http_native).
-export([atom_or_binary_to_string/1, listen/1, read_http/2]).

atom_or_binary_to_string(Atom) when is_atom(Atom) -> erlang:atom_to_binary(Atom, utf8);
atom_or_binary_to_string(Binary) when is_binary(Binary) -> Binary.

listen(Port) ->
  TcpOptions = [{mode, binary}, {packet, http_bin}, {active, false}, {reuseaddr, true}],
  gen_tcp:listen(Port, TcpOptions).


read_http(Socket, Timeout) ->
  gen_tcp:recv(Socket, 0, Timeout).
