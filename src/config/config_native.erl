-module(config_native).
-export([getenv/0]).

getenv() ->
  [erlang:list_to_binary(X) || X <- os:getenv()].
