-module(midas_utils_native).
-export([split_on/2]).

split_on(Subject, Pattern) ->
  case binary:split(Subject, Pattern) of
    [Part, Rest] -> {Part, {ok, Rest}};
    [Part] -> {Part, {error, nil}}
  end.
