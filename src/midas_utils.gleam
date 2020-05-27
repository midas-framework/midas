import gleam/option.{Option}

pub external fn split_on(String, String) -> tuple(String, Option(String)) =
  "midas_utils_native" "split_on"

pub external fn display(a) -> Nil =
  "erlang" "display"

pub external fn format(String) -> Nil =
  "io" "format"
