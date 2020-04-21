import gleam/result.{Option}

pub external fn split_on(String, String) -> tuple(String, Option(String))
  = "midas_utils_native" "split_on"
