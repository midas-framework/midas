import gleam/dynamic

type Never {
  Never(Never)
}

pub external type Task(t)

pub external fn spawn(fn() -> t) -> Task(t) =
  "task_native" "spawn"

pub external fn wait(Task(t)) -> t =
  "task_native" "yield"

pub external fn monitor(Task(t)) -> Task(t) =
  "task_native" "monitor"

fn foo() -> Never {
  dynamic.unsafe_coerce(dynamic.from(0))
}

fn demo() {
  let x = foo()
  x
}
