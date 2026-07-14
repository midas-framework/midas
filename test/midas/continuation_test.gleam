import gleeunit
import midas/continuation.{return, then}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn left_identity_test() {
  let value = 4
  let next = fn(value) { return(value + 3) }
  let finish = fn(value) { value * 2 }

  assert then(return(value), next)(finish) == next(value)(finish)
}

pub fn right_identity_test() {
  let computation = fn(continue) { continue(4) + 3 }
  let finish = fn(value) { value * 2 }

  assert then(computation, return)(finish) == computation(finish)
}

pub fn associativity_test() {
  let computation = fn(continue) { continue(4) + 3 }
  let first = fn(value) { fn(continue) { continue(value + 5) * 2 } }
  let second = fn(value) { return(value - 1) }
  let finish = fn(value) { value * 3 }

  let left = then(then(computation, first), second)
  let right = then(computation, fn(value) { then(first(value), second) })

  assert left(finish) == right(finish)
}
