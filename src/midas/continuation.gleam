//// Work with effects as values using continuations.

/// `t` is the final return type of the whole computation.
/// `a` is the type of the value currently inside the monad.
pub type Continuation(t, a) =
  fn(fn(a) -> t) -> t

/// A task is a continuation of a result.
pub type Task(t, a, b) =
  Continuation(t, Result(a, b))

/// Create a continuation that returns the given value when called.
/// 
/// Often called pure.
pub fn return(value: a) -> Continuation(t, a) {
  fn(k: fn(a) -> t) -> t { k(value) }
}

/// compose two a continuation with a new function that returns another continuation
/// 
/// This is monad bind.
pub fn then(
  cont: Continuation(t, a),
  next: fn(a) -> Continuation(t, b),
) -> Continuation(t, b) {
  fn(k: fn(b) -> t) -> t { cont(fn(a: a) -> t { next(a)(k) }) }
}

/// Apply a function to each value in a list in order
pub fn each(
  over list: List(a),
  with f: fn(a) -> Continuation(t, b),
) -> Continuation(t, List(b)) {
  case list {
    [] -> return([])
    [head, ..tail] -> {
      use head <- then(f(head))
      use rest <- then(each(tail, f))
      return([head, ..rest])
    }
  }
}

/// Iterate over a list of items, and apply each to the given function f.
/// 
pub fn fold(
  over list: List(a),
  from initial: acc,
  with f: fn(acc, a) -> Continuation(t, acc),
) -> Continuation(t, acc) {
  case list {
    [] -> return(initial)
    [head, ..tail] -> {
      use acc <- then(f(initial, head))
      fold(tail, acc, f)
    }
  }
}

pub fn try_then(
  cont: Continuation(t, Result(a, e)),
  next: fn(a) -> Continuation(t, Result(b, e)),
) -> Continuation(t, Result(b, e)) {
  use result <- then(cont)
  case result {
    Error(e) -> return(Error(e))
    Ok(a) -> next(a)
  }
}

/// Work with results that are not continuations.
pub fn try(
  result: Result(a, b),
  then: fn(a) -> Task(t, c, b),
) -> Task(t, c, b) {
  case result {
    Ok(value) -> then(value)
    Error(reason) -> return(Error(reason))
  }
}

/// Work with results that are not continuations.
pub fn try_or(
  result: Result(a, b),
  handle: fn(b) -> c,
  then: fn(a) -> Continuation(t, c),
) -> Continuation(t, c) {
  case result {
    Ok(value) -> then(value)
    Error(reason) -> return(handle(reason))
  }
}

/// A task has completed successfully
pub fn done(value: a) -> Task(t, a, b) {
  return(Ok(value))
}

/// A task is done with an error
pub fn fail(reason: b) -> Task(t, a, b) {
  return(Error(reason))
}

/// Apply the given function to each item in a list, returning at first error.
pub fn try_each(
  over list: List(a),
  with f: fn(a) -> Task(t, b, c),
) -> Task(t, List(b), c) {
  case list {
    [] -> return(Ok([]))
    [head, ..tail] -> {
      use head <- try_then(f(head))
      use rest <- try_then(try_each(tail, f))
      return(Ok([head, ..rest]))
    }
  }
}
