//// Work with continuations as values.

/// `r` is the final return type of the whole computation.
/// `a` is the type of the value currently inside the monad.
pub type Cont(r, a) =
  fn(fn(a) -> r) -> r

/// Create a continuation that returns the given value when called.
/// 
/// Often called pure.
pub fn return(value: a) -> Cont(r, a) {
  fn(k: fn(a) -> r) -> r { k(value) }
}

/// compose two a continuation with a new function that returns another continuation
/// 
/// This is monad bind.
pub fn then(cont: Cont(r, a), next: fn(a) -> Cont(r, b)) -> Cont(r, b) {
  fn(k: fn(b) -> r) -> r { cont(fn(a: a) -> r { next(a)(k) }) }
}

/// Apply a function to each value in a list in order
pub fn each(
  over list: List(a),
  with f: fn(a) -> Cont(r, b),
) -> Cont(r, List(b)) {
  case list {
    [] -> return([])
    [head, ..tail] -> {
      use head <- then(f(head))
      use rest <- then(each(tail, f))
      return([head, ..rest])
    }
  }
}
