import midas/effect as e
import midas/task as t
import snag

pub fn success_composition_test() {
  let task = {
    use a <- t.do(t.strong_random(1))
    use b <- t.do(t.strong_random(2))
    t.done(<<a:bits, b:bits>>)
  }
  let assert e.StrongRandom(length: 1, resume:) = task
  let assert e.StrongRandom(length: 2, resume:) = resume(Ok(<<1>>))
  let assert e.Done(Ok(<<1, 8, 9>>)) = resume(Ok(<<8, 9>>))

  let assert e.StrongRandom(length: 1, resume:) = task
  let assert e.Done(Error(reason)) = resume(Error("not available"))
  assert snag.new("Failed to generate strong random: not available") == reason
}
