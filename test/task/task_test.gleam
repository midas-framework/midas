import task/task
import gleam/should

// Don't want to write Normal at the end of all my functions.
// I want to assume anything that isn't a crash is normal.
// in which case do I ignore the return value and exit normal. Or do I do something new.
pub fn task_test() {
  let t1 = task.spawn(fn() { 2 + 2 })
  let r = task.wait(t1)
  should.equal(r, 4)
}
// let t2 = task.monitor(t1)
// let r = task.wait(t2)
// should.equal(r, 4)
