import midas/params.{Missing, CastFailure}
import gleam/should

pub fn required_param_test() {
  []
  |> params.required("foo", Ok)
  |> should.equal(Error(Missing("foo")))

  [tuple("foo", "")]
  |> params.required("foo", Ok)
  |> should.equal(Error(Missing("foo")))

  [tuple("foo", "value")]
  |> params.required("foo", Ok)
  |> should.equal(Ok("value"))

  [tuple("foo", "value")]
  |> params.required("foo", fn(_) { Error("not a foo") })
  |> should.equal(Error(CastFailure("foo", "not a foo")))
}
