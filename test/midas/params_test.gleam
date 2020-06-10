import gleam/option.{Some, None}
import midas/params.{Missing, CastFailure}
import gleam/should

pub fn required_param_test() {
  []
  |> params.required("foo", Ok)
  |> should.equal(Error(Missing("foo")))

  [tuple("foo", "")]
  |> params.required("foo", Ok)
  |> should.equal(Error(Missing("foo")))

  [tuple("foo", "bar")]
  |> params.required("foo", Ok)
  |> should.equal(Ok("bar"))

  [tuple("foo", "bar")]
  |> params.required("foo", fn(_) { Error("not a foo") })
  |> should.equal(Error(CastFailure("foo", "not a foo")))
}

pub fn optional_param_test() {
  []
  |> params.optional("foo", Ok)
  |> should.equal(Ok(None))

  [tuple("foo", "")]
  |> params.optional("foo", Ok)
  |> should.equal(Ok(None))

  [tuple("foo", "bar")]
  |> params.optional("foo", Ok)
  |> should.equal(Ok(Some("bar")))

  [tuple("foo", "bar")]
  |> params.optional("foo", fn(_) { Error("not a foo") })
  |> should.equal(Error(CastFailure("foo", "not a foo")))
}

pub fn overridable_param_test() {
  []
  |> params.overridable("foo", Ok, "baz")
  |> should.equal(Ok("baz"))

  [tuple("foo", "")]
  |> params.overridable("foo", Ok, "baz")
  |> should.equal(Ok("baz"))

  [tuple("foo", "bar")]
  |> params.overridable("foo", Ok, "baz")
  |> should.equal(Ok("bar"))

  [tuple("foo", "bar")]
  |> params.overridable("foo", fn(_) { Error("not a foo") }, "baz")
  |> should.equal(Error(CastFailure("foo", "not a foo")))
}

pub fn boolean_param_test() {
  []
  |> params.boolean("foo")
  |> should.equal(False)

  [tuple("foo", "")]
  |> params.boolean("foo")
  |> should.equal(True)

  [tuple("foo", "bar")]
  |> params.boolean("foo")
  |> should.equal(True)
}

pub fn disallowed_param_test() {
  []
  |> params.disallowed("foo", "unsupported")
  |> should.equal(Ok(Nil))

  [tuple("foo", "")]
  |> params.disallowed("foo", "unsupported")
  |> should.equal(Ok(Nil))

  [tuple("foo", "bar")]
  |> params.disallowed("foo", "unsupported")
  |> should.equal(Error(CastFailure("foo", "unsupported")))
}
