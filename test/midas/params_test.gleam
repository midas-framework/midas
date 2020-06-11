import gleam/option.{Some, None}
import midas/params.{Missing, CastFailure, as_string, as_integer, Trim, MinLength, MaxLength, Allow, Disallow, Min, Max}
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

pub fn as_string_test() {
  [tuple("foo", "bar")]
  |> params.required("foo", as_string(_, [MinLength(4)]))
  |> should.equal(Error(CastFailure("foo", "less than minimum length of 4")))
  [tuple("foo", "bar")]
  |> params.required("foo", as_string(_, [MaxLength(2)]))
  |> should.equal(Error(CastFailure("foo", "greater than maximum length of 2")))

  [tuple("foo", "bar")]
  |> params.required("foo", as_string(_, [Allow(["other"])]))
  |> should.equal(Error(CastFailure("foo", "not an allowed value")))

  [tuple("foo", "bar")]
  |> params.required("foo", as_string(_, [Disallow(["bar"])]))
  |> should.equal(Error(CastFailure("foo", "is a disallowed value")))

  // test can pass through validations
  let validations = [
      Allow(["bar"]),
      Disallow(["other"]),
      MinLength(2),
      MaxLength(4),
    ]
  [tuple("foo", "bar")]
  |> params.required("foo", as_string(_, validations))
  |> should.equal(Ok("bar"))

  // test runs all validations
  let validations = [
      Allow(["bar"]),
      Disallow(["other"]),
      MinLength(2),
      MaxLength(2),
    ]
  [tuple("foo", "bar")]
  |> params.required("foo", as_string(_, validations))
  |> should.equal(Error(CastFailure("foo", "greater than maximum length of 2")))

  [tuple("foo", "  bar   ")]
  |> params.required("foo", as_string(_, [Trim, MaxLength(3)]))
  |> should.equal(Ok("bar"))
}

pub fn as_integer_test() {
  [tuple("foo", "4")]
  |> params.required("foo", as_integer(_, [Min(5)]))
  |> should.equal(Error(CastFailure("foo", "less than minimum of 5")))

  [tuple("foo", "4")]
  |> params.required("foo", as_integer(_, [Max(3)]))
  |> should.equal(Error(CastFailure("foo", "greater than maximum of 3")))

  [tuple("foo", "4")]
  |> params.required("foo", as_integer(_, []))
  |> should.equal(Ok(4))
  [tuple("foo", " 4  ")]
  |> params.required("foo", as_integer(_, []))
  |> should.equal(Ok(4))

  [tuple("foo", "4.0")]
  |> params.required("foo", as_integer(_, []))
  |> should.equal(Error(CastFailure("foo", "not an integer value")))

  [tuple("foo", "bar")]
  |> params.required("foo", as_integer(_, []))
  |> should.equal(Error(CastFailure("foo", "not an integer value")))
}
