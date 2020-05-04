import uri/uri

import gleam/should

pub fn full_parse_test() {
    let Ok(parsed) = uri.parse("https://foo:bar@example.com:1234/path?query=true#fragment")
    should.equal(parsed.scheme, Ok("https"))
    should.equal(parsed.userinfo, Ok("foo:bar"))
    should.equal(parsed.host, Ok("example.com"))
    should.equal(parsed.port, Ok(1234))
    should.equal(parsed.path, Ok("/path"))
    should.equal(parsed.query, Ok("query=true"))
    should.equal(parsed.fragment, Ok("fragment"))
}

pub fn parse_only_path_test() {
    let Ok(parsed) = uri.parse("/")
    should.equal(parsed.scheme, Error(Nil))
    should.equal(parsed.userinfo, Error(Nil))
    should.equal(parsed.host, Error(Nil))
    should.equal(parsed.port, Error(Nil))
    should.equal(parsed.path, Ok("/"))
    should.equal(parsed.query, Error(Nil))
    should.equal(parsed.fragment, Error(Nil))
}

pub fn parse_only_host_test() {
    let Ok(parsed) = uri.parse("//")
    should.equal(parsed.scheme, Error(Nil))
    should.equal(parsed.userinfo, Error(Nil))
    should.equal(parsed.host, Ok(""))
    should.equal(parsed.port, Error(Nil))
    should.equal(parsed.path, Ok(""))
    should.equal(parsed.query, Error(Nil))
    should.equal(parsed.fragment, Error(Nil))
}
