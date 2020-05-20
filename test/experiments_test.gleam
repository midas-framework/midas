// import gleam/dynamic
// import gleam/should
//
// import process/process.{Infinity, From}
//
// pub type Parser(r) {
//   Pop(Parser(fn(String) -> r))
//   End(r)
// }
//
// fn apply(parser: Parser(fn(String) -> r), value: String) -> Parser(r) {
//   // fn apply(parser) {
//   case parser {
//     End(func) -> End(func(value))
//     Pop(inner) -> {
//       // the r value is always another function in the case of a POP here, but the compiler is unable to work that out.
//       let inner = dynamic.unsafe_coerce(dynamic.from(inner))
//       let applied = apply(inner, value)
//       let applied = dynamic.unsafe_coerce(dynamic.from(applied))
//       Pop(applied)
//     }
//   }
// }
//
// // Pop(inner) -> Pop( , value))
// // Pop(End(func)) -> Pop(End(func(value)))
// // Pop(Pop(End(func))) -> Pop(Pop(End(func(value))))
// fn decode(parser: Parser(r), input: List(String)) -> Result(r, Nil) {
//   case parser {
//     // THis is where the pop is removed
//     Pop(
//       inner,
//     ) -> // For now just assume there are enough entries in the list
//     case input {
//       [value, ..rest] -> decode(apply(inner, value), rest)
//       [] -> Error(Nil)
//     }
//     // A switch all the sub parsers have the same response type, soo can be a list
//     End(value) -> Ok(value)
//   }
// }
//
// // Run over the parser for documentation
// pub fn run_test() {
//   let parser = Pop(Pop(Pop(End(fn(a) { fn(b) { fn(c) { tuple(a, b) } } }))))
//   parser == End(tuple("", ""))
//   let Ok(tuple("a", "b")) = decode(parser, ["a", "b", "c"])
//   let Error(Nil) = decode(parser, [])
//   should.equal(5, 5)
// }


// type Stack(a) {
//     Push(a)
//     DoPop(From(a))
// }
//
// fn loop(receive, state) {
//     case receive(Infinity) {
//         Push(new) -> loop(receive, [new, ..state])
//         DoPop(from) -> {
//             let tuple(top, state) = list.pop(state)
//             process.reply(from, top)
//             loop(receive, state)
//         }
//     }
// }

// fn start_stack() {0}
    // process.spawn_link(loop(_, []))

// let s1 = start_stack()
// let s2 = process.spawn_link(loop(_, []))

// process.send(s1, Push(1))
// process.send(s2, Push("hello"))

//
// fn form_parser() {
//   Input("username", string, Input("age", integer, Collect(Curry2(User))))
//
//   Map2(User, element(""))
//
//   Pair(
//     Input("username", parse.string),
//     Pair(
//       Input("age", parse.integer),
//       Pair(Input("number", parse.telephone), Input),
//     ),
//   )
//
//   Request5(
//     ContentLength,
//     Body,
//     QueryParam("foo"),
//     Static("users"),
//     Segment(parse.string),
//     handler,
//   )
//
//   // THis approach doesn't really have branching
//   // let Pair(a, Pair(b, tuple(c, )))
//   Routes(
//     [
//       Segment("users", Param("user_id", string, Get(fn(user_id) { response }))),
//       // Param("user_id", string, Get(biz.get_users)),
//       // Response("json", serialize, )
//       // Handler returns result but parsing can return Stop/Continue types
//       Segment(
//         "users",
//         Query(
//           "order",
//           string,
//           // Raw handler passes in the request
//           Query("active", boolean, RawHandler(fn(user_id) { response })),
//         ),
//       ),
//       // Body can be list of parsers
//       Segment("users", Form(user_parser)),
//     ],
//   )
//
//   Match(
//     "users",
//     [
//       Get(fn() { "All users" }),
//       Param(
//         string([MinLength(14), MaxLength(14)]),
//         [
//           Get(fn(user_id) { "user 1" }),
//           Match("update", [Post(fn(user_id) { "update user 1" })]),
//           Match(
//             "posts",
//             [
//               Param(
//                 integer([Max(999)]),
//                 [
//                   QueryParam(
//                     "published",
//                     boolean,
//                     [
//                       Get(fn(user_id: String) {fn(post_id: Int) { fn(published: Bool) { todo } } } ),
//                     ],
//                   ),
//                 ],
//               ),
//             ],
//           ),
//         ],
//       ),
//     ],
//   )
//
//   Choice(
//     [
//       Segment("users", Get(fn() { "All users" })),
//       Segment("users", Param(string([MinLength(14), MaxLength(14)], Get(fn(user_id) { "user 1" })))),
//       Segment("users", Param(string([MinLength(14), MaxLength(14)], Match("update", Post(fn(user_id) { "update user 1" }))))),
//       Segment("users", Param(string([MinLength(14), MaxLength(14)], Match("posts", Param(integer([Max(999)]), QueryParam("published", boolean, Get(fn(user_id: String) {fn(post_id: Int) { fn(published: Bool) { todo } } } ))))))),
//     ],
//   )
//   user_id = string([MinLength(14), MaxLength(14)]
//   post_id = integer([Max(999)])
//   get_user = fn(user_id) { "user 1" }
//   update_user = fn(user_id) { "update user 1" }
//   get_published_posts = fn(user_id: String) {fn(post_id: Int) { fn(published: Bool) { todo } } }
//
//   Choice(
//     [
//       Segment("users", Get(get_users)),
//       Segment("users", Param(user_id, Get(get_user)))),
//       Segment("users", Param(user_id, Match("update", Post(update_user))))),
//       Segment("users", Param(user_id, Match("posts", Param(, QueryParam("published", boolean, Get(get_published_posts))))))),
//     ],
//   )
//
//   // Its a bunch of error composition
//   // the second option particularly if grouped by controller might as well be the clear fn call approach.
//
//   fn get_published_posts(){
//       try tuple(user_id, post_id) = web.match(request, Segment("users", Param(uuid, Segment("posts", Param(int, Done(tuple2))))))
//       try published = web.query_param(request, "published", integer([Max(999)]))
//   }
//
//   Can have a Response(serializer: fn(x) -> r, Get(return x))
//   // Could serialize just OK value
//
//   [
//     Required("blah", int),
//     Optional()
//     Many("foo", int, [])
//   ]
// }
// Previous efforts below here.
// // Int is status
// pub type Api(t) {
//   // Capture(String, fn(String) -> Int)
//   Segment(Api(fn(String) -> t))
//   Get(t)
// }
//
//
// // fn do_step(x: Api(fn(String) -> t)) -> Api(t) {
// //
// // }
//
// // pull enough from a list
// fn reduce(x: Api(fn(t) -> next), value: t) -> next {
//     case x {
//         Get(func) -> func(value)
//         Segment(rest) -> Segment(reduce(rest, value))
//     }
//     // todo
// }
// // so if segment know at least
//
// fn step(x: Api(a)) -> a {
//     case x {
//         // Segment(Segment(Get(handler))) -> Get(handler("foo")("bar"))
//         // Segment(Get(handler)) -> Get(handler("foo"))
//         Segment(rest) -> step(reduce(rest, "Foo"))
//         Get(x) -> x
//     }
// }
// //
// fn apply(endpoint, stack) {
//     Get(3)
//     Segment(Get(fn(_) { 3 }))
//
//     case endpoint {
//         // Capture(segment, next) -> {
//         //
//         // }
//         Get(handle) -> {
//             let tuple(part, rest) = stack
//             let response = handle(part)
//             case rest {
//                 // Ok(more) ->
//                 Error(Nil) -> response
//             }
//         }
//     }
// }
// //
// // fn handler(endpoint: Api(a)) -> fn() -> a {
// //     case endpoint {
// //         Capture(part, next) -> {
// //             let f = handler(next)
// //             fn(){
// //                 f(part)
// //             }
// //             // let Nil =
// //             // (part)
// //             // Nil
// //         }
// //         // Get(response) -> response
// //     }
// // }
// //
// // pub fn api_test() {
// // pub type VTwo {
// //     Segment(VTwo)
// //     Handler(fn() -> )
// // }
// //
// // fn reduce(x) {
// //     case x {
// //
// //     }
// // }
// //
// // pub fn segment_test() {
// //     Segment(Handler(fn() { "OK"}))
// //     // Handler(fn() { h("user_id") })
// // }
// //
// //   // apply(Capture("foo", Capture("bar", Get(fn(a) { fn(b) { 200 } }))))
// //   // apply(Get(200), Nil)
// //   0
// //   // let state = tuple("foo", tuple("bar"))
// // }
// // //
// // // pub type VTwo {
// // //     Segment(VTwo)
// // //     Handler(fn() -> )
// // // }
// // //
// // // fn reduce(x) {
// // //     case x {
// // //
// // //     }
// // // }
// // //
// // // pub fn segment_test() {
// // //     Segment(Handler(fn() { "OK"}))
// // //     // Handler(fn() { h("user_id") })
// // // }
