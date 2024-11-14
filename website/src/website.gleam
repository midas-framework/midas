import gleam/int
import lustre
import lustre/attribute as a
import lustre/element.{text}
import lustre/element/html as h
import website/draw

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
  // app
}

fn init(_flags) {
  0
}

pub type Message {
  Message
}

fn update(model, _) {
  model
}

fn view(model) {
  let count = int.to_string(model)

  h.div(
    [
      a.class("bg-gray-100 min-h-screen p-4"),
      a.style([
        #(
          "background",
          "radial-gradient(farthest-corner at 40px 40px, #00bcd430 0%, #43e0 100%), radial-gradient(farthest-corner at 80% 80%, #7900d430 0%, #43e0 100%)",
        ),
      ]),
    ],
    [
      header(),
      hero(),
      // h.h1([a.class("text-xl shadow")], [text("hello")]),
    ],
  )
}

fn header() {
  h.div([a.class("fixed top-0 left-0 right-0 p-2")], [
    h.header(
      [
        a.class(
          "mx-auto max-w-5xl bg-white shadow-2xl p-2 rounded-xl border-r border-b border-blue-500",
        ),
      ],
      [text("ooooo")],
    ),
  ])
}

fn hero() {
  h.div([a.class("p-4")], [
    h.div([a.class("flex mx-auto mt-16")], [
      h.div([a.class("text-6xl flex-1 border border-black")], [text("Midas")]),
      h.div(
        [
          a.class("text-6xl flex-1"),
          a.style([#("background-image", "url('#testImg')")]),
        ],
        [draw.area([a.id("testImg"), a.attribute("viewport", "0 400 400 800")])],
      ),
    ]),
  ])
}
