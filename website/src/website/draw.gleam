import gleam/int
import gleam/list
import lustre/attribute as a
import lustre/element
import lustre/element/svg
import website/elements
import website/frame
import website/shape

fn px(value) {
  int.to_string(value) <> "px"
}

fn viewbox(width, height) {
  a.attribute(
    "viewbox",
    "0 0 " <> int.to_string(width) <> " " <> int.to_string(height),
  )
}

fn rotate(angle) {
  "rotate(" <> int.to_string(angle) <> ")"
}

fn gradient_linear(id, stops, rotation) {
  svg.linear_gradient(
    [a.attribute("id", id), a.attribute("gradientTransform", rotate(rotation))],
    list.map(stops, fn(stop) {
      let #(offset, color) = stop
      elements.stop(offset, color, [])
    }),
  )
}

fn id(id) {
  "url('#" <> id <> "')"
}

pub fn area(attributes) {
  frame.Frame(800, 400, [
    svg.defs([], [gradient_linear("momosa", [#(0, "gold"), #(100, "red")], 0)]),
    // shape.rectangle(100, 100)
    //   |> shape.center(70, 70)
    //   |> shape.fill("blue")
    //   |> shape.stroke("red", 5),
    shape.circle(100)
      |> shape.center(100, 100)
      |> shape.fill(id("momosa"))
      |> shape.stroke("navy", 160.0)
      |> shape.dashed([6, 6, 4, 40])
      |> shape.to_element(),
    shape.path("M 0 333 L 800 333")
      |> shape.stroke("navy", 0.1)
      |> shape.to_element(),
    // shape.line(100, 0)
    //   |> shape.stroke("darkgreen", 5)
    //   |> shape.dashed([12, 4]),
    // shape.line(100, 0)
    //   |> shape.from(200, 20)
    //   |> shape.stroke("darkgreen", 5)
    //   |> shape.dashed([12, 4]),
    ..list.map(list.range(1, 7), fn(i) {
      shape.line(800, 0)
      |> shape.from(0, i * 50)
      |> shape.stroke("gray", 0.2)
      // |> shape.dashed([12, 8])
      |> shape.to_element
    })
  ])
  |> frame.to_element(attributes)
}
