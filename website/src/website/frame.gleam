import gleam/int
import gleam/list
import lustre/attribute as a
import lustre/element
import lustre/element/svg
import website/shape

pub type Frame(a) {
  Frame(width: Int, height: Int, shapes: List(element.Element(a)))
}

pub fn frame(width, height) {
  todo
}

pub fn to_element(frame, attributes) {
  let Frame(width, height, children) = frame
  svg.svg(
    [
      a.attribute("width", int.to_string(width)),
      a.attribute("height", int.to_string(height)),
      ..attributes
    ],
    // list.map(shapes, shape.to_element),
    children,
  )
}
