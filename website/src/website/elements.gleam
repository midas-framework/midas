import gleam/int
import lustre/attribute as a
import lustre/element/svg

pub fn stop(offset, color, attributes) {
  svg.stop([
    a.attribute("offset", int.to_string(offset) <> "%"),
    a.attribute("stop-color", color),
    ..attributes
  ])
}

pub fn circle(cx, cy, r, attributes) {
  svg.circle([
    a.attribute("cx", int.to_string(cx)),
    a.attribute("cy", int.to_string(cy)),
    a.attribute("r", int.to_string(r)),
    ..attributes
  ])
}

pub fn line(x1, y1, x2, y2, attributes) {
  svg.line([
    a.attribute("x1", int.to_string(x1)),
    a.attribute("y1", int.to_string(y1)),
    a.attribute("x2", int.to_string(x2)),
    a.attribute("y2", int.to_string(y2)),
    ..attributes
  ])
}

pub fn rectangle(x, y, width, height, attributes) {
  svg.rect([
    a.attribute("x", int.to_string(x)),
    a.attribute("y", int.to_string(y)),
    a.attribute("width", int.to_string(width)),
    a.attribute("height", int.to_string(height)),
    ..attributes
  ])
}

pub fn path(d, attributes) {
  svg.path([a.attribute("d", d), ..attributes])
}
