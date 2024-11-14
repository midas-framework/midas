import gleam/float
import gleam/int
import gleam/list
import gleam/string
import lustre/attribute as a
import website/elements

// const center_x = "cx"

// const center_y = "cy"

// const radius = "r"

// const fill = "fill"

// const stroke = "stroke"

// circle(r, at: #(0, 0))
// |> center(at: #(0,0))
// |> dashed
// |> stroke(red, 5, filled)
// |> fill(gradiend())

pub type Color =
  String

pub type Form {
  Circle(center_x: Int, center_y: Int, radius: Int)
  Rectangle(x: Int, y: Int, width: Int, height: Int)
  Line(x1: Int, y1: Int, x2: Int, y2: Int)
  Path(d: String)
}

pub fn circle(radius) {
  Shape(Circle(0, 0, radius), unstyled)
}

pub fn line(x2, y2) {
  Shape(Line(0, 0, x2, y2), unstyled)
}

pub fn rectangle(width, height) {
  Shape(Rectangle(0, 0, width, height), unstyled)
}

pub fn path(d) {
  Shape(Path(d), unstyled)
}

fn transform(shape, with) {
  let Shape(form, style) = shape
  Shape(with(form), style)
}

fn do_center(form, x, y) {
  case form {
    Circle(radius: radius, ..) -> Circle(x, y, radius)
    Rectangle(width: width, height: height, ..) -> {
      let x = x - width / 2
      let y = y - height / 2
      Rectangle(x, y, width, height)
    }
    Line(x1, y1, x2, y2) -> {
      let width = x2 - x1
      let height = y2 - y1
      let x1 = x - width / 2
      let y1 = y - height / 2
      let x2 = x1 + width
      let y2 = y1 + height
      Line(x1, y1, x2, y2)
    }
    _ -> todo as "center path"
  }
}

pub fn center(shape, x, y) {
  transform(shape, do_center(_, x, y))
}

fn do_from(form, x, y) {
  case form {
    Circle(radius: radius, ..) -> {
      let cx = x + radius / 2
      let cy = y + radius / 2
      Circle(cx, cy, radius)
    }
    Rectangle(width: width, height: height, ..) ->
      Rectangle(x, y, width, height)

    Line(x1, y1, x2, y2) -> {
      let width = x2 - x1
      let height = y2 - y1
      let x1 = x
      let y1 = y
      let x2 = x1 + width
      let y2 = y1 + height
      Line(x1, y1, x2, y2)
    }
    _ -> todo as "from path"
  }
}

pub fn from(shape, x, y) {
  transform(shape, do_from(_, x, y))
}

pub fn fill(shape, color) {
  let Shape(form, style) = shape
  let style = Style(..style, fill: color)
  Shape(form, style)
}

pub fn stroke(shape, color, width) {
  let Shape(form, style) = shape
  let Style(fill, stroke) = style
  let stroke = Stroke(..stroke, color: color, width: width)
  let style = Style(fill: fill, stroke: stroke)
  Shape(form, style)
}

fn stroke_offset(stroke) {
  let Stroke(dashed: dashed, ..) = stroke
  case dashed {
    Solid -> 0
    Dashed(offset: offset, ..) -> offset
  }
}

pub fn dashed(shape, array) {
  let Shape(form, style) = shape
  let Style(fill, stroke) = style
  let stroke = Stroke(..stroke, dashed: Dashed(array, stroke_offset(stroke)))
  let style = Style(fill: fill, stroke: stroke)
  Shape(form, style)
}

pub type Style {
  Style(fill: Color, stroke: Stroke)
}

const unstyled = Style("transparent", Stroke("transparent", 0.0, Solid))

// width 0
pub type Stroke {
  Stroke(color: Color, width: Float, dashed: Dashed)
}

pub type Dashed {
  Solid
  Dashed(array: List(Int), offset: Int)
}

pub type Shape {
  Shape(form: Form, style: Style)
}

pub fn to_element(shape) {
  let Shape(form, style) = shape
  let attributes = [
    a.attribute("fill", style.fill),
    ..{
      let Stroke(color, width, dashed) = style.stroke
      case width {
        0.0 -> []
        _ -> [
          a.attribute("stroke", color),
          a.attribute("stroke-width", float.to_string(width)),
          ..case dashed {
            Solid -> []
            Dashed(array, offset) -> [
              a.attribute(
                "stroke-dasharray",
                list.map(array, int.to_string) |> string.join(" "),
              ),
              ..case offset {
                0 -> []
                _ -> [a.attribute("stroke-dashoffset", int.to_string(offset))]
              }
            ]
          }
        ]
      }
    }
  ]

  case form {
    Circle(cx, cy, r) -> elements.circle(cy, cx, r, attributes)
    Rectangle(x, y, w, h) -> elements.rectangle(x, y, w, h, attributes)
    Line(x1, y1, x2, y2) -> elements.line(x1, y1, x2, y2, attributes)
    Path(d) -> elements.path(d, attributes)
  }
}
