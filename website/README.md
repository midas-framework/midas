# website

[![Package Version](https://img.shields.io/hexpm/v/website)](https://hex.pm/packages/website)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/website/)

```sh
gleam add website@1
```
```gleam
import website

pub fn main() {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/website>.

## Development

```sh
podman build -t midas_website .
podman run -it -w /opt/app -v ${PWD}:/opt/app -p 1234:1234 midas_website
```
