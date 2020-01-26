# flawless

Delightful, purely functional testing no-framework.

[![License](http://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
[![Gitter](https://badges.gitter.im/kubukoz/flawless.svg)](https://gitter.im/kubukoz/flawless?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

# Work in progress

This project is a work in progress. Use it at your own responsibility. I expect the API to change quite often for the time being.

## Installation

```sbt
"com.kubukoz" %% "flawless" % "x.x.x"
```

## Usage

```scala
import flawless._
import flawless.syntax._
```

## Design

See [design doc](DESIGN.md) and [watch the talk](https://vimeo.com/368027707)!

## Adoption

- [runaid/runaid](https://github.com/runaid/runaid)
- [kubukoz/datas](https://github.com/kubukoz/datas)
- [flawless itself](tests/src/main/scala/flawless/tests)
