---
layout: home
title:  "kuyfi"
section: "home"
---

## Scala Java-Time

[![Build Status](https://travis-ci.org/cquiroz/kuyfi?branch=master)](https://travis-ci.org/cquiroz/kuyfi)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.cquiroz/kuyfi_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.cquiroz/kuyfi_2.12)
[![Scala.js](http://scala-js.org/assets/badges/scalajs-0.6.8.svg)](http://scala-js.org)

This project provides a parser for the tzdb data files and can generate source code that can be imported by some other projects

#### Usage

The *kuyfi* library is currently available for Scala (JVM, version 8 and later)
Both Scala 2.10 and Scala 2.11

To get started with SBT, add one (or both) of these dependencies:

- `libraryDependencies += "io.github.cquiroz" %% "kuyfi" % "0.6.0"

#### Building
This project builds using sbt.

#### Testing

Tests are executed via

```
sbt test
```

Note that the tests compare the output with the timezone support on the JVM hence they may not match to any JVM version
At the moment the test use the JDK 1.8 b35 to match the version present in travis
