# Introduction

ByteC or `bytec` is a compiler from a tiny Rust-like language to bytecode-optimized Java, intended for use in Battlecode.

The language (also called "bytec" for lack of a better name) has a few features Java doesn't, like zero-overhead dynamic arrays, unrolled statically-sized arrays, and tuples, and it's fully interoperable with Java so it can be used for the performance-critical parts of a Java bot. The language itself is somewhere in between Rust and Java.

For an example of a Battlecode 2022 bot written in ByteC, see [https://github.com/naalit/battlecode22](https://github.com/naalit/battlecode22)
