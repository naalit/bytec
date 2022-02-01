# Modules

Using multiple source files ("modules") is generally a good idea. In ByteC, the directory that files are in doesn't matter, only the file name.
Circular dependencies are allowed (at least, *almost* all the time).
Items from other files can be accessed with `use` or with `::`, the same as Rust:

```rust
// One.bt
let one = 1;
fn main() = Two::doStuff();
```

```rust
// Two.bt
use One::one; // One::* would also work
extern fn println(x: str) = "System.out.println";
fn doStuff() {
    println("two: " + (one + one));
}
```