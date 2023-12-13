# Language Tour

ByteC is syntactically similar to Rust: functions are declared with `fn` and variables with `let` and type inference.

```rust
fn triple(x: i32): i32 {
    let y = x + x;
    x + y // the last statement in a block is returned automatically
}
```

Functions and variables declared on the top-level of a module (a source file) turn into `static` members in Java.
Functions can be written in a single-expression style as well:

```rust
fn triple2(x: i32): i32 = x * 3;
```

ByteC has a few "primitive" types, which should be familiar from Rust, along with `as` to convert between number types:
```rust
let a: i32 = 12;
let b: i64 = 1300000000;
let c: str = "Hello, world!";
let d: bool = false;
let e: () = {}; // The unit type, used for `void` functions
let f: f32 = 9.2;
let g: f64 = 0.9352891473 + a as f64;
```

As well as tuples, which don't yet support destructuring but do support member access with dot syntax. These are of course lowered to separate variables.
```rust
let tup: (i32, i64) = (a, b);
a += tup.0;
b += tup.1;
```

The `==` and `!=` operators are automatically translated to `.equals()` when comparing objects, as long as neither side is `null`;
so you can just compare everything with `==` and it should never cause problems unless you specifically need reference equality for some reason.
Other operators are the same as Java, except that bitwise operators all have the same precedence and you should really just be using parentheses for those anyway.

All variables are immutable by default, but they can be declared mutable using `let mut`, and can then be reassigned and modified in the normal way:
```rust
let mut a = 3;
a += 2;
a = 4;
```
However, since ByteC compiles to Java, objects are always mutable - if you have an immutable `let` variable referring to an object, you can still reassign fields of the object, but you can't reassign the variable to point to a new object.

`if-else` and `match` (the equivalent to Java `switch`) are expressions, and can return a value from each branch.
They don't require parentheses, but do require braces around the body:

```rust
fn do_stuff(cond: bool, ty: RobotType): i32 {
    let a = if cond {
        // this is just a normal block, we can put statements here too
        let tmp = 12 + 3;
        tmp + 4
    } else {
        22
    };
    let b = match ty {
        ARCHON => 27,
        MINER => 2 + {
            // a block can be used in expression position
            // this variable is unrelated to the last one, blocks create their own scopes
            let tmp = 9 + 8;
            tmp * 2
        },
        // this is the `default` case
        // it's not necessary if we handle all possible variants
        else => 0,
    };
    a * b
}
```

Loops are not expressions, and there are about three types:

```rust
// The simplest kind of loop, loops infinitely until a `break`
let a = 0;
loop {
    a += 1;
    if a > 12 {
        break;
    }
}

// A while loop, this is just like other procedural languages
while a < 24 {
    a += 1;
}

// There are actually three kinds of four loop:
// A range for loop - this can be unrolled by adding the `unroll` keyword before the start of the range:
for i in 0..10 {
    a += i;
}
// A loop over a static array - this is guaranteed to be unrolled:
let sArr: [i32; 3] = [1, 2, 3];
for i in sArr {
    a += i;
}
// A loop over a dynamic array - this is never unrolled:
let dArr: [i32] = [1, 2, 3];
for i in dArr {
    a += i;
}
```

ByteC does have `null`, which is a possible value of classes, enums, and strings (but not either kind of array).
There are currently no optional types or any type safety regarding `null`.
`null` is usually inferred as the correct type, but in some cases the compiler can't figure this out and a construction like this is needed:
```rust
fn getMapLocation(): MapLocation {
    let n: MapLocation = null;
    n
}
```
