# Language Tour

ByteC is syntactically similar to Rust: functions are declared with `fn` and variables with `let` and type inference.

```rust
fn triple(x: i32): i32 {
    let y = x + x;
    x + y // the last statement in a block is returned automatically
}
```

Functions can be written in a single-expression style as well:

```rust
fn triple2(x: i32): i32 = x * 3;
```

`if-else` and `match` (the equivalent to Java `switch`) are expressions, and can return a value from each branch:

```rust
fn do_stuff(cond: bool, ty: RobotType) {
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
for i in sta {
    a += i;
}
// A loop over a dynamic array - this is never unrolled:
let dArr: [i32] = [1, 2, 3];
for i in dArr {
    a += i;
}
```

The two types of arrays, static and dynamic, are a concept that doesn't exist in Java so they deserve some explanation.
A static array has a constant length known at compile time, and it becomes a bunch of separate variables in Java.
Any loop over a static array is unrolled, and indexing the array must either use an index that the compiler can figure out is constant, or use the `inline` keyword to be turned into a `switch`.

A dynamic array, on the other hand, doesn't have a length known at compile time. In fact, the length of the array can change at any moment.
It's similar to a Java `ArrayList`, but all the logic is inlined, so any operations that normal Java arrays support (indexing, index assignment, looping over the array) are exactly as fast, and `push` and `pop` are pretty fast as well and don't involve method calls. Java arrays returned by `extern` functions are automatically converted to ByteC dynamic arrays. Here's an example of the operations supported by dynamic arrays:

```rust
// This initializes an array of five zeros
let x: [i32] = [; 5];

// The length of the array can be accessed with .len(); there is currently no way to access the capacity.
// This is also supported by static arrays
println("length: " + x.len());

// Note that there isn't actually bounds checking in array indexes for performance
// So if ByteC has allocated 8 elements but the length of the array is only 5, accessing x[6] has an undefined result
// (in practice it will return 0 or the last element to occupy that slot, or throw an exception if the space isn't allocated)
x[0] = 2;
let y = x[4];

// adds 12 to the end of the array, reallocating if there isn't enough space
x.push(12);

// pops the last element off the end of the array, leaving the space to be used by future push() calls
let twelve = x.pop();

// clears all elements from the array, setting the length to 0 and leaving the capacity allocated
// note that this only costs 2 bytecode, no matter the array capacity!
x.clear();
```