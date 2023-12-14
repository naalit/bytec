# Optimization Features

The primary purpose of ByteC is to enable high-level, maintanable programming that preserves maximum bytecode efficiency.
To that end, it has a few features designed specifically for this.

## Unrolled loops

Statically sized arrays were briefly mentioned in the language tour, but it's worth explaining them in more detail here.
An array that looks like this:
```rust
let arr: [i32; 3] = [1, 2, 3];
arr[1] += 1;
for i in arr {
    println("i = " + i);
}
```
Will generate Java code like this:
```java
static int arr$0 = 1;
static int arr$1 = 2;
static int arr$2 = 3;

arr$1 += 1;

System.out.println("i = " + arr$0);
System.out.println("i = " + arr$1);
System.out.println("i = " + arr$2);
```

You can also unroll range for loops with the `unroll` keyword:
```rust
for i in unroll 0..3 {
    arr[i] += i;
}
```

Note that you can index static arrays with any expression that the compiler can figure out at compile time. For example, this reduces to a bunch of one-line assignments to local variables, and the indices and if statements are all resolved at compile time:
```rust
let cache: [i32; 81];

for x in unroll -4..5 {
    for y in unroll -4..5 {
        if x*x + y*y <= 20 {
            let i = (x + 4) * 9 + y + 4;
            cache[i] = rc.senseRubble(MapLocation(at.x + x, at.y + y));
        }
    }
}
```

By default, indexing static arrays by non-constant expressions is an error (although the message isn't very helpful yet in this case).
If you really want to generate a `switch` over all possible indices, you can do that with the `inline` keyword:
```rust
let tx = target.x - at.x;
let ty = target.y - at.y;
if tx*tx + ty*ty <= 20 {
    let i = (tx + 4) * 9 + ty + 4;
    return cache[inline i];
}
```

Note that these `inline` index expressions are currently not supported on the left-hand-side of an assignment, i.e. `cache[inline i] = 12;` doesn't work.

## Inline functions

You can mark functions `inline` to make the compiler inline them:
```rust
fn inline manhattanDistance(a: MapLocation, b: MapLocation): i32 {
    abs(a.x - b.x) + abs(a.y - b.y)
}
```

If the arguments are constant, they will be propagated throughout the function body, so you can e.g. access static arrays with indices that depend on the function arguments as long as the function is only ever called with constant arguments (e.g. in unrolled loops is fine). Note that `return` is not allowed in `inline` functions.
