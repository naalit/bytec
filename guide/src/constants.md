# Constants

Constants replace the old C-like system of `define`s, and are much easier to use (for instance, they're scoped like all other items!). They look like `let` definitions, but using `const` instead of `let`. Unlike a `let` definition, constants cannot be modified, and in fact the value is always inlined into use sites.

**Warning!** The value of a constant is essentially pasted at every use. Generally, a constant should have a value that is known at compile time; otherwise, it may be worse than it looks for performance, as the value is evaluated multiple times. Additionally, something like `const C = getCurrentTime()` is possible, and would have a different value each time it is used, which though convenient is not easily visible. Eventually the compiler is intended to check that constants have compile-time known values, but this has not yet been implemented, so be careful.

```rust
// Main.bt
const SOME_FLAG = true;
const SOME_CONST = 12;

fn someFunction() {
    // This if will go away, since the value of SOME_FLAG is known at compile time
    if SOME_FLAG {
        // SOME_CONST will be replaced with the value 12 during compilation
        println("SOME_CONST = " + SOME_CONST);
    }
}
```

You can also override constants from the command line with `C<module>::<constant>=<value>` - for this example: `-CMain::SOME_FLAG=false -CMain::SOME_CONST=42`.
This can be useful for testing different magic numbers and features in a systematic way.