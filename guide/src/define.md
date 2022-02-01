# Define

This is a somewhat hacky system for constants and conditional compilation; it might be replaced at some point.
Currently, it's similar to a simplified version of the C preprocessor.

```rust
define SOME_FLAG;
// the "value" of a define is allowed to be any one token, but 90% of the time it's a literal
define SOME_CONST = 12;

// ifdef can be used to qualify declarations
ifdef SOME_FLAG
fn someFunction() {}

fn otherFunction() {
    // or to qualify statements, usually blocks
    ifdef SOME_FLAG {
        println("SOME_CONST = " + SOME_CONST);
    }
}
```

You can also pass defines on the command line if they're not defined in the source code. For this example, it would be `-DSOME_FLAG -DSOME_CONST=12`.
This can be useful for testing different magic numbers and features in a systematic way.