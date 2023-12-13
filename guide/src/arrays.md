# Arrays

The two types of arrays, static and dynamic, are a concept that doesn't exist in Java so they deserve some explanation.
A static array has a constant length known at compile time, and it becomes a bunch of separate variables in Java.
Any loop over a static array is unrolled, and indexing the array must either use an index that the compiler can figure out is constant, or use the `inline` keyword to be turned into a `switch`:

```rust
let arr: [i32; 16];
fn getArr(i: i32): i32 {
    arr[inline i]
}
```

A dynamic array, on the other hand, doesn't have a length known at compile time. In fact, the length of the array can change at any moment.
It's similar to a Java `ArrayList`, but all the logic is inlined, so any operations that normal Java arrays support (indexing, index assignment, looping over the array) are exactly as fast, and `push` and `pop` are pretty fast as well and don't involve method calls. Java arrays returned by `extern` functions are automatically converted to ByteC dynamic arrays. (Internally, dynamic arrays are represented as a Java array, which might have open space at the end, and a length.) Here's an example of the operations supported by dynamic arrays:

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