# Modules

Using multiple source files ("modules") is generally a good idea.
Items from other files can be accessed with `::` or with `use` (wildcards `Mod::*` are supported!), the same as Rust:

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

Circular dependencies are allowed (at least, *almost* all the time).
Modules can be nested in directories, and module paths are defined relative to the root `bytec` directory that all your source files are in - this lets the compiler search the filesystem for a module when you use it for the first time.
So with the example layout from earlier:
- `bytec`
    - `common`
        - `Common.bt`
        - `Paths.bt`
    - `bot_one`
        - `RobotPlayer.bt`
        - `Soldier.bt`
    - `bot_two`
        - `RobotPlayer.bt`
        - `Miner.bt`

We can use items like so:
```rust
// bot_one/RobotPlayer.bt
use common::Common::RobotController;
use bot_one::Soldier;

fn turn(rc: RobotController) {
    Soldier::turn(rc);
}
```
