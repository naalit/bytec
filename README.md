# `bytec`

`bytec` is a compiler from a tiny Rust-like language to bytecode-optimized Java, intended for use in Battlecode.

The language (also called "bytec" for lack of a better name) has a few features Java doesn't, like zero-overhead dynamic arrays and tuples,
and it's fully interoperable with Java so it can be used for the performance-critical parts of a Java bot.

[Read the guide here!](https://naalit.github.io/bytec)

[Or check out this example of a Battlecode 2022 bot written entirely in bytec.](https://github.com/naalit/battlecode22)

### VSCode extension

There's a VSCode extension for ByteC with syntax highlighting, errors as you type, and autocomplete in the `vscode` folder in this repository. The easiest way to enable it is to symlink it to `[VSCODE-DIRECTIORY]/extensions/bytec-basic` - the VSCode directory is `~/.vscode-oss` on Linux. The extension assumes you've installed bytec to `$HOME/.cargo/bin`, which is the default on Linux and I think also on Mac.

## Language tour

Here's a simple player for Battlecode 2021. Compile it with something like `bytec <input directory>/RobotPlayer.bt <output directory>/testplayer/RobotPlayer.java`. (First install it with `cargo install --path .`, and make sure the cargo install directory is on your path (`~/.cargo/bin` on Linux)).

```rust
// bytec isn't smart enough to analyze Java libraries itself, so we have to tell it the parts of the Battlecode API we need
extern enum battlecode::common::RobotType {
    ENLIGHTENMENT_CENTER,
    POLITICIAN,
    SLANDERER,
    MUCKRAKER,
}
extern enum battlecode::common::Direction {
    NORTH,
    SOUTH,
    EAST,
    WEST,
    // We don't actually need to list all these, only the ones we use
    // (but you should list them all if you'll do any pattern matching on it so coverage checking works!)
}
extern class battlecode::common::RobotController {
    // We list the methods we use, with their types
    fn getType(): RobotType;
    fn getInfluence(): i32;

    // We can change the names, too, if we want
    fn canBuild(toBuild: RobotType, dir: Direction, influence: i32): bool = "canBuildRobot";
    fn buildRobot(toBuild: RobotType, dir: Direction, influence: i32): bool;

    fn canMove(dir: Direction): bool;
    fn move(dir: Direction): bool;
}
extern fn yield() = "battlecode.common.Clock.yield";

// We'll need these to pick a random direction
let mut directions: [Direction] = [
    Direction::NORTH,
    Direction::EAST,
    Direction::SOUTH,
];
extern class java::util::Random {
    // A constructor with no arguments
    constructor();
    fn nextInt(max: i32): i32;
}

// Now we write the run() function.
// Bytec code is organized in 'modules', which are created for each file and are really classes with static members.
// So, as long as this is compiled to a file called RobotPlayer.java, this will be RobotPlayer.run() and will work.
// Note the `pub`, which makes it accessible from Java; this can be used anytime a name is declared.
fn pub run(rc: RobotController) {
    // Oops, we forgot to add WEST to our directions!
    // Luckily, it's a dynamic array and can be added to whenever we want.
    directions.push(Direction::WEST);

    let rand = Random();

    // `loop {}` is the same thing as `while true {}`, like Rust
    loop {
        // Bytec doesn't have try-catch, but it can be inserted with inline Java code
        extern "try {";

        match rc.getType() {
            ENLIGHTENMENT_CENTER => {
                let toBuild = RobotType::SLANDERER;
                let influence = rc.getInfluence() / 2;

                // Try to build a robot in the first available direction
                for dir in directions {
                    if rc.canBuild(toBuild, dir, influence) {
                        rc.buildRobot(toBuild, dir, influence);
                    }
                }
            }
            else => {
                // Move in a random direction
                let dir = directions[rand.nextInt(directions.len())];
                if rc.canMove(dir) {
                    rc.move(dir);
                }
            }
        }

        yield();

        extern "} catch (Exception e) { e.printStackTrace(); }";
    }
}
```

## Why not compile to bytecode?

`bytec` would ideally compile directly to Java bytecode, as that gives it a lot more control over specific instructions used.
Unfortunately, the way Battlecode submissions are compiled means that only Java source code can be used.
In fact, it only accepts one package containing only Java source files.
That also means it's impossible to use any libraries in Battlecode bots unless you include the source files and change their package declarations.

I'm probably not going to work on `bytec` during Battlecode itself, but if Battlecode switches to allowing bytecode in submissions in the future, I'll change `bytec` to compile to bytecode before the next year's competition.