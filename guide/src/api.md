# Interacting with the Battlecode API

## Overview of Java interop in general

Java interop is done with the `extern` keyword, which can be used in many contexts with roughly the same meaning of "the thing following this is a Java thing".
The simplest way is `extern` blocks, which can be at the top level (e.g. for imports) or in blocks, and can be multiline with braces or single line with quotes:
```rust
extern {
    import battlecode.common.*;
}

fn doStuff() {
    extern "System.out.print(\"Hello, \");";
    extern {
        System.out.println("world!");
    }
}
```

You can mark almost any ByteC name as `pub` to make it visible from Java, as long as it has a type that exists in Java:
```rust
fn pub makeGreeting(x: str): str = "Hello, " + x;
fn greet(pub name: str) {
    extern {
        String greeting = makeGreeting(name);
        System.out.println(greeting);
    }
}
```

However, actually writing inline Java code is very rarely necessary. Usually, you'll define the Java API to interact with and then interact with it.
This uses `extern` classes, enums, and functions, which are generally written just like their non-`extern` variants, but without function bodies. There's also the `constructor` keyword for class constructor prototypes, although only one can be used for a given class. The names of extern functions can be changed by adding an `= "<name>"`, which is especially important for static methods and overloading (neither of which is natively supported by ByteC).
```rust
extern class MapLocation {
    constructor(x: i32, y: i32);
    let x: i32;
    let y: i32;

    fn add(dir: Direction): MapLocation;
    fn distanceSquaredTo(loc: MapLocation): i32;
}

extern enum RobotType {
    ARCHON,
    BUILDER,
    LABORATORY,
    MINER,
    SAGE,
    SOLDIER,
    WATCHTOWER;

    let damage: i32;
    let health: i32;

    fn canAttack(): bool;
    fn canMine(): bool;
}

extern fn bytecodeNum(): i32 = "Clock.getBytecodeNum";
```

## Interacting with the Battlecode API

You'll need to use the Battlecode API to do anything useful, and unfortunately this is a little bit harder than in Java, because ByteC isn't smart enough to find and read the API by itself. You'll probably have a file called something like `Common.bt` or `API.bt` shared by all your bots, which has a bunch of `extern` declarations for the entire Battlecode API - an example is at the end of this page. My API bindings were 300 lines of code in 2022, and automatically generating this from the Javadoc isn't too hard; I may write a dedicated script to do this and include it with ByteC at some point.

Also, most functions will probably require `throws GameActionException`. The `throws` clause is actually entirely ignored by ByteC and just passed on to the Java code, but the Java compiler will complain if you leave these out (that does mean you don't need them for inline functions, though). There's also a command-line flag `-T` to add a `throws` clause to every function - for example, `-TGameActionException` - but it's probably best to annotate individual functions.

You'll also need the RobotPlayer class, but this is actually easier than in Java, since every ByteC file ("module") turns into a Java class with static members. Just make sure you have a file called `RobotPlayer.bt`, with something like this:

```rust
extern {
    import battlecode.common.*;
}

use Common::*;

let rc: RobotController;

// This turns into a static function in Java.
// Don't forget the `pub`, it's necessary for Java code to access this function!
fn pub run(rc2: RobotController) {
    rc = rc2;
    loop {
        extern "try {";

        match rc.getType() {
            ARCHON => Archon::turn(),
            MINER => Miner::turn(),
            SOLDIER => Soldier::turn(),
            BUILDER => Builder::turn(),
            WATCHTOWER => Watchtower::turn(),
            SAGE => Sage::turn(),
            LABORATORY => Laboratory::turn(),
        }

        extern "} catch (Exception e) { e.printStackTrace(); }";

        clockYield();
    }
}
```

Some of the API binding code that corresponds to this example would look like this:

```rust
extern {
    import battlecode.common.*;
}

extern fn clockYield() = "Clock.yield";
extern fn bytecodeNum(): i32 = "Clock.getBytecodeNum";
extern fn bytecodeLeft(): i32 = "Clock.getBytecodesLeft";

extern enum RobotType {
    ARCHON,
    BUILDER,
    LABORATORY,
    MINER,
    SAGE,
    SOLDIER,
    WATCHTOWER;

    let actionCooldown: i32;
    let actionRadiusSquared: i32;
    let buildCostGold: i32;
    let buildCostLead: i32;
    let bytecodeLimit: i32;
    let damage: i32;
    let health: i32;
    let movementCooldown: i32;
    let visionRadiusSquared: i32;

    fn canAttack(): bool;
    fn canMine(): bool;
    fn canBuild(t: RobotType): bool;
    fn canRepair(t: RobotType): bool;
    fn canMutate(t: RobotType): bool;
    fn isBuilding(): bool;

    // etc.
}
extern class MapLocation {
    constructor(x: i32, y: i32);
    let x: i32;
    let y: i32;

    fn add(dir: Direction): MapLocation;
    fn subtract(dir: Direction): MapLocation;
    fn directionTo(loc: MapLocation): Direction;
    fn distanceSquaredTo(loc: MapLocation): i32;
    fn isWithinDistanceSquared(loc: MapLocation, distanceSquared: i32): bool;
    fn translate(dx: i32, dy: i32): MapLocation;
    fn isAdjacentTo(loc: MapLocation): bool;    
}
extern class RobotController {
    // Returns the location adjacent to current location in the given direction.
    fn adjacentLocation(dir: Direction): MapLocation;

    // Attack a given location.
    fn attack(loc: MapLocation);

    // Builds a robot of the given type in the given direction.
    fn buildRobot(type: RobotType, dir: Direction);

    // Note that ByteC doesn't support overloading, so these functions need to have different names but map to the same Java function.

    // Returns all robots within vision radius.
    fn senseNearbyRobots(): [RobotInfo];
    // Returns all robots that can be sensed within a certain distance of this robot.
    fn senseNearbyRobotsR(radiusSquared: i32): [RobotInfo] = "senseNearbyRobots";
    // Returns all robots of a given team that can be sensed within a certain distance of this robot.
    fn senseNearbyRobotsT(radiusSquared: i32, team: Team): [RobotInfo] = "senseNearbyRobots";
    // Returns all robots of a given team that can be sensed within a certain radius of a specified location.
    fn senseNearbyRobotsAt(center: MapLocation, radiusSquared: i32, team: Team): [RobotInfo] = "senseNearbyRobots";

    // etc.
}
```