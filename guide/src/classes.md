# Classes and Enums

Classes in bytec (which aren't `extern`) are pretty simple. They can have fields and methods, but custom constructors aren't yet supported.
```rust
class LocInfo {
    let loc: MapLocation;
    let rubble: i32;
    // Fields can have an initial value, but must have an explicit type
    let score: i32 = 0;

    fn set(loc: MapLocation) throws GameActionException {
        self.loc = loc;
        self.rubble = rc.senseRubble(loc);
    }
}
let x = LocInfo();
x.set(rc.getLocation());
```

Enums are a little further from Java enums, and closer to Rust ones. Each variant can have members, which essentially form a tuple:
```rust
enum Action {
    Attack(RobotInfo),
    Envision(AnomalyType),
    Move(Direction),
    Disintegrate;

    fn isGood(): bool {
        match self {
            Disintegrate => false,
            Envision(a) => a != AnomalyType::FURY || !isFriendlyArchonInRange(),
            _ => true,
        }
    }
}
let toTake = Action::Move(Direction::NORTH);
if toTake.isGood() {
    rc.move(Direction::NORTH);
}
```