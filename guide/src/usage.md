# Usage and Project Structure

ByteC can be installed by cloning this repository and then running `cargo install --path .`; make sure you have cargo's output directory on your `PATH` (this is `~/.cargo/bin` on Linux). After that, you can use the `bytec` command to run the compiler.

The project structure for a ByteC-based Battlecode bot should look something like this:

- `battlecode22` - the Git repository for this year's bots, a clone of `battlecode22-scaffold`
    - `bytec` - this is the directory that all ByteC source files go in; it would be called `src`, but that must be the output directory because it's hardcoded into the Battlecode engine.
        - `common` - modules in here will be shared between all bots.
            - `Common.bt`
            - `Paths.bt`
        - `bot_one`
            - `RobotPlayer.bt`
            - `Soldier.bt`
        - `bot_two`
            - `RobotPlayer.bt`
            - `Miner.bt`
    - `src` - output Java files must go in this directory to be seen by the Battlecode client. This is probably in `.gitignore` so you don't commit these files every time.
        - ...
    - ...

Then, the CLI syntax for `bytec` looks like `bytec source-path1 source-path2... dest-path`, so here it would be:

```
bytec bytec/common bytec/bot_one src/bot_one
```

This is intended to make it easy to test against alternate versions of your bot - simply compile to a certain output package (the names don't need to match, `bytec` will insert the necessary `package` declarations), make a change, and then compile it again to a different output package and run them against each other. A simplified form of C `define`s can also help with this, and you can pass those as `-DSOME_FLAG` or `-DSOME_CONST=12` on the command line.