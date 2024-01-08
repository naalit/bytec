# Usage and Project Structure

ByteC can be installed by cloning this repository and then running `cargo install --path .` (you'll need Rust installed to use `cargo`); make sure you have cargo's output directory on your `PATH` (this is `~/.cargo/bin` on Linux). After that, you can use the `bytec` command to run the compiler.

The project structure for a ByteC-based Battlecode bot should look something like this (see an [example Battlecode 2022 bot here](https://github.com/naalit/battlecode22)):

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

Then, the CLI syntax for `bytec` looks like `bytec source-path dest-path`, so here it would be (note that files in `bot_one` can still use files in `common`, even though it's not in the command-line arguments):

```
bytec bytec/bot_one src/bot_one
```

This is intended to make it easy to test against alternate versions of your bot - simply compile to a certain output package (the names don't need to match, `bytec` will insert the necessary `package` declarations), make a change, and then compile it again to a different output package and run them against each other. The ability to override [constants](./constants.md) from the command line (`-CModule::SOME_CONST=12`, along the lines of C's `-DSOME_CONST=12`) also helps with that, especially with scripts that might test many possible parameters automatically.

## VSCode extension

There's a VSCode extension for ByteC with syntax highlighting, errors as you type, and autocomplete for class members. The extension is not on the marketplace, unfortunately - to install it, you'll want to symlink the `vscode` folder at the top level of the `bytec` repository to `[VSCODE-DIRECTIORY]/extensions/bytec-basic` - the VSCode directory is `~/.vscode-oss` on Linux. The extension assumes you've installed bytec to `$HOME/.cargo/bin`, which is the default on Linux (and I think also on Mac?).

Alternatively, if you open the `bytec` repository in VSCode and then click F5 (or "Launch Extension" in the Run and Debug menu), it should open a new VSCode window with the extension loaded, without needing to install the extension manually - but you'd need to do that every time you use it, so actually installing the extension is preferred.
