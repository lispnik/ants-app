# ants-app

A puzzle game for iOS, written in Common Lisp and built with
[asdf-ios-app](https://github.com/lispnik/asdf-ios-app) and
[objc](https://github.com/lispnik/objc). There is no Xcode project: ECL
cross-compiles the Lisp, and SpriteKit draws.

A pixel picture of coloured cubes sits on a plate. Below it is a row of
deployment slots, and below that a queue of ant boxes in columns. Tap the box
at the front of a column and it moves to a free slot; its ants walk out, find a
cube of their colour they can reach -- through the gaps other ants have
already made -- carry it to a hole and jump in. Each ant that jumps takes one
off its box's count, and an empty box frees its slot. Clear the picture to
win. Fill every slot with boxes whose cubes are all still buried and the round
is lost.

Every deal is checked winnable before it is played: a search over deployment
orders confirms that some order clears the plate.

## Building

With `asdf-ios-app`'s toolchain bootstrapped and this directory, `objc` and
`asdf-ios-app` on your ASDF source registry:

```lisp
(asdf:make "ants-app")                          ; => build/iphonesimulator/Ants.app
(asdf-ios-app:run-in-simulator "ants-app")
```

A device build too, when the environment says how to sign it:

```sh
export IOS_SIGNING_IDENTITY="Apple Development: Your Name (XXXXXXXXXX)"
export IOS_DEVELOPMENT_TEAM=TEAMID1234
export IOS_PROVISIONING_PROFILE=/path/to/a/development.mobileprovision
ecl --eval '(asdf:make "ants-app")' --eval '(quit)'   # => build/iphoneos/Ants.app too
```

## TestFlight

`tools/testflight.sh` makes a clean App Store build, checks its signature,
validates it and uploads it. Its signing settings come from `local.env`, which
is not committed; the script's header lists what goes in it.

```sh
tools/testflight.sh              # build number from the clock
tools/testflight.sh --validate   # build and validate, upload nothing
```

## Files

| | |
|---|---|
| `model.lisp` | the rules, in plain Lisp: levels, dealing, reachability, ants |
| `view.lisp` | the SpriteKit scene, taps, and an autoplay mode for development |
| `test.lisp` | plays every level headless: `sbcl --script test.lisp` |
| `tools/make-icon.py` | draws `res/Ants.xcassets` with the standard library |
| `tools/testflight.sh` | App Store build, check, validate, upload |

A level is a list of strings, one character per cube; add one to `*levels*` in
`model.lisp`.

## Development switches

Set in the simulator's environment at launch:

```sh
SIMCTL_CHILD_ANTS_AUTOPLAY=1 xcrun simctl launch booted com.burnsidemk.ants
```

`ANTS_AUTOPLAY=1` plays by itself, `ANTS_AUTOPLAY=careless` picks columns at
random (and usually loses), `ANTS_LEVEL=n` starts at level n.
