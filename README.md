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

Every level is dealt from a fixed seed and comes with an order that wins it.
`src/tune.lisp` works these out ahead: a search over deployment orders, which
follows each box to its slot and lets its ants take the cubes nearest where
they come in, finds orders that clear the plate; each is played out in the
real game, ants walking and resting, and the fastest that wins is improved a
tap at a time. Ants are chaotic -- a frame a little longer or shorter can
send one to a different cube -- so an order counts only if it wins at the
frame steps of a 60Hz screen, the same at double speed, and a 120Hz screen;
its slowest time is the picture's par. `src/test.lisp` plays every level's
order again and fails unless each wins in its par.

Clearing a picture unlocks the next; the level counter at the top left opens
a picker of the unlocked ones, twenty to a page, and the app reopens where it
was left. The 2× button at the top right doubles the ants' speed. A won round
earns one to three stars, by its time against the par, and the picker shows
the best; the round's time and the par show between the picture and the
slots. The ? button beside the slots points at a box to tap, three times a
round, and a hinted round earns two stars at most. While the player keeps to
the level's order, a hint is the next tap of it; off it, the search runs from
where the round stands, and an order it finds is played out before the hint
believes it; that can take a second or two, under a note saying so. The
settings button, left of the slots, turns off the scoring -- then there is no
timer, the picker shows no stars, and a cleared picture just says so -- the
sound, and the vibration: a soft tick as each ant goes home, a firmer one as
a box empties, and the system's success and error patterns as a round is won
or lost. The sounds mix with the player's music, so the phone's Silent mode
silences them. The restart button, under the settings button, starts the
round again. The board is laid out at 390x800 points and scaled to fit the
screen's safe area.

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
| `src/package.lisp` | the package |
| `src/model.lisp` | the rules, in plain Lisp: levels, dealing, reachability, ants |
| `src/view.lisp` | the SpriteKit scene, taps, and an autoplay mode for development |
| `src/test.lisp` | plays every level headless, and fails if one can't be won: `sbcl --script src/test.lisp` |
| `src/tune.lisp` | works out each level's order and par, and how hard it is: `sbcl --script src/tune.lisp [name ...]` |
| `tools/make-icon.py` | draws `res/Ants.xcassets` with the standard library |
| `tools/make-header.py` | draws `docs/header.png`, the 2400x800 banner; `tools/header-title.swift` sets its title |
| `tools/make-sounds.py` | synthesises `res/sounds/*.wav` with the standard library |
| `tools/testflight.sh` | App Store build, check, validate, upload |

A level is a list of strings, one character per cube; add one to `*levels*` in
`src/model.lisp`, then run `src/tune.lisp` on it for its `:order` and `:par`.
Without an order, a level is dealt from the first seed on whose solver's order
wins, worked out as it starts.

## Development switches

Set in the simulator's environment at launch:

```sh
SIMCTL_CHILD_ANTS_AUTOPLAY=1 xcrun simctl launch booted com.burnsidemk.ants
```

`ANTS_AUTOPLAY=1` plays by itself, `ANTS_AUTOPLAY=careless` picks columns at
random (and usually loses), `ANTS_AUTOPLAY=hints` presses the hint button
before each move while it has hints, `ANTS_AUTOPLAY=astray` taps two columns
at random first, `ANTS_LEVEL=n` starts at level n instead of the saved one,
`ANTS_PICKER=1` opens the level picker, `ANTS_SETTINGS=1` the settings, and
`ANTS_FAST=1` starts at double speed.
