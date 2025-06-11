# mjolnir
Whosoever holds this hammer, if he be worthy, shall possess the power of Thor.

`mjolnir-mode` is an Emacs minor mode that bestows upon you the might of Thor!

When wielding Mjölnir, nothing shall come in the way of your buffers as they thunder through your windows. Instead of moving over to the window holding the buffer worthy of your attention, summon it into the window you're already in. However, you deem not all buffers as worthy - let be them smitten under the might of Mjölnir - and they shall stay their ground.

*May your windows be ever steadfast and your code mighty!* ⚡

## Installation
Clone this repository and add it to your `load-path`.
```emacs-lisp
(add-to-list 'load-path "/path/to/mjolnir")
(require 'mjolnir)
```

## Usage
Enable `mjolnir-mode`:
```emacs-lisp
(mjolnir-mode t)
```

Now wield the power of Mjölnir wherever you are (`*`).

Go this way (`M-n`):

`+----+---+    +----+---+    +----+---+    +----+---+`
`| A* | B |    | B* | C |    | C* | D |    | D* | A |`
`+----+---+ -> +----+---+ -> +----+---+ -> +----+---+`
`| D  | C |    | A  | D |    | B  | A |    | C  | B |`
`+----+---+    +----+---+    +----+---+    +----+---+`

Or that way (`M-p`):

`+----+---+    +----+---+    +----+---+    +----+---+`
`| A* | B |    | D* | A |    | C* | D |    | B* | C |`
`+----+---+ -> +----+---+ -> +----+---+ -> +----+---+`
`| D  | C |    | C  | B |    | B  | A |    | A  | D |`
`+----+---+    +----+---+    +----+---+    +----+---+`

Make `X` unworthy (`C-c u`), then go this way (`M-n`):

`+----+---+    +----+---+    +----+---+    +----+---+`
`| A* | B |    | B* | C |    | C* | A |    | A* | B |`
`+----+---+ -> +----+---+ -> +----+---+ -> +----+---+`
`| X  | C |    | X  | A |    | X  | B |    | X  | C |`
`+----+---+    +----+---+    +----+---+    +----+---+`

Change your grip using `mjolnir-cycle-window-forward-key`, `mjolnir-cycle-window-backward-key`, and `mjolnir-toggle-fixed-window-key`.

## Development
Run `./setup-dev.sh` as root to install development dependencies and fetch
pre-commit hooks. After running it once with internet access, you can work
offline. Use `pre-commit run --all-files` to verify changes before committing.
