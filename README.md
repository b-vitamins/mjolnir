# Mjolnir

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

### Visible Realm (Window Rotation)

Go this way (`M-n`):

```
+----+----+    +----+----+    +----+----+    +----+----+
| A* | B  |    | B* | C  |    | C* | D  |    | D* | A  |
+----+----+ -> +----+----+ -> +----+----+ -> +----+----+
| D  | C  |    | A  | D  |    | B  | A  |    | C  | B  |
+----+----+    +----+----+    +----+----+    +----+----+
```

Or that way (`M-p`):

```
+----+----+    +----+----+    +----+----+    +----+----+
| A* | B  |    | D* | A  |    | C* | D  |    | B* | C  |
+----+----+ -> +----+----+ -> +----+----+ -> +----+----+
| D  | C  |    | C  | B  |    | B  | A  |    | A  | D  |
+----+----+    +----+----+    +----+----+    +----+----+
```

Make `X` unworthy (`M-u`), then go this way (`M-n`):

```
+----+----+    +----+----+    +----+----+    +----+----+
| A* | B  |    | B* | C  |    | C* | A  |    | A* | B  |
+----+----+ -> +----+----+ -> +----+----+ -> +----+----+
| X  | C  |    | X  | A  |    | X  | B  |    | X  | C  |
+----+----+    +----+----+    +----+----+    +----+----+
```

### Hidden Realm (Buffer Summoning)

Summon buffers not visible in any window!

Summon next (`M-N`):

```
+----+----+    +----+----+    +----+----+    +----+----+
| A* | B  |    | G* | B  |    | F* | B  |    | E* | B  |
+----+----+ -> +----+----+ -> +----+----+ -> +----+----+
| D  | C  |    | D  | C  |    | D  | C  |    | D  | C  |
+----+----+    +----+----+    +----+----+    +----+----+
  E  F  G        A  E  F        G  A  E        F  G  A
```

Summon previous (`M-P`):

```
+----+----+    +----+----+    +----+----+    +----+----+
| A* | B  |    | E* | B  |    | F* | B  |    | G* | B  |
+----+----+ -> +----+----+ -> +----+----+ -> +----+----+
| D  | C  |    | D  | C  |    | D  | C  |    | D  | C  |
+----+----+    +----+----+    +----+----+    +----+----+
  E  F  G        F  G  A        G  A  E        A  E  F
```

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `M-n` | `mjolnir-rotate-forward` | Rotate buffers forward through windows |
| `M-p` | `mjolnir-rotate-backward` | Rotate buffers backward through windows |
| `M-N` | `mjolnir-summon-next` | Summon next hidden buffer |
| `M-P` | `mjolnir-summon-previous` | Summon previous hidden buffer |
| `M-u` | `mjolnir-toggle-worthy` | Mark window/buffer as worthy/unworthy |
| `M-o` | `mjolnir-invoke` | Show transient menu (if available) |

## Customization

### Key Bindings

Change your grip by customizing these variables:

```emacs-lisp
(setq mjolnir-rotate-forward-key "C-M-n"
      mjolnir-rotate-backward-key "C-M-p"
      mjolnir-summon-next-key "C-M-N"
      mjolnir-summon-previous-key "C-M-P"
      mjolnir-toggle-worthy-key "C-M-u"
      mjolnir-invoke-key "C-M-o")
```

### Unworthy Modes

Certain modes are automatically excluded from rotation:

```emacs-lisp
(add-to-list 'mjolnir-unworthy-modes 'dired-mode)
```

### Project-aware Summoning

Limit buffer summoning to current project (requires Emacs 28+):

```emacs-lisp
(setq mjolnir-summon-project-only t)
```

### Display Options

```emacs-lisp
;; Choose display style
(setq mjolnir-display-method 'both)  ; 'echo, 'overlays, or 'both

;; Customize echo area format
(setq mjolnir-echo-format-current "[%s]"
      mjolnir-echo-format-other "%s")

;; Mode-line indicators
(setq mjolnir-worthy-indicator " ⚡"
      mjolnir-unworthy-indicator " ⚔")
```

## Features

- **Visible Realm**: Rotate buffers through visible windows
- **Hidden Realm**: Summon buffers from the void
- **Worthiness System**: Mark windows/buffers as worthy or unworthy
- **Project Support**: Limit summoning to project buffers
- **Mode-line Indicators**: Visual feedback for window worthiness
- **Smart Caching**: Lightning-fast performance
- **Frame-local State**: Independent operation per frame

## Requirements

- Emacs 27.1 or later
- Optional: `transient.el` for the visual menu

## Development

Run `./setup-dev.sh` as root to install development dependencies and fetch pre-commit hooks. After running it once with internet access, you can work offline. Use `pre-commit run --all-files` to verify changes before committing.

## License

GPL-3.0-or-later

## Author

Ayan Das <bvits@riseup.net>
