# mjolnir-mode
Whosoever holds this hammer, if he be worthy, shall possess the power of Thor.

## About

`mjolnir-mode` is an Emacs minor mode that bestows upon you the might of Thor!
When wielding Mjölnir, nothing shall come in the way of your buffers as they thunder through the windows. However, not all buffers are deemed worthy - the unworthy shall remain immobile, having collapsed under the weight of the mighty Mjölnir.

## Installation

First, clone this repository to your local machine and ensure that it's added to your Emacs `load-path`.

```emacs-lisp
(add-to-list 'load-path "/path/to/mjolnir-mode")
(require 'mjolnir-mode)
```

## Usage

To wield the power of Mjölnir, enable `mjolnir-mode`:

```emacs-lisp
(mjolnir-mode t)
```

To journey all of the realms with it, enable `global-mjolnir-mode`:

```emacs-lisp
(global-mjolnir-mode t)
```

## Buffer Navigation Example

This way (`M-n`):

`+---+---+    +---+---+    +---+---+    +---+---+`  
`| A | B | -> | B | C | -> | C | D | -> | D | A |`  
`+---+---+    +---+---+    +---+---+    +---+---+`  
`| D | C |    | A | D |    | B | A |    | C | B |`  
`+---+---+    +---+---+    +---+---+    +---+---+`

That way (`M-p`):

`+---+---+    +---+---+    +---+---+    +---+---+`  
`| A | B | -> | D | A | -> | C | D | -> | B | C |`  
`+---+---+    +---+---+    +---+---+    +---+---+`  
`| D | C |    | C | B |    | B | A |    | A | D |`  
`+---+---+    +---+---+    +---+---+    +---+---+`

Change your grip using `mjolnir-cycle-window-keys`.

*May your windows be ever steadfast and your code mighty!* ⚡