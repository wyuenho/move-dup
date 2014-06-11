move-dup.el
===========

Eclipse-like moving and duplications of lines or regions with a single key binding.

This package offers convenient editing commands much like Eclipse's ability
to move and duplicate lines or selections.


## Installation

(All of the following are automatically done for you if you've installed via
`package.el`)

```elisp
(require 'move-dup)
(global-set-key (kbd "M-<up>") 'md/move-lines-up)
(global-set-key (kbd "M-<down>") 'md/move-lines-down)
(global-set-key (kbd "C-M-<up>") 'md/duplicate-up)
(global-set-key (kbd "C-M-<down>") 'md/duplicate-down)
```

