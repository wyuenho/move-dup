move-dup
========

Minor mode for Eclipse-like moving and duplicating lines or rectangles.

This package offers convenient editing commands much like Eclipse's ability to
move and duplicate lines or selections.

## Commentary

This package offers convenient editing commands much like Eclipse's ability to
move and duplicate lines or rectangular selections.

If you aren't using `package.el` or plan to customize the default key-bindings,
you need to put `move-dup.el` into your Emacs' load-path and `require` it in
your Emacs init file; otherwise you can skip this part.

```elisp
(require 'move-dup)
```

If you don't want to toggle the minor mode, you can bind these functions like
so. All of these functions work on a single line or a rectangle.

```elisp
(global-set-key (kbd "M-<up>") 'md-move-lines-up)
(global-set-key (kbd "M-<down>") 'md-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'md-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'md-duplicate-down)
```

If you used `package.el` to install `move-dup`, this is equivalent to all of the
above.

```elisp
(global-move-dup-mode)
```

You can also turn on `move-dup-mode` individually for each buffer.

```elisp
(move-dup-mode)
```
