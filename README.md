Display-buffer-control (`dbc`) package is an interface to emacs's powerful `display-buffer` function.
It allows to specify how the buffers should be opened: should the new buffer be opened in a
new frame or the current frame (emacs frame is a window in terms of window managers); how to
position a new window relative to the current one etc.

For example, the following code forces `*Help*` buffer to be shown in the right side window with 40% width:

```lisp
(dbc-add-ruleset "right" '((display-buffer-in-side-window) . ((side . right) (window-width . 0.4))))
(dbc-add-rule "right" "help" :newname "\\*help\\*")
```

## rules and rulesets ##

`dbc` uses rules and ruleset to describe how the new buffers should be opened.
Rulesets have a `display-buffer` action associated with them.
Actions describe how the new window will be displayed (see `display-buffer` help page for details).

*Example:*

```lisp
(dbc-add-ruleset "pop-up-frame" '((display-buffer-reuse-window display-buffer-pop-up-frame) . ((reusable-frames . 0))))
(dbc-add-ruleset "bottom" '(display-buffer-reuse-window display-buffer-below-selected))
(dbc-add-ruleset "right" '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . right) (window-width . 0.4))))
```

The code above adds three rulesets:

- `pop-up-frame` opens buffer in previously used window or in new frame;
- `bottom` opens buffer in previously used window or on the bottom;
- `right` right sidewindow with 40% window width.

In order to apply these actions to buffers, rules must be added.
Rules specify matching conditions for the ruleset.

*Example:*

```lisp
(dbc-add-rule "pop-up-frame" "python" :newmajor "inferior-python-mode")
(dbc-add-rule "right" "help" :newname "\\*help\\*")
(dbc-add-rule "rightside" "shell" :oldmajor "sh-mode" :newname "\\*shell\\*")
```

The code above adds three rules:

- `python` rule uses `pop-up-frame` ruleset action to open buffers with `inferior-python-mode` major
  mode;
- `help` rule uses `bottom` ruleset action to open `*Help*` buffer;
- `shell` rule uses `rightside` ruleset action to open `*shell*` buffer if opened from `sh-mode`
  major mode buffer.

## ruleset priority ##

Third optional argument to `dbc-add-ruleset` specifies priority of the ruleset.
Priority is an integer from `1` to `1000`, smallest priority gets evaluated first.
Default priority is `500`.
`display-buffer-alist` entries not controlled by dbc also get the default
priority.

## rule matching conditions ##


`dbc-add-rule` has the following syntax:

```lisp
(dbc-add-rule ruleset-name
                                 rule-name
                                 :newname new-name
                                 :newmajor new-major-mode
                                 :newminor new-minor-mode-list
                                 :oldname old-name
                                 :oldmajor old-major-mode
                                 :oldminor old-minor-mode-list)
```

`old` prefix refers to buffer we're switching from, `new` -- switching to.

- `ruleset-name`: name of the ruleset to add rule to;
- `rule-name`: name of the rule;
- `new-name`: regexp that matches new buffers name;
- `new-major`: regexp that matches new buffers major mode;
- `new-minor`: minor mode name or a list of minor mode names enabled in the new buffer
- `old-name`: regexp that matches old buffers name;
- `old-major`: regexp that matches old buffers major mode;
- `old-minor`: minor mode name or a list of minor mode names enabled in the old buffer

All keyword arguments are optional. Empty arguments match all.

*Tip:* Set `dbc-verbose` flag to print arguments to the `*Messages*` buffer when
switching buffers.

## examples ##

Open Lua, Python, R, Julia and shell inferior buffers in new frames, enable
`dbc-verbose` flag:

```lisp
(use-package dbc
  :custom
  (dbc-verbose t)
  :config
  (dbc-add-ruleset "pop-up-frame" '((display-buffer-reuse-window display-buffer-pop-up-frame) . ((reusable-frames . 0))))
  (dbc-add-rule "pop-up-frame" "shell" :oldmajor "sh-mode" :newname "\\*shell\\*")
  (dbc-add-rule "pop-up-frame" "python" :newmajor "inferior-python-mode")
  (dbc-add-rule "pop-up-frame" "ess" :newmajor "inferior-ess-.+-mode")
  (dbc-add-rule "pop-up-frame" "lua repl" :newmajor "comint-mode" :oldmajor
  "lua-mode" :newname "\\*lua\\*"))
```

---

Display help in right side window:

```lisp
(require 'dbc)

(dbc-add-ruleset "rightside" '((display-buffer-in-side-window) . ((side . right) (window-width . 0.4))))
(dbc-add-rule "rightside" "help" :newname "\\*help\\*")
```

---

Display help in right window:

```lisp
(use-package dbc
  :config
  (dbc-add-ruleset "right" '(dbc-actions-right) 300)
  (dbc-add-rule "right" "help" :newname "\\*help\\*"))
```

## installation ##

<key>M-x package-install dbc</key> and then

```lisp
(require 'dbc)
```

or just

```lisp
(use-package dbc)
```

## package functions ##

- `dbc-add-ruleset`: add ruleset
- `dbc-remove-ruleset`: remove ruleset
- `dbc-add-rule`: add rule to ruleset
- `dbc-remove-rule`: remove rule from ruleset
- `dbc-clear-rules`: remove all rules from ruleset
- `dbc-toggle-inhibit`: inhibit display-buffer-control (interactive)
