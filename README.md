This package provides an interface to emacs's `display-buffer` function.
It uses user defined rules to decide is new buffer should be opened in new frame.

For example, the following code forces `sh-mode`, `python-mode` and `lua-mode` inferior buffers be
opened in a new window upon creation:

```lisp
(use-package pop-up-frames
  :config
  (pop-up-frames/add-rule "shell" :major "sh-mode" :newname "\\*shell\\*")
  (pop-up-frames/add-rule "python repl" :major "python-mode" :newname "\\*Python\\*")
  (pop-up-frames/add-rule "lua repl" :major "lua-mode" :newname "\\*lua\\*"))
```

## Installation ##

```lisp
(require 'pop-up-frames)
```

or

```lisp
(use-package pop-up-frames)
```

## Defining rules ##

Rules are defined by `pop-up-frames/add-rule` function.

Syntax is following:

```lisp
(pop-up-frames/add-rule rule-name
			:newname new-name
			:oldname old-name
			:major old-major-mode
			:minor old-minor-mode-list)
```

- `rule-name`: name of the rule. It may be used to remove specific rule by calling
`pop-up-frames/remove-rule` function;
- `new-name`: regexp that matches new buffer name. New buffer is the one that is about to be created
  or switched to;
- `old-name`: regexp that matches old buffer name. Old buffer is the one from which a call was made
- `major`: literal `major-mode` string of the old buffer;
- `minor`: minor mode name or a list of minor mode names enabled in the old buffer. Match is made if
  `minor` is a subset of the minor modes enabled in the old buffer.

Except for `rule-name` all these arguments are optional.

It's helpful to set customize variable `pop-up-frames/verbose` to `t` when creating rules.
This will send matching function arguments to `*Messages*` buffer.

## Other functions ##

- `pop-up-frames/remove-rule`: remove specific rule from list
- `pop-up-frames/clear-rules`: remove all rules
- `pop-up-frames/toggle-inhibit`: inhibit pop-up-frames matching function. Buffers will be opened
  according to the `Windows` customize group settings.
- `pop-up-frames/toggle-always-match`: open all buffers in new
  frames. `pop-up-frames/toggle-inhibit` has precedence over this option
