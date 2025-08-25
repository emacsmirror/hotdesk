# hotdesk

[![MELPA](https://melpa.org/packages/hotdesk-badge.svg)](https://melpa.org/#/hotdesk)

`hotdesk` is a non-intrusive (stay out the way) Emacs minor mode providing
multiple buffer lists. This is preferable to navigating Emacs standard
monolithic buffer list when you have a lot of buffers open simultaneously.

The motivation for `hotdesk` was to allow a global org clock across multiple
frames, without losing the sensation of independent Emacs instances.

There are two main useful scenarios:

* **Standalone Emacs (single frame)** switch between buffer lists to instantly swap
your current workspace.

* **Client-Server Emacs (multi frame)** assign different buffer lists to different
frames to maintain distinct workspaces.

Buffer lists grow automatically based on your usage, so don't interfere with
your workflow or require configuration.

### How it works

In essence `hotdesk` provides alternatives for two core Emacs functions:

  | Emacs function     | Emacs key binding | Hotdesk alernative function  |  Hotdesk key binding |
  | ---                | ---               | ---                          | ---                  |
  | `list-buffers`     | `C-x C-b`         | `hotdesk-show-listing`       | `C-c C-d C-b`        |
  | `switch-to-buffer` | `C-x b`           | `hotdesk-switch-buffer`      | `C-c C-d b`          |

The frame/buffer tracking mechanism is simple:

  * a frame can be assigned an identifying label
  * a buffer can be assigned multiple labels to determine in which frame(s) it
    participates

Multiple frames can use the same label.

## Getting Started

The code was tested in Emacs 29, but will probably work on older versions (since
the implementation is simple). Integration with packages like projectile or ivy
is untested.

### Installation

You can install via [MELPA](http://melpa.org) with the following command:

`M-x package-install [RET] hotdesk [RET]`

or manually:

1. Clone the repository:

   ```bash
   git clone https://github.com/j-hotlink/hotdesk.git
   ```

2. Add the directory to your Emacs load-path and require the package:

   ```elisp
   (add-to-list 'load-path "/path/to/hotdesk")
   (require 'hotdesk)
   ```

### Enabling hotdesk mode

Activate `hotdesk` mode globally:

```elisp
(hotdesk-mode 1)
```

Now the mode can retain an independent buffer list per label.

## Usage

### 1. Labelling frames

Labelling a frame enables it to participate in `hotdesk`, and is the only
requirement for the mode to work.

Once a frame is labelled, the buffer list associated with that label is used by
the frame. Changing the label switches to a different buffer list, and removing
the label reverts to Emacs standard buffer list.

You can interactively manage the current frame's label with:

 * `C-c C-d ?` show the current frame's label
 * `C-c C-d !` set the current frame's label
 * `C-c C-d $` delete the current frame's label
 * `C-c C-d @` set the current frame's title (for cosmetic purposes)

Programatically you can set the label for a frame (eg. your 'personal' frame)
on startup with:

``` elisp
(hotdesk--frame--set-label (selected-frame) 'personal)
```

or label and title a frame simultaneously with:

``` elisp
(hotdesk--frame--init (selected-frame) "personal" 'personal)
```

If you launch Emacs clients using named bash files (eg. `emacs-personal`,
`emacs-work`), this startup file is helpful to initalise the frame based
on the filename:

``` bash
#!/bin/bash
name=$(basename $0)
frame="${name#emacs-}"
exec emacsclient -n -c -s server --eval "(hotdesk--frame--init (selected-frame) \"${frame}\" (quote ${frame})))"
```

### 2. Labelling buffers

When you visit a globally new buffer within a frame, the frame's label is
automatically assigned to the buffer, building associations based on your usage.

Normally that's all you need, however you can view and edit the labels assigned
to buffers directly with:

 * `C-c C-d l` open the list editor to manage the current buffer's labels, eg.

        family.org  Label
        [ ]         work
        [x]         personal
 
 * `C-c C-d =` show the current buffer's labels
 * `C-c C-d +` add current frame label to the current buffer
 * `C-c C-d -` remove current frame label from the current buffer, and hide the buffer without killing

Add and remove are useful because `hotdesk` doesn't prevent you (or Emacs)
visiting any buffer you like within any frame.

For a global view of labels, the grid editor provides a matrix allowing you
to easily modify assignments:

 * `C-c C-d g` open the grid editor to manage labels globally, eg.

        *Buffer* ▼                       [1]project   [1]work    [1]personal
        *Buffer List: personal*
        *Completions*                    [ ]          [ ]        [x]
        *Hotdesk Grid Editor*            [ ]          [ ]        [x]
        *Messages*                       [ ]          [ ]        [ ]
        *scratch*                        [ ]          [ ]        [x]
        *shell*                          [x]          [x]        [x]
        README.md                        [ ]          [ ]        [x]
        family.org                       [ ]                     [x]
        general.org                      [x]          [ ]        [x]
        holidays.org                     [ ]                     [x]
        meetings.org                     [ ]          [x]        [ ]
        projects.org                     [ ]          [x]
        specification.org                [x]          [ ]        [ ]
        work.org                         [ ]          [x]

The `[1]` in the heading indicates the number of frames assigned that label.
Within the grid editor you can also toggle a 'Major Mode' column with `m`.

### 3. Navigating buffers

The `hotdesk` equivalents of Emacs list and switch buffer commands, utilising
the currently assigned frame label are:

 * `C-c C-d C-b` list the buffers for the current frame's label
 * `C-c C-d b` switch to a buffer belonging to the current frame's label

By default `hotdesk` now reassigns Emacs `C-x C-b` and `C-x b` key bindings to
these same functions (to quickstart demo the mode upon installation). You can
disable this behaviour on mode start by customising the
`hotdesk-overload-default-keys` variable, or in your init:

``` elisp
(setq hotdesk-overload-default-keys nil)
```

Emacs' global buffer list named `*Buffer List*` remains available and unchanged
via `M-x list-buffers`. Label specific buffer lists produced by `hotdesk` are
named `*Buffer List: <label>*`.

You can always switch to *any* buffer you like by providing it's name in
full during switching, or using `M-x switch-to-buffer` directly.

### 4. Saving the desktop

If you use Emacs' built-in `desktop`, `hotdesk` will persist it's labels with
`desktop-save` and restore them with `desktop-read`, preserving your
label assignments across sessions.

#### Persisting frame titles

The default `M-x desktop-save` does not save frame titles, which can be annoying
when restoring multiple frames. You can solve this by adding the following to
your init sequence:

```elisp
(push '(name . nil) frameset-filter-alist)
```

#### Persisting shell buffers

Standard `desktop-save` does not save and restore your shell buffers, which is
a handy facility to have. The following snippet in your init sequence will
add support for this (preserving only the current folder, not the full buffer
content):

```elisp
(require 'desktop)

;;
;;  save shell buffers
;;
(defun hotdesk-desktop-save-shell-buffer ( _dirname )
  "Determine necessary data to save the current shell buffer."
  `(:dir ,default-directory))

(defun hotdesk-desktop-shell-mode-hook ()
  "Setup saving of shell buffers."
  (setq-local desktop-save-buffer 'hotdesk-desktop-save-shell-buffer))

(add-hook 'shell-mode-hook 'hotdesk-desktop-shell-mode-hook)

;;
;;  restore shell buffers
;;
(defun hotdesk-desktop-restore-shell-buffer (_buffer-filename buffer-name buffer-misc)
  "Restore a shell buffer using the provided buffer name and misc data."
  (let ((default-directory (plist-get buffer-misc :dir)))
    (shell buffer-name)))

(add-to-list 'desktop-buffer-mode-handlers
             '(shell-mode . hotdesk-desktop-restore-shell-buffer))
```

## Advanced configuration

### Whitelisting buffers

`hotdesk-buffer-whitelist` holds a list of buffer names to always include in
every buffer list, regardless of labelling. It defaults to `*scratch*` and
`*Messages*`. You can amend in your init with:

```elisp
(with-eval-after-load 'hotdesk
  (add-to-list 'hotdesk-buffer-whitelist "My favourite buffer"))
```

### Restricting labels

The variable `hotdesk-buffer-deny-label-predicates` contains a list of predicate
functions. Before assigning a label to a buffer, each function in the list is
checked. If any returns `t`, then the label assignment is skipped.

Adding your own functions to this list gives you fine-grained control over which
labels can be assigned to which frames - your own permission system. This helps
prevent frame pollution by restricting automated assignments (for example
when org agenda gratuitously visits buffers on startup).

This example applies labelling permissions based on buffer names, modes or
buffer file path and would typically reside in your init sequence:

```elisp
(defun my/hotdesk-deny-label-p (buffer label)
  "Return t to *prevent* assignment of LABEL to BUFFER."
  (let ((name (buffer-name buffer))
        (mode (buffer-local-value 'major-mode buffer))
        (file (buffer-file-name buffer)))
    (cond
     ;; Check for specific label conditions
     ((equal label 'personal)
      (cond ((equal name "work.org") t)
            ((equal name "projects.org") t)
            (t nil)))
     ((equal label 'work)
      (cond ((equal name "family.org") t)
            ((equal name "holidays.org") t)
            (t nil)))
     ;; Restrict elixir mode buffers to the 'elixir frame label
     ((eq mode 'elixir-mode) (not (equal label 'elixir)))
     ;; Restrict ..projects/prototype.. files to the 'prototype frame label
     ((and file (string-match-p "*/projects/prototype/.*" file))
      (not (equal label 'prototype)))
     (t nil))))

(setq hotdesk-buffer-deny-label-predicates
      (append hotdesk-buffer-deny-label-predicates
              (list #'my/hotdesk-deny-label-p)))

```

Denied labels appear as gaps within the editors, eg. grid editor

```
  *Buffer*                         [1]work   [1]personal ▲
  *Hotdesk Grid Editor*            [ ]       [x]
  *scratch*                        [ ]       [x]
  family.org                                 [x]
  general.org                      [x]       [x]
  holidays.org                               [x]
  some-other-file.txt              [ ]       [x]
  *Messages*                       [ ]       [ ]
  projects.org                     [x]
  work.org                         [x]
```

> [!NOTE]
> Preventing label assignments doesn't stop you visiting buffers.

Given the above labelling, visiting `projects.org` on your frame labelled
`personal` (eg. via `org-agena` or by `M-x switch-to-buffer`) presents that
buffer. However as it won't be assigned a `personal` label, it won't appear in
the frame's buffer list, nor be offered for selection when switching buffers. It
will naturally move out of scope as you continue working in the frame, or you
can hide it immediately with `C-c C-d -`.
