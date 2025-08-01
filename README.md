# hotdesk
`hotdesk` is a non-intrusive (stay out the way) Emacs minor mode providing
per-frame buffer lists.

It natually captures your usage to produce useful frame buffer associations
without requiring configuration. This offers an efficient workflow in
multi-frame environments, keeping your curated work spaces clean and distinct,
and alleviates navigating a single monolithic buffer list.

The motivation for `hotdesk` was to allow a global org clock across multiple
frames, without losing the sensation of independent Emacs instances.

In summary `hotdesk` provides alternatives for two core Emacs functions:

  | Emacs function     | Emacs key binding | Hotdesk alernative function  |  Hotdesk key binding |
  | ---                | ---               | ---                          | ---                  |
  | `list-buffers`     | `C-x C-b`         | `hotdesk-show-listing`       | `C-c C-k C-b`        |
  | `switch-to-buffer` | `C-x b`           | `hotdesk-switch-buffer`      | `C-c C-k b`          |

These provide a frame-filtered versions of the Emacs functions.

The frame/buffer tracking mechanism is simple:

  * a frame can be assigned a single identifying label
  * a buffer can be assigned multiple labels to determine the frame(s) in
   which it participates

Multiple frames can use the same label.

## Getting Started

The code was tested in Emacs 29, but will probably work on older versions (since
the implementation is simple). Integration with packages like projectile or ivy
is untested.

### Installation

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

## Usage

### 1. Labelling frames

Labelling a frame enables it to participate in `hotdesk`, and is the only
configuration requirement for the mode to work. You can interactively manage
the current frame's label with:

 * `C-c C-k ?` show the current frame's label
 * `C-c C-k !` set the current frame's label
 * `C-c C-k $` delete the current frame's label
 * `C-c C-k @` set the current frame's title (for cosmetic purposes)

Programatically you can set the label for a frame (eg. your 'personal' frame)
with:

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

Whenever you visit a buffer within a frame, the frame's label is automatically
assigned to the buffer, naturally building frame associations from your usage.

Normally that's all you need, however you can view and edit the labels assigned
to buffers directly with:

 * `C-c C-k l` open the list editor to manage the current buffer's labels, eg.

        family.org  Label
        [ ]         work
        [x]         personal
 
 * `C-c C-k =` show the current buffer's labels
 * `C-c C-k +` add current frame label to the current buffer
 * `C-c C-k -` remove current frame label from the current buffer, and hide the buffer without killing

Add and remove are useful because `hotdesk` doesn't prevent you (or Emacs)
visiting any buffer you like within any frame.

For a global view of labels, the grid editor provides a matrix allowing you
to easily modify assignments:

 * `C-c C-k g` open the grid editor to manage labels globally, eg.

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
Within the grid editor you can toggle a 'Major Mode' column with `m`.

### 3. Navigating buffers

The `hotdesk` equivalents of Emacs list and switch buffer commands, within a
frame scope are:

 * `C-c C-k C-b` list the buffers for the current frame
 * `C-c C-k b` switch to a buffer belonging to the current frame

Emacs' global buffer list named `*Buffer List*` remains available and
unchanged. Frame specific buffer lists produced by `hotdesk` are named
`*Buffer List: <label>*`.

You can always switch to *any* buffer you like by providing it's name in
full during switching, or using `M-x switch-to-buffer` directly.

> [!TIP]
> To leverage your existing key memory and boost your productivity, consider
> swapping the Emacs default key bindings with their `hotdesk` equivalents:
>
``` elisp
(add-hook 'hotdesk-mode-hook
          (lambda ()
            (if hotdesk-mode
                (progn
                  (global-set-key (kbd "C-x C-b") #'hotdesk-show-listing)
                  (global-set-key (kbd "C-x b")   #'hotdesk-switch-buffer))
              (progn
                (global-set-key (kbd "C-x C-b") 'list-buffers)
                (global-set-key (kbd "C-x b")   'switch-to-buffer)))))
```

### 4. Saving the desktop

If you use Emacs `desktop`, `hotdesk` will persist it's labels with
`desktop-save` and restore them with `desktop-read`, preserving your
label assignments across sessions.

The default `M-x desktop-save` does not save frame titles, which can be annoying
when restoring multiple frames. You can solve this by adding the following to
your init sequence:

```elisp
(push '(name . nil) frameset-filter-alist)
```

## Advanced configuration

The variable `hotdesk-buffer-deny-label-predicates` contains a list of predicate
functions. Before assigning a label to a buffer, each function in the list is
checked, invoked via `(fn-p buffer label)`. If any returns `t`, then the label
assignment is skipped.

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

Given the above labelling, visiting `projects.org` on your `personal` frame
(eg. via `org-agena` or by `M-x switch-to-buffer`) will present that buffer.
However as it won't be assigned a `personal` label, it won't appear in the
frame's buffer list, nor be offered for selection when switching frame
buffers. It will naturally move out of scope as you continue working in the
frame, or you can hide it immediately with `C-c C-k -`.
