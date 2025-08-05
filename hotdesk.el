;;; hotdesk.el --- Flexible frame Buffer Lists for projects & workspaces -*- lexical-binding: t; -*-
;;        
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025-2025 Jonathan Doull
;;
;; Author: Jonathan Doull <jonathan@hotlink.technology>
;; Maintainer: Jonathan Doull <jonathan@hotlink.technology>
;; Created: 9 Jul 2025
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "29.3"))
;; Keywords: convenience, frames, local
;; URL: https://github.com/j-hotlink/hotdesk
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `hotdesk-mode' provides unopinionated per-frame buffer lists that adapt with
;; your workflow.
;;
;; A frame may be assigned a label; a buffer may be assigned multiple labels.
;; When a frame and buffer have the same label, they are linked.
;;
;; Buffer labelling is automatic and naturally captures your usage, producing
;; useful frame buffer associations without configuration.  This offers an
;; efficient workflow in multi-frame environments, giving you curated buffer
;; lists you can navigate, preserve and manage per frame(s).
;;
;; A customisable predicate function can prevent combinations of labels and
;; buffers (a rudimentary permission system).  The mode also provides utilities
;; and UI editors for fine-tuning label assignments.
;;
;; Usage:
;;
;;   (require 'hotdesk)
;;   (hotdesk-mode 1)
;;
;; See README.md or the project URL for configuration and advanced commands.
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; version 3.0 or later.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Code:

(require 'cl-lib)
(require 'tabulated-list)

(put 'hotdesk-tag 'permanent-local t)
(defvar-local hotdesk-tag nil
  "Buffer-local list of labels associating buffers to frames.")

(defconst hotdesk--grid-editor-name "*Hotdesk Grid Editor*")
(defconst hotdesk--list-editor-name "*Hotdesk List Editor*")

;;
;;  hotdesk mode
;;
(defvar-keymap hotdesk-list-editor-mode-map
  :doc "Keymap for hotdesk-list-editor-mode."
  :parent tabulated-list-mode-map
  "<SPC>" #'hotdesk--list-editor-toggle-label
  "x"     #'hotdesk--list-editor-toggle-label
  "q"     #'hotdesk--list-editor-quit
  "g"     #'hotdesk-refresh)

(defvar-keymap hotdesk-grid-editor-mode-map
  :doc "Keymap for hotdesk-grid-editor-mode."
  :parent tabulated-list-mode-map
  "m"     #'hotdesk--grid-editor-toggle-mode-column
  "<SPC>" #'hotdesk--grid-editor-toggle-label
  "x"     #'hotdesk--grid-editor-toggle-label
  "RET"   #'hotdesk--grid-editor-visit-buffer
  "q"     #'hotdesk--grid-editor-quit
  "g"     #'hotdesk-refresh)

(defvar-keymap hotdesk-mode-map
  :doc "Keymap for `hotdesk-mode', a global minor mode for per-frame buffer
 groups."
  ;; Do not set :parent for a global minor mode map.
  "C-c C-k ?"   #'hotdesk-get-frame-label
  "C-c C-k !"   #'hotdesk-set-frame-label
  "C-c C-k @"   #'hotdesk-set-frame-title
  "C-c C-k $"   #'hotdesk-del-frame-label
  "C-c C-k C-b" #'hotdesk-show-listing
  "C-c C-k b"   #'hotdesk-switch-buffer
  "C-c C-k g"   #'hotdesk-start-grid-editor
  "C-c C-k l"   #'hotdesk-start-list-editor
  "C-c C-k ="   #'hotdesk-get-buffer-labels
  "C-c C-k +"   #'hotdesk-add-buffer-label
  "C-c C-k -"   #'hotdesk-del-buffer-label)

(defgroup hotdesk nil
  "Frame-centric buffer management for Emacs."
  :prefix "hotdesk-"
  :group 'convenience)

;;;###autoload
(define-minor-mode hotdesk-mode
  "Minor mode for assigning buffers to frames via hotdesk labels."
  :lighter " Hotdesk"
  :keymap hotdesk-mode-map
  :global t
  (if hotdesk-mode
      (progn
        (hotdesk--desktop--enable)
        (hotdesk--mode--buffer-hooks-enable)
        (add-hook 'hotdesk-grid-editor-mode-hook
                  #'hotdesk--grid-editor--mode-on-start)
        (add-hook 'hotdesk-list-editor-mode-hook
                  #'hotdesk--list-editor--mode-on-start)
        (add-hook 'delete-frame-functions
                  #'hotdesk--mode-on-frame-deleted))
    (progn
      (hotdesk--desktop--disable)
      (hotdesk--mode--buffer-hooks-disable)
      (remove-hook 'hotdesk-grid-editor-mode-hook
                   #'hotdesk--grid-editor--mode-on-start)
      (remove-hook 'hotdesk-list-editor-mode-hook
                   #'hotdesk--list-editor--mode-on-start)
      (remove-hook 'delete-frame-functions
                   #'hotdesk--mode-on-frame-deleted))))

(defun hotdesk--mode-on-frame-deleted (_frame)
  "Call `hotdesk-refresh' when a FRAME is deleted."
  (hotdesk-refresh))

(defun hotdesk--grid-editor--mode-on-start ()
  "Update minor mode line for hotdesk in grid editor."
  (setq-local minor-mode-alist
              (assq-delete-all 'hotdesk-mode minor-mode-alist)))

(defun hotdesk--list-editor--mode-on-start ()
  "Update minor mode line for hotdesk in list editor."
  (setq-local minor-mode-alist
              (assq-delete-all 'hotdesk-mode minor-mode-alist)))

(defun hotdesk--mode--buffer-hooks-enable ()
  "Activate hooks when mode enabled."
  (add-hook 'after-change-major-mode-hook
            #'hotdesk--mode--buffer-mode-change-hook)
  (add-hook 'buffer-list-update-hook
            #'hotdesk--mode--buffer-list-update-hook))

(defun hotdesk--mode--buffer-hooks-disable ()
  "Deactivate hooks when mode disable."
  (remove-hook 'after-change-major-mode-hook
               #'hotdesk--mode--buffer-mode-change-hook)
  (remove-hook 'buffer-list-update-hook
               #'hotdesk--mode--buffer-list-update-hook))

(defun hotdesk--mode--buffer-mode-change-hook ()
  "On major mode change, append frame label to buffer if not already present."
  (let* ((frame   (last-nonminibuffer-frame))
         (label   (hotdesk--frame--get-label frame))
         (buffer  (current-buffer))
         (name    (buffer-name buffer))
         (listing (hotdesk--listing--get-name frame label)))
    (when (and label (not (equal name listing)))
      (let ((labels (hotdesk--buffer--get-labels buffer)))
        (unless (memq label labels)
          (hotdesk--buffer--add-label buffer label))))))

(defun hotdesk--mode--buffer-list-update-hook ()
  "Refresh hotdesk ui after a buffer update that affects the listing.
Updates are triggerred by functions like `rename-buffer', `kill-buffer' and
`bury-buffer'."
  (hotdesk-refresh))

;;
;;  frame functions
;;
(defun hotdesk--frame--get-label (frame)
  "Return the label currently assigned to FRAME, or nil if none."
  (frame-parameter frame 'hotdesk-tag))

(defun hotdesk--frame--set-label (frame label)
  "Apply LABEL to FRAME."
  (set-frame-parameter frame 'hotdesk-tag label))

(defun hotdesk--frame--del-label (frame)
  "Delete any label assigned to FRAME."
  (modify-frame-parameters frame '((hotdesk-tag . nil))))

(defun hotdesk--frame--get-buffers (frame)
  "Return a list of buffers satisfying all hotdesk predicates for FRAME."
  (let ((label (hotdesk--frame--get-label frame)))
    (seq-filter
     (lambda (buffer)
       (and (hotdesk--buffer--filter-is-user-buffer-p buffer)
            (hotdesk--buffer--filter-is-label-match-p buffer label)))
     (buffer-list))))

(defun hotdesk--frame--init (frame title label)
  "Assign a TITLE and LABEL to FRAME."
  (with-selected-frame frame (set-frame-name title))
  (set-frame-parameter frame 'title title)
  (hotdesk--frame--set-label frame label)
  (hotdesk-refresh))

(defun hotdesk--frame--create (title label)
  "Create a new frame and assign TITLE and LABEL."
  (interactive "sFrame title: \nsFrame label symbol: ")
  (unless hotdesk-mode (user-error "Enable `hotdesk-mode' first"))
  (let ((frame (make-frame `((title . ,title) (minibuffer . t)))))
    (hotdesk--frame--init frame title label)
    frame))

;;
;;  buffer functions
;;
(defcustom hotdesk-buffer-deny-label-predicates
  (list
   #'hotdesk--buffer--deny-label-buffer-list-p
   #'hotdesk--buffer--deny-label-internal-buffer-p)
  "List of predicate functions invoked as (PREDICATE BUFFER LABEL).
If any returns non-nil, LABEL will not be assigned to BUFFER.
Add your own functions to customise labelling rules."
  :type '(repeat function)
  :group 'hotdesk)

(defun hotdesk--buffer--deny-label-buffer-list-p (buffer _label)
  "Return t if BUFFER is a Buffer List buffer."
  (and (string-match-p "\\*Buffer List:" (buffer-name buffer)) t))

(defun hotdesk--buffer--deny-label-internal-buffer-p (buffer _label)
  "Return t if BUFFER is an internal (not user-facing) buffer."
  (not (hotdesk--buffer--filter-is-user-buffer-p buffer)))

(defun hotdesk--buffer--get-labels (buffer)
  "Return the list of labels assigned to BUFFER, or nil if none."
  (with-current-buffer buffer
    (when (local-variable-p 'hotdesk-tag) hotdesk-tag)))

(defun hotdesk--buffer--set-labels (buffer labels)
  "Set BUFFER's label list to LABELS."
  (with-current-buffer buffer
    (setq-local hotdesk-tag labels)))

(defun hotdesk--buffer--has-label (buffer label)
  "Return t if BUFFER has LABEL among its labels, nil otherwise."
  (memq label (hotdesk--buffer--get-labels buffer)))

(defun hotdesk--buffer--add-label (buffer label)
  "Attempt to append LABEL to BUFFER's label list and trigger UI update."
  (when (hotdesk--buffer--allow-label-p buffer label)
    (unless (hotdesk--buffer--has-label buffer label)
      (let ((labels (hotdesk--buffer--get-labels buffer)))
        (hotdesk--buffer--set-labels buffer (cons label labels))
        (hotdesk-refresh label)))))

(defun hotdesk--buffer--del-label (buffer label)
  "Remove LABEL from BUFFER's label list."
  (when (hotdesk--buffer--has-label buffer label)
    (let ((labels (hotdesk--buffer--get-labels buffer)))
      (hotdesk--buffer--set-labels buffer (delq label labels))
      (hotdesk-refresh label))))

(defun hotdesk--buffer--allow-label-p (buffer label)
  "Return non-nil if all predicates permit assignment of LABEL to BUFFER.
Predicates are defined in `hotdesk-buffer-deny-label-predicates'."
  (catch 'blocked
    (dolist (pred hotdesk-buffer-deny-label-predicates)
      (when (functionp pred)
        (condition-case err
            (when (funcall pred buffer label)
              (throw 'blocked nil))
          (error "[hotdesk] label predicate error: %s" err))))
    t))

(defun hotdesk--buffer--other (buffer frame)
  "Return most recent buffer other than BUFFER in FRAME, as per `other-buffer'."
  (interactive)
  (other-buffer buffer t frame))

(defun hotdesk--buffer--filter-is-user-buffer-p (buffer)
  "Return t if BUFFER is a user-facing buffer (no leading space ' *...')."
  (not (string-match-p "^\\ \\*.*$" (buffer-name buffer))))

(defun hotdesk--buffer--filter-is-label-match-p (buffer label)
  "Return t if BUFFER has a LABEL matching the current frame.
Returns nil for unmatched (and unlabelled) buffers."
  (hotdesk--buffer--has-label buffer label))

;;
;;  listing functions (as in the list of buffers for a frame)
;;
(defun hotdesk--listing--get-name (frame label)
  "Generate a unique `buffer-list' name from LABEL and FRAME."
  (if label
      (format "*Buffer List: %s*" label)
    (format "*Buffer List: default-%s*" (frame-parameter frame 'window-id))))

(defun hotdesk--listing--get-buffer-create (frame)
  "Overload Emacs `get-buffer-create' and rename the buffer for FRAME.
Also set the local `revert-buffer-function' to ensure scoped updates.
This needs to done every invocation because Emacs `get-buffer-create' internals
reset this to `tabulated-list-revert'."
  (unless (frame-live-p frame)
    (error "Attempted to refresh buffer list for non-existent frame"))
  (let* ((label       (hotdesk--frame--get-label frame))
         (newname     (hotdesk--listing--get-name frame label))
         (original-fn (symbol-function 'get-buffer-create)))
    (cl-letf (((symbol-function 'get-buffer-create)
               (lambda (name &rest rest)
                 (if (string= name "*Buffer List*")
                     (apply original-fn newname rest)
                   (apply original-fn newname rest)))))
      (let ((buffer (list-buffers-noselect
                     nil
                     (lambda () (hotdesk--frame--get-buffers frame)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local revert-buffer-function
                        (lambda (&rest _)
                          (hotdesk--listing--rebuild frame (current-buffer)))))
          buffer)))))

(defun hotdesk--listing--rebuild (frame buffer)
  "Reinitialise the contents of BUFFER to match the buffer list for FRAME."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (tabulated-list-init-header)
      (hotdesk--listing--get-buffer-create frame)
      (goto-char (point-min))
      (tabulated-list-print t))))

(defun hotdesk--listing--refresh (&optional label)
  "Refresh the visible listing windows (buffer lists) for LABEL frames.
If LABEL is nil, applies to all frames.  Invoking `revert-buffer'
delegates to the `revert-buffer-function' prepared by
 `hotdesk--listing--get-buffer-create', specifically
 `hotdesk--listing--rebuild'."
  (let ((labels (if (null label) (hotdesk--get-all-labels) (list label))))
    (dolist (frame (frame-list))
      (let ((label (hotdesk--frame--get-label frame)))
        (when (or (null labels) (memq label labels))
          (let ((win (get-buffer-window
                      (hotdesk--listing--get-name frame label) frame)))
            (when win
              (with-selected-window win
                (revert-buffer :ignore-auto :noconfirm)))))))))

;;
;;  desktop integration
;;
(defun hotdesk--advice-desktop-read (orig-fun &rest args)
  "Passthrough invoke `desktop-read' ORIG-FUN using ARGS.
Disable buffer hooks during `desktop-read' to permit external labelling."
  (hotdesk--mode--buffer-hooks-disable)
  (unwind-protect
      (apply orig-fun args)
    (hotdesk--mode--buffer-hooks-enable)))

(defun hotdesk--desktop--enable ()
  "When desktop features available, persist labels and manage hook advice."
  (when (fboundp 'desktop-read)
    (advice-add 'desktop-read :around #'hotdesk--advice-desktop-read))
  (when (boundp 'desktop-locals-to-save)
    (unless (memq 'hotdesk-tag desktop-locals-to-save)
      (add-to-list 'desktop-locals-to-save 'hotdesk-tag))))

(defun hotdesk--desktop--disable ()
  "When desktop features available, undo label persist and advice."
  (when (fboundp 'desktop-read)
    (advice-remove 'desktop-read #'hotdesk--advice-desktop-read))
  (when (boundp 'desktop-locals-to-save)
    (setq desktop-locals-to-save
          (remove 'hotdesk-tag desktop-locals-to-save))))

;;
;;  grid editor
;;
(defvar hotdesk--grid-editor-show-mode-column nil
  "Indicates whether the grid editor should show the major mode column.")

(defun hotdesk--grid-editor--calculate-label-counts ()
  "Calculate the number of frames using each label."
  (mapcar (lambda (label)
            (cons label
                  (length (cl-remove-if-not
                           (lambda (frame)
                             (eq (hotdesk--frame--get-label frame) label))
                           (frame-list)))))
          (hotdesk--get-all-labels)))

(defun hotdesk--grid-editor--heading (label count)
  "Format a column heading with LABEL and its COUNT."
  (format "[%d]%s" count (symbol-name label)))

(defun hotdesk--grid-editor--setup-columns ()
  "Setup tabulated list columns sized according to label length.
Optionally include a column for major modes if enabled."
  (let* ((labels  (hotdesk--grid-editor--calculate-label-counts))
         (columns (vconcat
                   (vector (list "Buffer" 35 t))
                   (when hotdesk--grid-editor-show-mode-column
                     (vector (list "Major Mode" 30 t)))
                   (cl-mapcar
                    (lambda (label-count)
                      (let* ((label (car label-count))
                             (count (cdr label-count))
                             (name  (hotdesk--grid-editor--heading label count))
                             (width (+ 2 (string-width name))))
                        (list name width t)))
                    labels))))
    (setq tabulated-list-format columns)
    (tabulated-list-init-header)))

(defun hotdesk--grid-editor--label-index (col)
  "Return zero-based label index for column COL."
  (let* ((columns tabulated-list-format)
         (widths  (mapcar (lambda (c) (nth 1 c)) columns)))
    (let ((col-offset (if hotdesk--grid-editor-show-mode-column 2 1)))
      (cl-loop for idx from 0 below (length widths)
               for start = (cl-reduce
                            #'+ (cl-subseq widths 0 idx) :initial-value 0)
               for end   = (+ start (nth idx widths))
               if (and (>= col start) (< col end))
               return (- idx col-offset)))))

(defun hotdesk--grid-editor--render-row (buffer labels)
  "Return a grid editor row for BUFFER with frame labels LABELS.
Include the major mode if the column is enabled."
  (let* ((buffer-name (buffer-name buffer))
         (mode-column (when hotdesk--grid-editor-show-mode-column
                        (list (symbol-name
                               (with-current-buffer buffer major-mode)))))
         (label-columns
          (mapcar
           (lambda (l)
             (cond
              ((not (hotdesk--buffer--allow-label-p buffer l)) "   ")
              ((hotdesk--buffer--has-label buffer l)           "[x]")
              (t                                               "[ ]")))
           labels)))
    (list buffer (apply #'vector (cons buffer-name
                                      (append mode-column label-columns))))))

(defun hotdesk--grid-editor-toggle-mode-column ()
  "Toggle the visibility of the major mode column in the grid editor."
  (interactive)
  (setq hotdesk--grid-editor-show-mode-column
        (not hotdesk--grid-editor-show-mode-column))
  (hotdesk-refresh))

(defun hotdesk--grid-editor--refresh ()
  "Refresh the contents of the current grid editor buffer."
  (let ((inhibit-read-only t))
    (hotdesk--grid-editor--setup-columns)
    (let* ((col0            (current-column))
           (row0            (line-number-at-pos))
           (buffers         (hotdesk--get-all-user-buffers))
           (flabels         (hotdesk--get-all-labels)))
      (setq tabulated-list-entries
            (mapcar (lambda (buffer)
                      (hotdesk--grid-editor--render-row buffer flabels))
                    buffers))
      (tabulated-list-print t)
      (goto-char (point-min))
      (forward-line (1- row0))
      (move-to-column col0))))

(define-derived-mode hotdesk-grid-editor-mode tabulated-list-mode
  "Hotdesk Grid Editor"
  "Editor for assigning buffer labels globally.

All defined labels (applied to either frames or buffers) are shown in columns.
The number '[n]' in the heading row gives the number of frames using that label.

The editor is contolled with:
\\{hotdesk-grid-editor-mode-map}"
  :interactive nil
  (hotdesk--grid-editor--setup-columns)
  (hotdesk--grid-editor--refresh))

(defun hotdesk--grid-editor-visit-buffer ()
  "Open selected buffer and assign current frame's label to it."
  (interactive)
  (let* ((buffer (tabulated-list-get-id))
         (label  (hotdesk--frame--get-label (selected-frame))))
    (when (and buffer label)
      (with-current-buffer buffer
        (hotdesk--buffer--add-label buffer label))
      (hotdesk-refresh label)
      (switch-to-buffer-other-window buffer))))

(defun hotdesk--grid-editor-toggle-label ()
  "Toggle assignment between buffer and frame-label at point."
  (interactive)
  (let* ((grid   (selected-window))
         (buffer (tabulated-list-get-id))
         (col    (current-column))
         (labels (hotdesk--get-all-labels))
         (index  (hotdesk--grid-editor--label-index col)))
    (when (and buffer (>= index 0) (< index (length labels)))
      (let ((label (nth index labels)))
        (when (and label (hotdesk--buffer--allow-label-p buffer label))
          (if (hotdesk--buffer--has-label buffer label)
              (hotdesk--buffer--del-label buffer label)
            (hotdesk--buffer--add-label buffer label))
          (hotdesk-refresh label)
          (select-window grid)
          (tabulated-list-print t)
          (move-to-column col))))))

(defun hotdesk--grid-editor-quit ()
  "Quit the hotdesk grid editor by killing its buffer."
  (interactive)
  (let ((buffer (get-buffer hotdesk--grid-editor-name)))
    (when (and buffer
               (with-current-buffer buffer
                 (eq major-mode 'hotdesk-grid-editor-mode)))
      (kill-buffer buffer))))

;;
;;  list editor
;;
(defvar hotdesk--list-editor--source-buffer nil
  "Buffer that is being edited in the current Hotdesk list editor session.")

(defun hotdesk--list-editor--render-row (source label)
  "Render a row for the list editor, indicating LABEL status for SOURCE buffer."
  (let ((allowed (hotdesk--buffer--allow-label-p source label))
        (has (hotdesk--buffer--has-label source label)))
    (list label
          (vector (cond ((not allowed) "   ")
                        (has            "[x]")
                        (t              "[ ]"))
                  (symbol-name label)))))

(defun hotdesk--list-editor--refresh ()
  "Rebuild the editor content with current frame labels."
  (let ((editor (get-buffer hotdesk--list-editor-name)))
    (when (and editor (buffer-live-p editor))
      (with-current-buffer editor
        (when (eq major-mode 'hotdesk-list-editor-mode)
          (if (buffer-live-p hotdesk--list-editor--source-buffer)
              (let* ((source hotdesk--list-editor--source-buffer)
                     (bufname (buffer-name source))
                     (col-width (+ 1 (max 9 (string-width bufname))))
                     (format (vector
                              (list bufname col-width t)
                              (list "Label" 25 t)))
                     (flabels (hotdesk--get-all-labels)))
                (if (null flabels)
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert "No labels defined, assign a frame label."))
                  (progn
                    (let ((entries
                           (mapcar
                            (lambda (label)
                              (hotdesk--list-editor--render-row source label))
                            flabels)))
                      (setq tabulated-list-format format)
                      (setq tabulated-list-entries nil)
                      (tabulated-list-init-header)
                      (setq tabulated-list-entries entries)
                      (tabulated-list-print t)))))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Source buffer is no longer available."))))))))

(define-derived-mode hotdesk-list-editor-mode tabulated-list-mode
  "Hotdesk List Editor"
  "Editor for interactively toggling a buffer's label assignments.

The editor is contolled with:
\\{hotdesk-list-editor-mode-map}"
  :interactive nil
  (tabulated-list-init-header))

(defun hotdesk--list-editor-toggle-label ()
  "Toggle frame-label assignment for the label at point, if allowed."
  (interactive)
  (let* ((label (tabulated-list-get-id))
         (buffer hotdesk--list-editor--source-buffer))
    (when (and label buffer (buffer-live-p buffer)
               (hotdesk--buffer--allow-label-p buffer label))
      (if (hotdesk--buffer--has-label buffer label)
          (hotdesk--buffer--del-label buffer label)
        (hotdesk--buffer--add-label buffer label))
      (hotdesk-refresh))))

(defun hotdesk--list-editor-quit ()
  "Quit the hotdesk list editor by killing its buffer."
  (interactive)
  (let ((buffer hotdesk--list-editor-name))
    (when (get-buffer buffer)
      (kill-buffer buffer))))

;;
;;  hotdesk
;;
(defcustom hotdesk-frame-title-format "Hotdesk - %s"
  "Format string for frame title, where %s represents the frame's label."
  :type 'string
  :group 'hotdesk)

(defun hotdesk--get-all-user-buffers ()
  "Return the global `buffer-list' filtered for user buffers."
  (seq-filter #'hotdesk--buffer--filter-is-user-buffer-p (buffer-list)))

(defun hotdesk--get-all-labels ()
  "Return a sorted list of unique labels from both frames and buffers."
  (let ((frame-labels
         (delete-dups
          (delq nil (mapcar #'hotdesk--frame--get-label (frame-list)))))
        (buffer-labels
         (delete-dups
          (apply #'append
                 (mapcar (lambda (buffer)
                           (hotdesk--buffer--get-labels buffer))
                         (buffer-list))))))
    (sort (delete-dups (append frame-labels buffer-labels)) #'string-lessp)))

(defun hotdesk-get-frame-label ()
  "Identify and display the current frame's label."
  (interactive)
  (message "%S" (hotdesk--frame--get-label (selected-frame))))

(defun hotdesk-set-frame-label ()
  "Set the current frame label, or remove label if user selection is '<none>'.
Input cannot contain spaces.

Assign the new label to the visible windows of the frame, set the frame title
using the new label, and replace any existing \"*Buffer List: <oldlabel>*\"
window with a fresh one \"*Buffer List: <newlabel>*\".

This does not assign the new label to *all* existing buffers of the current
frame (ie. not a rename), which increases flexibility when reorganising labels."
  (interactive)
  (let* ((frame   (selected-frame))
         (options (append (hotdesk--get-all-labels) '("<none>")))
         (old     (or (hotdesk--frame--get-label frame) "<none>"))
         (prompt  (format "Enter label for this frame [%s] (no spaces): " old))
         (filter  (lambda (str) (not (string-match-p " " str))))
         (minibuffer-allow-text-properties t)
         (new     (completing-read prompt options nil nil nil nil nil filter)))
    (cond

     ((string-empty-p new)
      (message "Labelling skipped."))
      
     ((and new (string= new "<none>"))
      (hotdesk--frame--del-label frame)
      (message "Label removed from the current frame.")
      (hotdesk-refresh))

     ((and new (not (string-match-p " " new)))
      (unless (equal old new)
        (let* ((label     (intern new))
               (new-title (format hotdesk-frame-title-format label))
               (old-name  (when (not (string= old "<none>"))
                            (hotdesk--listing--get-name frame old)))
               (new-name  (hotdesk--listing--get-name frame label)))

          ;; Update visible buffers with the new label
          (dolist (win (window-list frame nil))
            (let ((buf (window-buffer win)))
              (when (hotdesk--buffer--allow-label-p buf label)
                (hotdesk--buffer--add-label buf label))))

          ;; Set the new frame label and title
          (hotdesk--frame--set-label frame label)
          (set-frame-name new-title)

          (when old-name
            ;; Replace old buffer associations with new ones
            (dolist (win (window-list frame))
              (when (string= (buffer-name (window-buffer win)) old-name)
                (set-window-buffer win (get-buffer-create new-name))))

            ;; Kill the old buffer if no longer needed
            (let ((old-buffer (get-buffer old-name)))
              (when (and old-buffer (not (memq old (hotdesk--get-all-labels))))
                (kill-buffer old-buffer))))

          ;; Refresh to reflect new frame and buffer label setup
          (hotdesk-refresh label)))))))

(defun hotdesk-del-frame-label ()
  "Remove any label assigned to the current frame."
  (interactive)
  (hotdesk--frame--del-label (selected-frame))
  (message "Label removed from the current frame.")
  (hotdesk-refresh))

(defun hotdesk-set-frame-title ()
  "Prompt for a new frame title.
Default to the format given by `hotdesk-frame-title-format'."
  (interactive)
  (let* ((frame         (selected-frame))
         (label         (or (hotdesk--frame--get-label frame) "none"))
         (default-title (format hotdesk-frame-title-format label))
         (new-title     (read-from-minibuffer
                         (format "Enter new frame title: ") default-title)))
    (set-frame-parameter frame 'title new-title)
    (set-frame-name new-title)))

(defun hotdesk-get-buffer-labels (&optional buffer)
  "Show the labels for BUFFER (defaults to `current-buffer')."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (labels (hotdesk--buffer--get-labels buffer)))
    (message "%S" labels)))

(defun hotdesk-add-buffer-label ()
  "Append the frame label to the current buffer."
  (interactive)
  (let* ((frame (selected-frame))
         (label (hotdesk--frame--get-label frame)))
    (when label
      (hotdesk--buffer--add-label (current-buffer) label))))

;;
;;   this should utilise the hotdesk buffer list, not the global one
;;
(defun hotdesk-del-buffer-label ()
  "Remove the frame label from the current buffer."
  (interactive)
  (let* ((frame (selected-frame))
         (label (hotdesk--frame--get-label frame)))
    (when label
      (let ((buffer (current-buffer)))
        (bury-buffer buffer)
        (switch-to-buffer (hotdesk--buffer--other buffer frame))
        (hotdesk--buffer--del-label buffer label)))))

(defun hotdesk-switch-buffer ()
  "Switch to a user specified buffer (similar to Emacs `switch-to-buffer').
The buffers offerred for selection are restricted to those labelled for
`selected-frame'."
  (interactive)
  (let* ((frame    (selected-frame))
         (current  (current-buffer))
         (buffers  (seq-remove
                    (lambda (b) (eq b current)) (hotdesk--frame--get-buffers frame)))
         (names    (mapcar #'buffer-name buffers))
         (default  (buffer-name (hotdesk--buffer--other current frame)))
         (default? (and (member default names) default))
         (prompt   (if default?
                       (format "Switch to Buffer (default %s): " default)
                     "Switch to Buffer: "))
         (buf-name (completing-read prompt names
                                   nil  ; predicate
                                   nil  ; require-match
                                   nil nil default)))
    (when buf-name
      (switch-to-buffer buf-name))))

(defun hotdesk-show-listing ()
  "Display or refresh the current frame-specific buffer list."
  (interactive)
  (let ((frame (selected-frame)))
    (if (frame-live-p frame)
        (let ((buffer (hotdesk--listing--get-buffer-create frame)))
          (display-buffer buffer '(display-buffer-pop-up-window)))
      (message "The frame is invalid or has been deleted."))))

(defun hotdesk-start-grid-editor ()
  "Display a global interactive buffer-to-frame-label assignment grid."
  (interactive)
  (let ((buffer (get-buffer-create hotdesk--grid-editor-name)))
    (with-current-buffer buffer
      (hotdesk-grid-editor-mode))
    (pop-to-buffer buffer)))

(defun hotdesk-start-list-editor ()
  "Interactively toggle label assignment for the current buffer."
  (interactive)
  (let* ((source (current-buffer))
         (editor (get-buffer-create hotdesk--list-editor-name)))
    (with-current-buffer editor
      (hotdesk-list-editor-mode)
      (setq-local hotdesk--list-editor--source-buffer source)
      (hotdesk-refresh))
    (pop-to-buffer editor)))

(defun hotdesk-refresh (&optional label)
  "Update listing buffer and LABEL related UI buffers."
  (interactive)
  (progn
    (hotdesk--listing--refresh label)
    (let ((list (get-buffer hotdesk--list-editor-name)))
      (when (buffer-live-p list)
        (with-current-buffer list
          (hotdesk--list-editor--refresh))))
    (let ((grid (get-buffer hotdesk--grid-editor-name)))
      (when (buffer-live-p grid)
        (with-current-buffer grid
          (hotdesk--grid-editor--refresh))))))

(provide 'hotdesk)
;;; hotdesk.el ends here
