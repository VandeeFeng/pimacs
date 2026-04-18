;;; pi-coding-agent-extension.el --- extension for pi-coding-agent -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'pi-coding-agent-ui)
(require 'cl-lib)
(require 'subr-x)

;;; Extension: insert-region
;; File-centric helpers for pi-coding-agent.
;;
;; This module currently provides one user command:
;;   `pi-coding-agent-insert-region'
;;
;; It inserts the active file region into the current pi input buffer and
;; prefixes it with an LLM-friendly location tag like:
;;   @path/to/file.ext#L10-L24

(defvar pi-coding-agent--active-input-buffer nil
  "Globally active pi input buffer for commands triggered from other buffers.")

(defun pi-coding-agent--input-buffer-p (buffer)
  "Return non-nil when BUFFER is a live pi input buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'pi-coding-agent-input-mode))))

(defun pi-coding-agent--visible-input-buffers (&optional frame)
  "Return visible pi input buffers in FRAME.
When FRAME is nil, use the selected frame."
  (let ((target-frame (or frame (selected-frame)))
        result)
    (dolist (window (window-list target-frame 'no-minibuf))
      (let ((buffer (window-buffer window)))
        (when (pi-coding-agent--input-buffer-p buffer)
          (cl-pushnew buffer result))))
    result))

(defun pi-coding-agent--set-active-input-buffer (buffer)
  "Set global active pi input BUFFER.
When BUFFER is nil or invalid, clear active input."
  (setq pi-coding-agent--active-input-buffer
        (and (pi-coding-agent--input-buffer-p buffer)
             buffer)))

(defun pi-coding-agent--get-active-input-buffer ()
  "Return the active pi input buffer, or nil."
  (let* ((selected (window-buffer (selected-window)))
         (visible (pi-coding-agent--visible-input-buffers))
         (live (cl-remove-if-not #'pi-coding-agent--input-buffer-p (buffer-list)))
         (resolved
          (cond
           ((pi-coding-agent--input-buffer-p pi-coding-agent--active-input-buffer)
            pi-coding-agent--active-input-buffer)
           ((pi-coding-agent--input-buffer-p selected)
            selected)
           ((= (length visible) 1)
            (car visible))
           ((= (length live) 1)
            (car live)))))
    (pi-coding-agent--set-active-input-buffer resolved)
    resolved))

(defun pi-coding-agent--files-target-input-buffer ()
  "Return pi input target buffer, or nil."
  (pi-coding-agent--get-active-input-buffer))

(defun pi-coding-agent--files-region-line-range (beg end)
  "Return (START-LINE . END-LINE) for region BEG..END.
END is treated as exclusive, so selecting full lines ending at the next
line's BOL reports the intuitive previous line."
  (let* ((start-line (line-number-at-pos beg))
         (last-pos (if (= beg end) end (max beg (1- end))))
         (end-line (line-number-at-pos last-pos)))
    (cons start-line (max start-line end-line))))

(defun pi-coding-agent--files-reference-path (file session-dir)
  "Return FILE formatted for @reference usage.
When FILE is under SESSION-DIR, return a relative path; otherwise return
an abbreviated absolute path."
  (let ((expanded-file (expand-file-name file))
        (expanded-dir (and session-dir (expand-file-name session-dir))))
    (if (and expanded-dir (file-in-directory-p expanded-file expanded-dir))
        (file-relative-name expanded-file expanded-dir)
      (abbreviate-file-name expanded-file))))

(defun pi-coding-agent--files-code-language (file)
  "Infer fenced code language token from FILE extension."
  (or (file-name-extension file) "text"))

(defun pi-coding-agent--files-format-region-snippet (location text file)
  "Format TEXT from FILE with LOCATION tag for insertion into pi input."
  (let ((lang (pi-coding-agent--files-code-language file)))
    (concat location "\n"
            "````" lang "\n"
            text
            (unless (string-suffix-p "\n" text) "\n")
            "````\n")))

(defun pi-coding-agent--files-insert-into-input (input-buf text)
  "Insert TEXT at point in INPUT-BUF with minimal spacing."
  (with-current-buffer input-buf
    (when (and (> (point) (point-min))
               (not (eq (char-before) ?\n)))
      (insert "\n"))
    (insert text)))

(defun pi-coding-agent--files-current-region-bounds ()
  "Return current buffer region bounds as (BEG . END), or nil."
  (let ((mark-pos (mark t))
        (pt (point)))
    (when (and mark-pos (/= pt mark-pos))
      (cons (min pt mark-pos) (max pt mark-pos)))))

;;;###autoload
(defun pi-coding-agent-insert-region ()
  "Insert current file region into the active pi input as @file#Lx-Ly."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (when (derived-mode-p 'pi-coding-agent-input-mode 'pi-coding-agent-chat-mode)
    (user-error "Run this command from a file buffer"))
  (let* ((bounds (pi-coding-agent--files-current-region-bounds))
         (input-buf (pi-coding-agent--files-target-input-buffer)))
    (unless bounds
      (user-error "No selected region in current file buffer"))
    (unless input-buf
      (user-error "No active pi input buffer"))
    (let* ((region-beg (car bounds))
           (region-end (cdr bounds))
           (range (pi-coding-agent--files-region-line-range region-beg region-end))
           (start-line (car range))
           (end-line (cdr range))
           (session-dir (with-current-buffer input-buf
                          (pi-coding-agent--session-directory)))
           (ref-path (pi-coding-agent--files-reference-path buffer-file-name session-dir))
           (location (format "@%s#L%d-L%d" ref-path start-line end-line))
           (text (buffer-substring-no-properties region-beg region-end))
           (snippet (pi-coding-agent--files-format-region-snippet
                     location text buffer-file-name)))
      (pi-coding-agent--files-insert-into-input input-buf snippet)
      (message "Pi: Inserted %s into input" location))))

;; Global shortcut: allow inserting selected file regions directly
;; from source buffers without switching back to pi input first.
(define-key global-map (kbd "C-c C-a") #'pi-coding-agent-insert-region)

(provide 'pi-coding-agent-extension)
;;; pi-coding-agent-extension.el ends here
