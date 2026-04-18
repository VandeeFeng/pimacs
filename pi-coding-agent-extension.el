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
(require 'seq)
(require 'subr-x)

;;; Extension: insert-region
;; File-centric helpers for pi-coding-agent.
;;
;; This extension provides one user command:
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

;;; Extension: @agent mention support
;; Mention helpers for pi-coding-agent input.
;;
;; Agent definitions are discovered in pi's default order:
;; - ~/.pi/agent/agents
;; - nearest ancestor .pi/agents from cwd
;;
;; The actual @agent execution semantics still live on the pi side; this
;; extension layer only handles editor-side discovery and completion.

(defconst pi-coding-agent--agent-file-extension ".md"
  "Filename extension for agent definitions.")

(defconst pi-coding-agent--project-agents-dir ".pi/agents"
  "Project-relative directory containing agent definitions.")

(defconst pi-coding-agent--mention-completion-metadata
  '(metadata
    (display-sort-function . identity)
    (cycle-sort-function . identity))
  "Metadata for mention completion tables that must preserve candidate order.")

(defun pi-coding-agent--agent-user-directory ()
  "Return the user agent directory."
  (expand-file-name "~/.pi/agent/agents"))

(defun pi-coding-agent--agent-project-directory (&optional directory)
  "Return nearest project agent directory from DIRECTORY, or nil.
Matches pi's subagent discovery by walking upward and checking
`<dir>/.pi/agents' at each level."
  (when-let* ((start-dir (or directory (pi-coding-agent--session-directory)))
              (project-root (locate-dominating-file start-dir
                                                    pi-coding-agent--project-agents-dir)))
    (expand-file-name pi-coding-agent--project-agents-dir project-root)))

(defun pi-coding-agent--agent-frontmatter-value (header key)
  "Return string value for KEY from frontmatter HEADER, or nil."
  (when (string-match (format "^%s:[[:space:]]*\\(.+\\)$" (regexp-quote key)) header)
    (string-trim (string-trim (match-string 1 header)) "\"" "\"")))

(defun pi-coding-agent--parse-agent-file (file)
  "Return agent plist parsed from FILE, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (looking-at-p "---[[:space:]]*$")
        (forward-line 1)
        (let ((header-start (point)))
          (when (re-search-forward "^---[[:space:]]*$" nil t)
            (let* ((header (buffer-substring-no-properties header-start (match-beginning 0)))
                   (name (pi-coding-agent--agent-frontmatter-value header "name"))
                   (description (pi-coding-agent--agent-frontmatter-value header "description")))
              (when (and name description)
                (list :name (string-remove-prefix "@" name)
                      :description description
                      :file file)))))))))

(defun pi-coding-agent--load-agents-from-directory (directory source)
  "Return agent plists loaded from DIRECTORY with SOURCE.
SOURCE is recorded as either `user' or `project'."
  (when (file-directory-p directory)
    (delq nil
          (mapcar
           (lambda (file)
             (when-let* ((agent (pi-coding-agent--parse-agent-file file)))
               (plist-put agent :source source)))
           (directory-files directory t
                            (concat (regexp-quote pi-coding-agent--agent-file-extension) "\\'")
                            t)))))

(defun pi-coding-agent--discover-agents (&optional directory)
  "Return available agents for DIRECTORY.
Project agents override user agents with the same name."
  (let ((agents (make-hash-table :test 'equal)))
    (dolist (agent (pi-coding-agent--load-agents-from-directory
                    (pi-coding-agent--agent-user-directory) 'user))
      (puthash (plist-get agent :name) agent agents))
    (when-let* ((project-dir (pi-coding-agent--agent-project-directory directory)))
      (dolist (agent (pi-coding-agent--load-agents-from-directory project-dir 'project))
        (puthash (plist-get agent :name) agent agents)))
    (sort (hash-table-values agents)
          (lambda (left right)
            (string-lessp (plist-get left :name)
                          (plist-get right :name))))))

(defun pi-coding-agent--mention-bounds ()
  "Return bounds for current @ mention text as (START . END), or nil.
START is the first character after @."
  (save-excursion
    (let ((limit (line-beginning-position))
          (end (point))
          start)
      (while (and (not start) (> (point) limit))
        (backward-char)
        (when (eq (char-after) ?@)
          (let ((prev (char-before)))
            (when (or (null prev)
                      (not (string-match-p "[[:alnum:]]" (string prev))))
              (setq start (1+ (point)))))))
      (when start
        (let ((prefix (buffer-substring-no-properties start end)))
          (unless (string-match-p "[[:space:]]" prefix)
            (cons start end)))))))

(defun pi-coding-agent--mention-candidates (&optional prefix)
  "Return completion metadata for @ PREFIX.
Agents are listed before files."
  (let* ((query (or prefix ""))
         (matcher (lambda (text)
                    (string-match-p (regexp-quote query) text)))
         (agents (seq-filter
                  (lambda (agent)
                    (funcall matcher (plist-get agent :name)))
                  (pi-coding-agent--discover-agents)))
         (project-files (pi-coding-agent--get-project-files))
         (files (if (string-empty-p query)
                    project-files
                  (cl-remove-if-not matcher project-files))))
    (append
     (mapcar (lambda (agent)
               (list :candidate (plist-get agent :name)
                     :display (concat "@" (plist-get agent :name))
                     :annotation (format "  [agent] %s"
                                         (plist-get agent :description))
                     :kind 'function))
             agents)
     (mapcar (lambda (file)
               (list :candidate file
                     :display file
                     :annotation " (file)"
                     :kind 'file))
             files))))

(defun pi-coding-agent--mention-entry (candidate entries)
  "Return metadata entry for CANDIDATE from ENTRIES."
  (seq-find (lambda (entry)
              (equal (plist-get entry :candidate) candidate))
            entries))

(defun pi-coding-agent--mention-affixation (candidates entries)
  "Return affixation tuples for CANDIDATES using ENTRIES."
  (mapcar (lambda (candidate)
            (let ((entry (pi-coding-agent--mention-entry candidate entries)))
              (list candidate
                    (or (plist-get entry :display) candidate)
                    (or (plist-get entry :annotation) ""))))
          candidates))

(defun pi-coding-agent--ordered-completion-table (candidates)
  "Return a completion table preserving CANDIDATES order."
  (let ((table
         (lambda (string pred action)
           (if (eq action 'metadata)
               pi-coding-agent--mention-completion-metadata
             (complete-with-action action candidates string pred)))))
    (if (fboundp 'completion-table-with-metadata)
        (completion-table-with-metadata
         table
         pi-coding-agent--mention-completion-metadata)
      table)))

(defun pi-coding-agent--mention-capf ()
  "Completion-at-point function for @agent and @file references."
  (when-let* ((bounds (pi-coding-agent--mention-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (prefix (buffer-substring-no-properties start end))
           (entries (pi-coding-agent--mention-candidates prefix))
           (candidates (mapcar (lambda (entry) (plist-get entry :candidate)) entries)))
      (when candidates
        (list start end
              (pi-coding-agent--ordered-completion-table candidates)
              :exclusive 'no
              :annotation-function
              (lambda (candidate)
                (plist-get (pi-coding-agent--mention-entry candidate entries)
                           :annotation))
              :affixation-function
              (lambda (items)
                (pi-coding-agent--mention-affixation items entries))
              :company-kind
              (lambda (candidate)
                (plist-get (pi-coding-agent--mention-entry candidate entries)
                           :kind)))))))

(defun pi-coding-agent--complete-mention ()
  "Prompt for an @agent or @file completion at point."
  (interactive)
  (when-let* ((bounds (pi-coding-agent--mention-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (prefix (buffer-substring-no-properties start end))
           (entries (pi-coding-agent--mention-candidates prefix)))
      (when entries
        (let* ((choices
                (mapcar (lambda (entry)
                          (cons (format "%s%s"
                                        (plist-get entry :display)
                                        (plist-get entry :annotation))
                                (plist-get entry :candidate)))
                        entries))
               (choice (completing-read
                        "Mention: "
                        (pi-coding-agent--ordered-completion-table (mapcar #'car choices))
                        nil t prefix)))
          (when-let* ((value (cdr (assoc choice choices))))
            (delete-region start end)
            (insert value)))))))

(provide 'pi-coding-agent-extension)
;;; pi-coding-agent-extension.el ends here
