;;; pi-coding-agent-extension-test.el --- Tests for pi-coding-agent-extension -*- lexical-binding: t; -*-

;;; Commentary:

;;; test for pi-coding-agent-extension.el

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

;;; @ Mention Completion

(ert-deftest pi-coding-agent-test-mention-capf-includes-agents-and-files ()
  "@ completion should merge agent and file candidates."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "Ask @se")
    (cl-letf (((symbol-function 'pi-coding-agent--discover-agents)
               (lambda (&optional _directory)
                 (list '(:name "search" :description "Search docs"))))
              ((symbol-function 'pi-coding-agent--get-project-files)
               (lambda () '("service.el" "notes.org"))))
      (let* ((result (pi-coding-agent--mention-capf))
             (table (nth 2 result))
             (candidates (all-completions "" table)))
        (should result)
        (should (member "search" candidates))
        (should (member "service.el" candidates))))))

(ert-deftest pi-coding-agent-test-complete-mention-inserts-agent-name ()
  "Auto @ completion should insert the selected agent name without duplicating @."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "Ask @")
    (cl-letf (((symbol-function 'pi-coding-agent--mention-candidates)
               (lambda (&optional _prefix)
                 (list '(:candidate "search" :display "@search"
                                    :annotation "  [agent] Search docs" :kind function)
                       '(:candidate "service.el" :display "service.el"
                                    :annotation " (file)" :kind file))))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "@search  [agent] Search docs")))
      (pi-coding-agent--complete-mention)
      (should (equal (buffer-string) "Ask @search")))))

(ert-deftest pi-coding-agent-test-mention-affixation-prefixes-agents-with-at ()
  "Mention affixation should display agents with @ prefix only in the UI."
  (let* ((entries (list '(:candidate "search" :display "@search"
                                     :annotation "  [agent] Search docs" :kind function)
                        '(:candidate "service.el" :display "service.el"
                                     :annotation " (file)" :kind file)))
         (affixation (pi-coding-agent--mention-affixation '("search" "service.el") entries)))
    (should (equal affixation
                   '(("search" "@search" "  [agent] Search docs")
                     ("service.el" "service.el" " (file)"))))))

(ert-deftest pi-coding-agent-test-ordered-completion-table-preserves-agent-first-order ()
  "Mention completion table should preserve agent-first ordering."
  (let* ((table (pi-coding-agent--ordered-completion-table
                 '("probe" "search" "README.md" "src/main.el")))
         (all (all-completions "" table)))
    (should (equal all '("probe" "search" "README.md" "src/main.el")))))

(ert-deftest pi-coding-agent-test-ordered-completion-table-fallback-provides-metadata ()
  "Fallback mention completion table should still expose metadata."
  (let ((original-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (if (eq symbol 'completion-table-with-metadata)
                     nil
                   (funcall original-fboundp symbol)))))
      (let ((table (pi-coding-agent--ordered-completion-table
                    '("probe" "search" "README.md"))))
        (should (equal (all-completions "" table)
                       '("probe" "search" "README.md")))
        (should (equal (funcall table "" nil 'metadata)
                       '(metadata
                         (display-sort-function . identity)
                         (cycle-sort-function . identity))))))))

(ert-deftest pi-coding-agent-test-ordered-completion-table-uses-built-in-metadata-wrapper ()
  "Mention completion table should use built-in metadata wrapper when available."
  (let ((original-fboundp (symbol-function 'fboundp))
        wrapped-table
        wrapped-metadata)
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (if (eq symbol 'completion-table-with-metadata)
                     t
                   (funcall original-fboundp symbol))))
              ((symbol-function 'completion-table-with-metadata)
               (lambda (table metadata)
                 (setq wrapped-table table
                       wrapped-metadata metadata)
                 table)))
      (let ((table (pi-coding-agent--ordered-completion-table
                    '("probe" "search" "README.md"))))
        (should (functionp table))
        (should (equal (all-completions "" table)
                       '("probe" "search" "README.md")))
        (should (equal wrapped-metadata
                       '(metadata
                         (display-sort-function . identity)
                         (cycle-sort-function . identity))))
        (should (equal (funcall wrapped-table "" nil 'metadata)
                       wrapped-metadata))))))


;;; insert region

(ert-deftest pi-coding-agent-test-insert-region-has-global-keybinding ()
  "C-c C-a is globally available in non-pi buffers."
  (with-temp-buffer
    (fundamental-mode)
    (should (eq (key-binding (kbd "C-c C-a")) 'pi-coding-agent-insert-region))))

(defun pi-coding-agent-test--open-snippet-buffer (dir contents)
  "Create snippet.el in DIR with CONTENTS and return its buffer."
  (let ((file (expand-file-name "snippet.el" dir)))
    (write-region contents nil file nil 'silent)
    (find-file-noselect file)))

(defun pi-coding-agent-test--insert-lines-from-buffer (buffer start-line end-line)
  "Insert START-LINE..END-LINE from BUFFER using `pi-coding-agent-insert-region'."
  (with-current-buffer buffer
    (goto-char (point-min))
    (forward-line (1- start-line))
    (set-mark (point))
    (forward-line (1+ (- end-line start-line)))
    (activate-mark)
    (pi-coding-agent-insert-region)))

(ert-deftest pi-coding-agent-test-active-input-buffer-resolves-single-live-input ()
  "Active input resolution recovers when exactly one input buffer exists."
  (let ((input-buf (get-buffer-create "*pi-coding-agent-test-single-active*")))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode))
          (setq pi-coding-agent--active-input-buffer nil)
          (should (eq input-buf (pi-coding-agent--get-active-input-buffer))))
      (setq pi-coding-agent--active-input-buffer nil)
      (pi-coding-agent-test--kill-live-buffers input-buf))))

(ert-deftest pi-coding-agent-test-active-input-buffer-remains-nil-when-ambiguous ()
  "Active input resolution stays nil when multiple input buffers exist."
  (let ((buffers (mapcar #'get-buffer-create
                         '("*pi-coding-agent-test-ambiguous-a*"
                           "*pi-coding-agent-test-ambiguous-b*"))))
    (unwind-protect
        (progn
          (dolist (buf buffers)
            (with-current-buffer buf
              (pi-coding-agent-input-mode)))
          (setq pi-coding-agent--active-input-buffer nil)
          (should-not (pi-coding-agent--get-active-input-buffer)))
      (setq pi-coding-agent--active-input-buffer nil)
      (apply #'pi-coding-agent-test--kill-live-buffers buffers))))

(ert-deftest pi-coding-agent-test-insert-region-adds-location-tag ()
  "Insert selected file region into input with @file#Lx-Ly marker."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-coding-agent-test-region-ref-"))
         (file-buf nil))
    (unwind-protect
        (pi-coding-agent-test-with-mock-session dir
                                                (setq file-buf (pi-coding-agent-test--open-snippet-buffer
                                                                dir "line1\nline2\nline3\nline4\n"))
                                                (let ((input-buf (get-buffer (pi-coding-agent-test--input-buffer-name dir))))
                                                  (should (buffer-live-p input-buf))
                                                  (pi-coding-agent--set-active-input-buffer input-buf)
                                                  (pi-coding-agent-test--insert-lines-from-buffer file-buf 2 3)
                                                  (let ((language (with-current-buffer file-buf
                                                                    (file-name-extension buffer-file-name))))
                                                    (with-current-buffer input-buf
                                                      (let ((text (buffer-string)))
                                                        (should (string-match-p "@snippet\\.el#L2-L3" text))
                                                        (should (string-match-p
                                                                 (regexp-quote
                                                                  (format "````%s\nline2\nline3\n````\n"
                                                                          language))
                                                                 text)))))))
      (pi-coding-agent-test--kill-live-buffers file-buf)
      (delete-directory dir t))))

(ert-deftest pi-coding-agent-test-insert-region-falls-back-to-visible-input-buffer ()
  "Region insertion falls back to the only visible input buffer."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-region-fallback-session-"))
         (source-dir (pi-coding-agent-test--make-temp-directory
                      "pi-coding-agent-test-region-fallback-source-"))
         (file-buf nil))
    (unwind-protect
        (pi-coding-agent-test-with-mock-session session-dir
          (setq file-buf (pi-coding-agent-test--open-snippet-buffer
                          source-dir "line1\nline2\nline3\n"))
          (let ((input-buf (get-buffer (pi-coding-agent-test--input-buffer-name session-dir))))
            (should (buffer-live-p input-buf))
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer file-buf)
              (set-window-buffer (split-window-below) input-buf)
              (pi-coding-agent--set-active-input-buffer nil)
              (pi-coding-agent-test--insert-lines-from-buffer file-buf 2 3))
            (let ((language (with-current-buffer file-buf
                              (file-name-extension buffer-file-name))))
              (with-current-buffer input-buf
                (let ((text (buffer-string)))
                  (should (string-match-p "@.*snippet\\.el#L2-L3" text))
                  (should (string-match-p
                           (regexp-quote
                            (format "````%s\nline2\nline3\n````\n"
                                    language))
                           text)))))))
      (pi-coding-agent-test--kill-live-buffers file-buf)
      (delete-directory source-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-insert-region-fails-without-active-input ()
  "Region insertion errors when no active pi input buffer exists."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-coding-agent-test-region-no-input-"))
         (file-buf nil))
    (unwind-protect
        (progn
          (setq file-buf (pi-coding-agent-test--open-snippet-buffer dir "line1\nline2\n"))
          (pi-coding-agent--set-active-input-buffer nil)
          (with-current-buffer file-buf
            (goto-char (point-min))
            (set-mark (point))
            (goto-char (point-max))
            (activate-mark)
            (should-error (pi-coding-agent-insert-region) :type 'user-error)))
      (pi-coding-agent-test--kill-live-buffers file-buf)
      (delete-directory dir t))))

(ert-deftest pi-coding-agent-test-insert-region-requires-file-buffer ()
  "Region insertion command errors when current buffer has no file."
  (with-temp-buffer
    (insert "abc")
    (transient-mark-mode 1)
    (goto-char (point-min))
    (set-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (should-error (pi-coding-agent-insert-region) :type 'user-error)))

(provide 'pi-coding-agent-extension-test)
;;; pi-coding-agent-extension-test.el ends here
