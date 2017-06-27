;;; rufo-mode.el --- use rufo to automatically format ruby files

;; Copyright (C) 2017 by Daniel Ma

;; Author: Daniel Ma <danielhgma@gmail.com>
;; URL: https://github.com/danielma/rufo-mode
;; Version: 0.0.1
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This package provides the rufo-mode minor mode, which will use rufo
;; (https://github.com/asterite/rufo) to automatically fix ruby code
;; when it is saved.

;; To use it, require it, make sure `rufo' is in your path and add it to
;; your favorite ruby mode:

;;    (add-hook 'ruby-mode-hook #'rufo-mode)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defgroup rufo-mode nil
  "Fix ruby code with rufo"
  :group 'tools)

(defcustom rufo-mode-executable "rufo"
  "The rufo executable used by `rufo-mode'."
  :group 'rufo-mode
  :type 'string)

(defcustom rufo-mode-debug-mode nil
  "Whether rufo-mode should message debug information."
  :group 'rufo-mode
  :type 'boolean)

(defvar-local rufo-mode--verified nil
  "Set to t if rufo has been verified as working for this buffer.")

(defun rufo-mode--goto-line (line)
  "Move point to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun rufo-mode--delete-whole-line (&optional arg)
    "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
    (setq arg (or arg 1))
    (if (and (> arg 0)
             (eobp)
             (save-excursion (forward-visible-line 0) (eobp)))
        (signal 'end-of-buffer nil))
    (if (and (< arg 0)
             (bobp)
             (save-excursion (end-of-visible-line) (bobp)))
        (signal 'beginning-of-buffer nil))
    (cond ((zerop arg)
           (delete-region (progn (forward-visible-line 0) (point))
                          (progn (end-of-visible-line) (point))))
          ((< arg 0)
           (delete-region (progn (end-of-visible-line) (point))
                          (progn (forward-visible-line (1+ arg))
                                 (unless (bobp)
                                   (backward-char))
                                 (point))))
          (t
           (delete-region (progn (forward-visible-line 0) (point))
                                                  (progn (forward-visible-line arg) (point))))))

(defun rufo-mode--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (if rufo-mode-debug-mode
            (message (concat "rufo diff: " (buffer-string))))
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in rufo-mode--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (rufo-mode--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (rufo-mode--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in rufo-mode--apply-rcs-patch")))))))))

(defun rufo-mode--executable-available-p ()
  "Verify that the rufo executable exists."
  (let ((executable (executable-find rufo-mode-executable)))
    (and executable
         (file-executable-p executable)
         (zerop (call-process-shell-command (concat
                                "("
                                executable
                                " --help"
                                ")"))))))

(defun rufo-mode--verify ()
  "Set rufo-mode--verified to true if the executable is runnable."
  (or rufo-mode--verified
      (cond ((not (rufo-mode--executable-available-p))
             (rufo-mode -1)
             (message "rufo-mode: Could not find rufo.")
             nil)
            (t (setq-local rufo-mode--verified t)))))

(defun rufo-mode--kill-error-buffer (errbuf)
  "Kill buffer ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (with-current-buffer errbuf
        (erase-buffer))
      (kill-buffer errbuf))))

(defun rufo-format ()
  "Format the current buffer with rufo."
  (interactive)
  (let* ((ext (file-name-extension (or buffer-file-name "source.rb") t))
         (outputfile (make-temp-file "rufo-output" nil ext))
         (errorfile (make-temp-file "rufo-errors" nil ext))
         (errbuf (get-buffer-create "*rufo errors*"))
         (patchbuf (get-buffer-create "*rufo patch*"))
         (executable (executable-find rufo-mode-executable))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (rufo-args (concat "--filename=" buffer-file-name))
         )
    (if (rufo-mode--verify)
        (unwind-protect
            (save-restriction
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))
          (call-process-region nil nil executable nil (list :file outputfile) nil rufo-args)
          ;; (with-current-buffer errbuf
          ;;   (insert-file-contents errorfile)
          ;;   (if (string-equal "" (buffer-string))
          ;;       (message (buffer-string))))
          (call-process-region nil nil "diff" nil patchbuf nil "-n" "-" outputfile)
          (rufo-mode--apply-rcs-patch patchbuf)
          (message "Applied rufo with args `%s'" rufo-args)
          (if errbuf (rufo-mode--kill-error-buffer errbuf))))
      (kill-buffer patchbuf)
      (delete-file errorfile)
      (delete-file outputfile)))

;;;###autoload
(define-minor-mode rufo-mode
  "Use rufo to automatically fix ruby before saving."
  :lighter " rufo"
  (if rufo-mode
      (add-hook 'before-save-hook #'rufo-format nil t)
    (setq-local rufo-mode--verified nil)
    (remove-hook 'before-save-hook #'rufo-format t)))

(provide 'rufo-mode)
;;; rufo-mode.el ends here
