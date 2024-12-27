;;; lte.el --- Large table edition -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Frédéric Giquel
;; Author: Frédéric Giquel <frederic.giquel@laposte.net>
;; URL: http://github.com/fredericgiquel/lte.el
;; Git-Repository: git://github.com/fredericgiquel/lte.el.git
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org "9.6") (edit-indirect "0.1.13"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Offers facilities to display and edit large table in Org and Markdown buffer.

;;; Code:
(require 'edit-indirect)

(defcustom lte-indirect-buffer-disable-minor-mode-list '(visual-line-mode visual-fill-column-mode olivetti-mode)
  "List of minor modes to disable in indirect buffer used to edit large table."
  :group 'lte
  :type '(repeat function))

(define-fringe-bitmap 'lte-dots [0 0 0 0 0 0 0 0 0 0 0 219 219] nil nil 'center)

(require 'org-table)
(defalias 'lte--org-table-at-point-p 'org-at-table-p)

(require 'markdown-mode nil t)
(defalias 'lte--markdown-table-at-point-p 'markdown-table-at-point-p)

(defun lte--inherit-parent-major-mode (parent-buffer _beg _end)
  "Call PARENT-BUFFER major-mode."
  (funcall (with-current-buffer parent-buffer major-mode)))

(defun lte--visual-line-end-position ()
  "Return end position of current visual line."
  (let ((truncate-lines t))
    (save-excursion (end-of-visual-line) (point))))

(defun lte--add-truncate-table-overlays (start end)
  "Add overlays to truncate large table between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when-let* ((visual-line-end (lte--visual-line-end-position))
                  (line-end (line-end-position))
                  (truncate-p (> line-end visual-line-end))
                  (ov (make-overlay (- visual-line-end 1) line-end)))
        (overlay-put ov 'category 'lte-overlay)
        (overlay-put ov 'display '(right-fringe lte-dots))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'evaporate t))
      (forward-line))))

(defun lte--remove-truncate-table-overlays (start end)
  "Remove all overlays used to truncate table between START and END."
  (let ((overlay-list (overlays-in start end))
        (win (selected-window)))
    (dolist (ov overlay-list)
      (when (and (eq (overlay-get ov 'category) 'lte-overlay)
                 (eq (overlay-get ov 'window) win))
        (delete-overlay ov)))))

(defun lte--find-tables (start end)
  "Find all tables between START and END."
  (let* ((short-mode-name (string-trim-right (symbol-name major-mode) "-mode"))
         (table-at-point-p-func (intern (concat "lte--" short-mode-name "-table-at-point-p")))
         (table-begin-func (intern (concat short-mode-name "-table-begin")))
         (table-end-func (intern (concat short-mode-name "-table-end")))
         (table-list nil))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward "^[ \t]*|" end t))
        (when (funcall table-at-point-p-func)
          (let ((table-begin (funcall table-begin-func))
                (table-end (funcall table-end-func)))
            (push (cons (max table-begin start) (min table-end end)) table-list)
            (goto-char table-end)))))
    table-list))

(defun lte--truncate-tables-in-region (start end)
  "Truncate all tables between START and END."
  (let ((table-list (lte--find-tables start end)))
    (dolist (table table-list)
      (lte--remove-truncate-table-overlays (car table) (cdr table))
      (lte--add-truncate-table-overlays (car table) (cdr table)))))

(defun lte--truncate-tables-in-buffer ()
  "Truncate all tables in current buffer."
  (lte--truncate-tables-in-region (point-min) (point-max)))

(defun lte--truncate-tables-in-org-entry ()
  "Truncate all tables in current Org entry (only when org-indent is enabled)."
  (when (bound-and-true-p org-indent-mode)
    (lte--truncate-tables-in-region (org-entry-beginning-position) (org-entry-end-position))))

;;;###autoload
(defun lte-edit-table ()
  "Edit Org or Markdown table at point in an indirect buffer."
  (interactive)
  (if (or (eq major-mode 'org-mode)
          (eq major-mode 'markdown-mode))
      (let* ((short-mode-name (string-trim-right (symbol-name major-mode) "-mode"))
             (table-at-point-p-func (intern (concat "lte--" short-mode-name "-table-at-point-p")))
             (table-begin-func (intern (concat short-mode-name "-table-begin")))
             (table-end-func (intern (concat short-mode-name "-table-end"))))
        (if (funcall table-at-point-p-func)
            (let* ((begin (funcall table-begin-func))
                   (end (funcall table-end-func))
                   (relative-point (- (point) begin))
                   (edit-indirect-guess-mode-function 'lte--inherit-parent-major-mode)
                   (indirect-buf (edit-indirect-region begin end 'display-buffer)))
              (with-current-buffer indirect-buf
                (lte-truncate-table-mode -1)
                (dolist (minor-mode lte-indirect-buffer-disable-minor-mode-list)
                  (when (fboundp minor-mode) (funcall minor-mode -1)))
                (setq-local truncate-lines t)
                (goto-char (+ (point-min) relative-point))))
          (user-error "Not inside a table")))
    (user-error "Not in an Org or Markdown buffer")))

;;;###autoload
(define-minor-mode lte-truncate-table-mode
  "Minor mode that truncate Org or Markdown tables larger than window body width."
  :lighter " LTE"
  :group 'lte
  (if lte-truncate-table-mode
      (progn
        (add-hook 'window-configuration-change-hook #'lte--truncate-tables-in-buffer nil t)
        (add-hook 'text-scale-mode-hook #'lte--truncate-tables-in-buffer nil t)
        (jit-lock-register #'lte--truncate-tables-in-region)
        (when (eq major-mode 'org-mode)
          (add-hook 'org-after-promote-entry-hook #'lte--truncate-tables-in-org-entry nil t)
          (add-hook 'org-after-demote-entry-hook #'lte--truncate-tables-in-org-entry nil t)
          (add-hook 'org-indent-post-buffer-init-functions #'lte--truncate-after-org-indent nil t)))
    (remove-hook 'window-configuration-change-hook #'lte--truncate-tables-in-buffer t)
    (remove-hook 'text-scale-mode-hook #'lte--truncate-tables-in-buffer t)
    (jit-lock-unregister #'lte--truncate-tables-in-region)
    (when (eq major-mode 'org-mode)
      (remove-hook 'org-after-promote-entry-hook #'lte--truncate-tables-in-org-entry t)
      (remove-hook 'org-after-demote-entry-hook #'lte--truncate-tables-in-org-entry t)
      (remove-hook 'org-indent-post-buffer-init-functions #'lte--truncate-after-org-indent))
    (remove-overlays (point-min) (point-max) 'category 'lte-overlay)))

(defun lte--truncate-after-org-indent (buf)
  "Truncate all tables in BUF after org-indent initialisation."
  (when-let* ((lte-truncate-mode-enabled-p lte-truncate-table-mode)
              (win (get-buffer-window buf)))
    (with-selected-window win
      (with-current-buffer buf
        (lte--truncate-tables-in-region (point-min) (point-max))))))

(defun lte--org-fold-advice (from to flag &rest _)
  "Advice for `org-fold-core-region'.
Truncate tables between FROM and TO when `lte-truncate-table-mode' is
enabled and FLAG is nil (unfold action)."
  (when (and lte-truncate-table-mode (not flag))
    (lte--truncate-tables-in-region from to)))
(advice-add #'org-fold-core-region :after #'lte--org-fold-advice)

(provide 'lte)

;;; lte.el ends here
