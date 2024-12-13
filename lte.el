;;; lte.el --- Large table edition -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Frédéric Giquel
;; Author: Frédéric Giquel <frederic.giquel@laposte.net>
;; URL: http://github.com/fredericgiquel/lte
;; Git-Repository: git://github.com/fredericgiquel/lte.git
;; Created: 2024-12-07
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9.5") (edit-indirect "0.1.13"))

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
  "List of minor modes to be disabled in indirect buffer used to edit large table."
  :group 'lte
  :type '(repeat function))

(defvar-local lte--last-widths '(0 . 0))

(define-fringe-bitmap 'lte-right-arrow [128 192 224 240 248 252 254 252 248 240 224 192 128] nil nil 'center)

(require 'org-table)
(defalias 'lte--org-table-at-point-p 'org-at-table-p)

(require 'markdown-mode nil t)
(defalias 'lte--markdown-table-at-point-p 'markdown-table-at-point-p)

(defun lte--inherit-parent-major-mode (parent-buffer _beg _end)
  "Call PARENT-BUFFER major-mode."
  (funcall (with-current-buffer parent-buffer major-mode)))

(defun lte--get-visual-line-width ()
  "Return visual line width."
  (let ((word-wrap nil))
    (- (save-excursion (end-of-visual-line) (point))
       (save-excursion (beginning-of-visual-line) (point)))))

(defun lte--add-overlays (start end)
  "Add overlays to truncate large table between START and END."
  (save-excursion
    (goto-char start)
    (let ((visual-line-width (lte--get-visual-line-width)))
      (while (< (point) end)
        (when-let* ((begin-hidden (+ (line-beginning-position) visual-line-width))
                    (end-hidden (line-end-position))
                    (large-p (> end-hidden begin-hidden))
                    (ov (make-overlay (- begin-hidden 1) end-hidden)))
          (overlay-put ov 'category 'lte-overlay)
          (overlay-put ov 'display '(right-fringe lte-right-arrow))
          (overlay-put ov 'invisible t)
          (overlay-put ov 'window t)
          (overlay-put ov 'evaporate t))
        (forward-line)))))

(defun lte--remove-overlays (start end)
  "Remove all overlays in `lte-overlay' category between START and END."
  (remove-overlays start end 'category 'lte-overlay))

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
            (push (cons table-begin table-end) table-list)
            (goto-char table-end)))))
    table-list))

(defun lte--truncate-tables-in-region (start end)
  "Truncate all tables between START and END."
  (let ((table-list (lte--find-tables start end)))
    (dolist (table table-list)
      (lte--remove-overlays (car table) (cdr table))
      (lte--add-overlays (car table) (cdr table)))))

(defun lte--predisplay-handler (win)
  "Trigger truncate tables (re-)display in window WIN if `window-body-width' changed."
  (let ((last-width1 (car lte--last-widths))
        (last-width2 (cdr lte--last-widths))
        (new-width (window-body-width win)))
    (when (and (equal new-width last-width1)
               (not (equal last-width1 last-width2)))
      (with-selected-window win
        (lte--truncate-tables-in-region (window-start) (window-end))))
    (setq-local lte--last-widths `(,new-width . ,last-width1))))

(defun lte--truncate-after-org-indent (buf)
  "Truncate all tables in BUF after org-indent initialisation."
  (when-let* ((win (get-buffer-window buf)))
    (with-selected-window win
      (with-current-buffer buf
        (lte--truncate-tables-in-region (point-min) (point-max))))))

;;;###autoload
(defun lte-edit-table ()
  "Edit Org or Markdown table in an indirect buffer."
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
  "Minor mode that automatically truncate tables larger than window width."
  :lighter " LTE"
  :group 'lte
  (if lte-truncate-table-mode
      (progn
        (add-hook 'pre-redisplay-functions #'lte--predisplay-handler nil t)
        (jit-lock-register #'lte--truncate-tables-in-region)
        (when (eq major-mode 'org-mode)
          (add-hook 'org-indent-post-buffer-init-functions #'lte--truncate-after-org-indent nil t)))
    (remove-hook 'pre-redisplay-functions #'lte--predisplay-handler t)
    (jit-lock-unregister #'lte--truncate-tables-in-region)
    (when (eq major-mode 'org-mode)
      (remove-hook 'org-indent-post-buffer-init-functions #'lte--truncate-after-org-indent))
    (lte--remove-overlays (point-min) (point-max))))

(provide 'lte)

;;; lte.el ends here
