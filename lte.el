;;; lte.el --- Large Table Edition in Org and Markdwon buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Frédéric Giquel
;; Author: Frédéric Giquel <frederic.giquel@laposte.net>
;; URL: http://github.com/fredericgiquel/lte.el
;; Git-Repository: git://github.com/fredericgiquel/lte.el.git
;; Version: 0.3
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

;; Provides facilities to display and edit large table in Org and Markdown
;; buffers when line-wrapping is enabled:
;; - a minor-mode (lte-truncate-table-mode) that truncates table larger than
;;   window width
;; - a command (lte-edit-table) to open the table at point in an indirect buffer
;;   with line-wrapping disabled
;;
;; Full documentation here:
;;   https://github.com/fredericgiquel/lte.el/blob/main/README.org

;;; Code:
(require 'edit-indirect)
(require 'org)
(require 'org-indent)
(require 'markdown-mode nil t)

(defcustom lte-indirect-buffer-disable-minor-mode-list '(visual-line-mode visual-fill-column-mode olivetti-mode)
  "List of minor modes to disable in indirect buffer used to edit large table."
  :group 'lte
  :type '(repeat function))

(define-fringe-bitmap 'lte-dots [0 0 0 0 0 0 0 0 0 0 0 219 219] nil nil 'center)

(defvar lte--table-properties-by-mode
  '(org-mode (:at-point org-at-table-p :begin org-table-begin :end org-table-end :regexp "^[ \t]*|")
    markdown-mode (:at-point markdown-table-at-point-p :begin markdown-table-begin :end markdown-table-end :regexp "^[ \t]*|")))

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
        (overlay-put ov 'display (if (display-graphic-p) '(right-fringe lte-dots) '(right-margin "…")))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'window (get-buffer-window))
        (overlay-put ov 'evaporate t))
      (forward-line))))

(defun lte--remove-truncate-table-overlays (start end)
  "Remove all overlays used to truncate table between START and END."
  (let ((overlay-list (overlays-in start end))
        (win (get-buffer-window)))
    (dolist (ov overlay-list)
      (when (and (eq (overlay-get ov 'category) 'lte-overlay)
                 (eq (overlay-get ov 'window) win))
        (delete-overlay ov)))))

(defun lte--find-tables (start end)
  "Find all tables between START and END."
  (let* ((table-properties (plist-get lte--table-properties-by-mode major-mode))
         (table-list nil))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward (plist-get table-properties :regexp) end t))
        (when (funcall (plist-get table-properties :at-point))
          (let ((table-begin (funcall (plist-get table-properties :begin)))
                (table-end (funcall (plist-get table-properties :end))))
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

(defvar-local lte--line-numbers-display-width-by-win nil)
(defun lte--handle-line-numbers-display-width-change (win)
  "Refresh overlays in window WIN if `line-number-display-width' changed."
  (with-selected-window win
    (when display-line-numbers
      (let ((current-width (line-number-display-width))
            (saved-width (plist-get lte--line-numbers-display-width-by-win win)))
        (unless (eq current-width saved-width)
          (lte--truncate-tables-in-buffer)
          (setq-local lte--line-numbers-display-width-by-win
                      (plist-put lte--line-numbers-display-width-by-win win current-width)))))))

;;;###autoload
(defun lte-edit-table ()
  "Edit Org or Markdown table at point in an indirect buffer."
  (interactive)
  (if (or (eq major-mode 'org-mode)
          (eq major-mode 'markdown-mode))
      (let ((table-properties (plist-get lte--table-properties-by-mode major-mode)))
        (if (funcall (plist-get table-properties :at-point))
            (let* ((begin (funcall (plist-get table-properties :begin)))
                   (end (funcall (plist-get table-properties :end)))
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
  "Minor mode that truncates Org or Markdown tables larger than window body width."
  :lighter " LTE"
  :group 'lte
  (if lte-truncate-table-mode
      (progn
        (add-hook 'window-configuration-change-hook #'lte--truncate-tables-in-buffer nil t)
        (add-hook 'text-scale-mode-hook #'lte--truncate-tables-in-buffer nil t)
        (add-hook 'pre-redisplay-functions #'lte--handle-line-numbers-display-width-change nil t)
        (jit-lock-register #'lte--truncate-tables-in-region))
    (remove-hook 'window-configuration-change-hook #'lte--truncate-tables-in-buffer t)
    (remove-hook 'text-scale-mode-hook #'lte--truncate-tables-in-buffer t)
    (remove-hook 'pre-redisplay-functions #'lte--handle-line-numbers-display-width-change t)
    (jit-lock-unregister #'lte--truncate-tables-in-region)
    (remove-overlays (point-min) (point-max) 'category 'lte-overlay)))

(defun lte--org-fold-advice (from to flag &rest _)
  "Advice for `org-fold-core-region'.
Truncate tables between FROM and TO when `lte-truncate-table-mode' is
enabled and FLAG is nil (unfold action)."
  (when (and lte-truncate-table-mode (not flag))
    (lte--truncate-tables-in-region from to)))
(advice-add #'org-fold-core-region :after #'lte--org-fold-advice)

(defun lte--org-indent-advice (beg end &rest _)
  "Advice for `org-indent-add-properties'.
Truncate tables between BEG and END when `lte-truncate-table-mode' is
enabled."
  (when (and lte-truncate-table-mode (get-buffer-window))
    (lte--truncate-tables-in-region beg end)))
(advice-add #'org-indent-add-properties :after #'lte--org-indent-advice)

(provide 'lte)

;;; lte.el ends here
