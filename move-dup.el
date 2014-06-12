;;; move-dup.el --- Eclipse-like moving and duplicating lines or rectangles.

;; Copyright (C) 2014 Jimmy Yuen Ho Wong

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 0.1.2
;; Created 11 June 2014
;; Keywords: convenience wp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package offers convenient editing commands much like Eclipse's ability
;; to move and duplicate lines or rectangular selections.

;; If you aren't using `package.el' or plan to customize the default
;; key-bindings, you need to put `move-dup.el' into your Emacs' load-path and
;; `require' it in your Emacs init file; otherwise you can skip this part.

;; (require 'move-dup)

;; If you don't want to toggle the minor mode, you can bind these functions like
;; so. All of these functions work on a single line or a rectangle.

;; (global-set-key (kbd "M-<up>") 'md/move-lines-up)
;; (global-set-key (kbd "M-<down>") 'md/move-lines-down)
;; (global-set-key (kbd "C-M-<up>") 'md/duplicate-up)
;; (global-set-key (kbd "C-M-<down>") 'md/duplicate-down)

;; If you used `package.el' to install `move-dup.el', this is equivalent to all
;; of the above.
;; (global-move-dup-minor-mode)

;; You can also turn on `move-dup-minor-mode' individually for each buffer.
;; (move-dup-minor-mode)

;;; Code:
(defun md/ensure-rectangle ()
  (if (< (point) (mark))
      (exchange-point-and-mark))
  (when (not (char-equal (char-before (region-end)) 10))
    (end-of-line)
    (forward-char))
  (exchange-point-and-mark)
  (beginning-of-line)
  (exchange-point-and-mark))

(defun md/move-region (&optional n)
  (interactive "p")
  (md/ensure-rectangle)
  (let* ((start (region-beginning))
         (end (region-end))
         (lines (count-lines start end)))
    (if (< n 0)
        (exchange-point-and-mark))
    (pop-mark)
    (push-mark)
    (forward-line n)
    (let* ((swap-start (region-beginning))
           (swap-end (region-end)))
      (let (deactivate-mark)
        (transpose-regions start end swap-start swap-end)
        (pop-mark))
      (push-mark)
      (backward-char (- end start))
      (exchange-point-and-mark))))

(defun md/move-line (&optional n)
  (interactive "p")
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines n)
    (forward-line -1)
    (move-to-column col)))

(defun md/move-line-or-region (n)
  (if (use-region-p)
      (md/move-region n)
    (md/move-line n)))

(defun md/move-lines-up (&optional n)
  (interactive "p")
  (md/move-line-or-region (if (or (null n) (= n 0)) -1 (- n))))

(defun md/move-lines-down (&optional n)
  (interactive "p")
  (md/move-line-or-region (if (or (null n) (= n 0)) 1 n)))

(defun md/duplicate-up (&optional times)
  (interactive "p")
  (dotimes (i times) (md/duplicate-line-or-region "up")))

(defun md/duplicate-down (&optional times)
  (interactive "p")
  (dotimes (i times) (md/duplicate-line-or-region "down")))

(defun md/duplicate-line-or-region (direction)
  (if (use-region-p)
      (md/duplicate-region direction)
    (md/duplicate-line direction)))

(defun md/duplicate-line (direction)
  (interactive "p")
  (let ((text (buffer-substring (line-beginning-position) (line-end-position)))
        (col (current-column)))
    (forward-line)
    (insert text)
    (open-line 1)
    (if (string= direction "up")
        (forward-line -1))
    (move-to-column col)))

(defun md/duplicate-region (direction)
  (interactive "p")
  (md/ensure-rectangle)
  (let* ((start (region-beginning))
         (end (region-end))
         (text (buffer-substring start end))
         (text-length (length text)))
    (let (deactivate-mark)
      (insert text))
    (cond ((string= direction "down")
           (pop-mark)
           (push-mark)
           (backward-char text-length)
           (exchange-point-and-mark))
          ((string= direction "up")
           (backward-char text-length)))))

(defgroup move-dup nil
  "Eclipse-like moving and duplicating lines or rectangles."
  :group 'convenience
  :version "24.3"
  :package-version "0.1.2")

;;;###autoload
(define-minor-mode move-dup-mode
  "Eclipse-like moving and duplicating lines or rectangles with default key-bindings."
  :lighter " md"
  :keymap '(([M-up] . md/move-lines-up)
            ([M-down] . md/move-lines-down)
            ([C-M-up] . md/duplicate-up)
            ([C-M-down] . md/duplicate-down)))

(defun move-dup-on ()
  (unless (minibufferp)
    (move-dup-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-move-dup-mode move-dup-mode move-dup-on)

(provide 'move-dup)
;;; move-dup.el ends here
