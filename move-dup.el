;;; move-dup.el --- Eclipse-like moving and duplications of lines or regions
;;; with a single key binding.

;; Copyright (c) 2014 Jimmy Yuen Ho Wong

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 0.1
;; Created 11 June 2014
;; Keywords: convenience wp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package offers convenient editing commands much like Eclipse's ability
;; to move and duplicate lines or selections. The following is the installation
;; instruction and recommended key-bindings.

;; (require 'move-dup)
;; (global-set-key (kbd "M-<up>") 'md/move-lines-up)
;; (global-set-key (kbd "M-<down>") 'md/move-lines-down)
;; (global-set-key (kbd "C-M-<up>") 'md/duplicate-up)
;; (global-set-key (kbd "C-M-<down>") 'md/duplicate-down)

;; TODO:
;; customizable bindings

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

(provide 'move-dup)

;;;###autoload
(require 'move-dup)
;;;###autoload
(global-set-key (kbd "M-<up>") 'md/move-lines-up)
;;;###autoload
(global-set-key (kbd "M-<down>") 'md/move-lines-down)
;;;###autoload
(global-set-key (kbd "C-M-<up>") 'md/duplicate-up)
;;;###autoload
(global-set-key (kbd "C-M-<down>") 'md/duplicate-down)

;;; move-dup.el ends here
