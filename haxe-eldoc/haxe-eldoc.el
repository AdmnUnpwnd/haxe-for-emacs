
;;; haxe-eldoc.el --- invoke haxe compiler to generate eldoc hints
;; Copyright (C) 2012 Amit Patel

;; URL: http://github.com/AdmnUnpwnd/haxe-for-emacs
;; Version: 0.1
;; Keywords: haxe

;; Author: Amit Patel <amitp@cs.stanford.edu>
;; Written: September 2012
;; License: MIT
;; (please feel free to use this or parts of this for your own setup)

;;; Commentary:
;;
;; I tried hxc-complete and modified it to make it work, but it was
;; just too slow for me, disrupting my typing. Pressing "." brings up
;; an ido completion, so if you were already typing something it
;; doesn't work. Pressing "(" always waits for completion even though
;; most "(" characters aren't function calls.
;;
;; I also tried hx-emacs but couldn't get it to work.
;;
;; So instead I've done my own thing, connected to eldoc. Here's how
;; I use it:

;; (require 'haxe-eldoc)
;; (defun set-up-haxe-eldoc ()
;;   (set (make-local-variable 'eldoc-documentation-function) 'haxe-eldoc-documentation-function)
;;   (eldoc-mode 1))
;; (add-hook 'haxe-mode-hook 'set-up-haxe-eldoc))

;; Caveats:
;;   1. It assumes there's a build.hxml
;;   2. I only wrote it for my own use and haven't documented it well
;;      or tested under different configurations
;;   3. I have an SSD so it's possible this won't run fast enough on
;;      other computers (I think eldoc is blocking)
;;   4. I don't know if it works on nested directories; I've only
;;      tested it at the top level.

;;; Code:

(defun get-haxe-data-1 (filename pos)
  "Invoke Haxe to get the documentation string for FILENAME at POS"
  (let* ((cmd (format "haxe build.hxml %s --display %s@%s"
                      filename filename (- (point) 1))))
    (shell-command-to-string cmd)))

(defvar get-haxe-data-2-cache '(("" -1) . "")
  "1-element cache for get-haxe-data-2, format ((filename pos) eldocstring)")

(defun get-haxe-data-2 (filename pos)
  "Cache the results of get-haxe-data-1"
  (unless (equal (list filename pos) (car get-haxe-data-2-cache))
    (setq get-haxe-data-2-cache
          (cons (list filename pos)
                (get-haxe-data-1 filename pos))))
  (cdr get-haxe-data-2-cache))

;; TODO: there's a bug here … if two buffers both are unsaved, they'll
;; both be saved in ____temp.hx, and there's a potential cache hit
;; when it should be a miss. To fix, use the buffer instead of the
;; filename as the cache key.
(defun get-haxe-data (pos)
  "Get Haxe documentation for the current buffer at POS"
  ;; If the original buffer is modified, Haxe won't see it, so write
  ;; the current contents to a temp file and use that instead
  (if (buffer-modified-p)
      (let ((temp-file-name "____temp.hx")
            (current-buffer-contents (buffer-string)))
        (with-temp-file temp-file-name (insert current-buffer-contents))
        (let ((output (get-haxe-data-2 temp-file-name pos)))
          (delete-file temp-file-name)
          output))
    (get-haxe-data-2 (file-relative-name (buffer-name)) pos)))

(defun parse-xml-string (str)
  (with-temp-buffer
    (insert str)
    (xml-parse-region (point-min) (point-max) (current-buffer))))

(defun haxe-eldoc-parse-xml (xml)
  (loop for r in xml
        concat
        (cond ((eq 'list (car r))  ;; list of options
               (loop for opt in (cdr r)
                     if (and (listp opt) (eq 'i (car opt)))
                     concat (concat " " (cdr (car (nth 1 opt))))))
              ((eq 'type (car r))  ;; type of a given function
               (replace-regexp-in-string " -> \\([^ ]+\\) : " ", \\1:"
                                         (replace-regexp-in-string "\n" " " (nth 2 r))))
              (t ""))))


(defun haxe-eldoc-documentation-function ()
  ;; TODO: if not looking at "." or "(" maybe use backward-sexp to go back to one
  ;; NOTE: only checking when looking at "." and "(" means that the cache may be overkill now
  (with-local-quit
    (if (save-excursion (backward-char) (looking-at "[.(]"))
        (let* ((output (get-haxe-data (- (point) 1)))
               (parsed (haxe-eldoc-parse-xml (parse-xml-string output))))
          (if (equal parsed "") output parsed))
      "")))


(provide 'haxe-eldoc)

;;; haxe-eldoc.el ends here

