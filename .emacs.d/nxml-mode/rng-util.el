;;; rng-util.el --- utility functions for RELAX NG library

;; Copyright (C) 2003 Thai Open Source Software Center Ltd

;; Author: James Clark <jjc@thaiopensource.com>
;; Keywords: XML, RelaxNG

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;;; Code:

(defconst rng-xml-namespace-uri 'http://www.w3.org/XML/1998/namespace)
(defconst rng-xsd-namespace-uri 'http://www.w3.org/2001/XMLSchema-datatypes)

(defun rng-normalize-space (str)
  (mapconcat 'identity (split-string str) " "))

(defun rng-uniquify-eq (list)
  "Destructively remove any element from LIST that is eq to
its predecessor."
  (and list
       (let ((head list))
	 (while (cdr head)
	   (if (eq (car head) (cadr head))
	       (setcdr head (cddr head)))
	   (setq head (cdr head)))
	 list)))

(defun rng-uniquify-equal (list)
  "Destructively remove any element from LIST that is equal to
its predecessor."
  (and list
       (let ((head list))
	 (while (cdr head)
	   (if (equal (car head) (cadr head))
	       (setcdr head (cddr head)))
	   (setq head (cdr head)))
	 list)))

(defun rng-blank-p (str) (string-match "\\`[ \t\n\r]*\\'" str))

(defun rng-time-to-float (time)
  (+ (* (nth 0 time) 65536.0)
     (nth 1 time)
     (/ (nth 2 time) 1000000.0)))

(defun rng-substq (new old list)
  "Replace first member of LIST (if any) that is eq to OLD by NEW.
LIST is not modified."
  (cond ((null list) nil)
	((eq (car list) old)
	 (cons new (cdr list)))
	(t
	 (let ((tail (cons (car list)
			   nil))
	       (rest (cdr list)))
	   (setq list tail)
	   (while rest
	     (let ((item (car rest)))
	       (setq rest (cdr rest))
	       (cond ((eq item old)
		      (setcdr tail
			      (cons new rest))
		      (setq rest nil))
		     (t
		      (setq tail
			    (setcdr tail
				    (cons item nil))))))))
	 list)))

(defmacro rng-with-unmodifying-text-property-changes (&rest body)
  "Evaluate BODY without any text property changes modifying the buffer.
Any text properties changes happen as usual but the changes are not treated as
modifications to the buffer."
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
	   (inhibit-read-only t)
	   (inhibit-modification-hooks t)
	   (buffer-undo-list t)
	   (deactivate-mark nil)
	   ;; Apparently these avoid file locking problems.
	   (buffer-file-name nil)
	   (buffer-file-truename nil))
       (unwind-protect
	   (progn ,@body)
	 (unless ,modified
	   (restore-buffer-modified-p nil))))))

(put 'rng-with-unmodifying-text-property-changes 'lisp-indent-function 0)
(def-edebug-spec rng-with-unmodifying-text-property-changes t)

(defmacro rng-with-invisible-motion (&rest body)
  "Evaluate body without calling any point motion hooks."
  `(let ((inhibit-point-motion-hooks t))
     ,@body))

(put 'rng-with-invisible-motion 'lisp-indent-function 0)
(def-edebug-spec rng-with-invisible-motion t)

(defun rng-quote-string (s)
  (concat "\"" s "\""))

(provide 'rng-util)

;;; rng-util.el ends here
