;;; rng-maint.el --- commands for RELAX NG maintainers

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

(defvar rng-dir (file-name-directory load-file-name))

(defconst rng-autoload-modules
  '(rng-cmpct rng-maint rng-valid rng-xsd nxml-mode))

;;;###autoload
(defun rng-update-autoloads ()
  "Update the autoloads in rng-auto.el."
  (interactive)
  (let* ((generated-autoload-file (expand-file-name "rng-auto.el"
						    rng-dir)))
    (mapcar (lambda (x)
	      (update-file-autoloads
	       (expand-file-name (concat (symbol-name x) ".el") rng-dir)))
	    rng-autoload-modules)))


(defconst rng-compile-modules
  '(rng-util
    xmltok
    nxml-mode
    xsd-regexp
    rng-dt
    rng-xsd
    rng-ns
    rng-pttrn
    rng-cmpct
    rng-match
    rng-valid
    rng-maint))

;;;###autoload
(defun rng-byte-compile-load ()
  "Byte-compile and load all of the RELAX NG library in an appropriate order."
  (interactive)
  (mapcar (lambda (x)
	    (byte-compile-file (expand-file-name (concat (symbol-name x) ".el")
						 rng-dir)
			       t))
	  rng-compile-modules))

;;; rng-maint.el ends here
