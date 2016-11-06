;;; rng-ns.el --- RELAX NG namespace processing

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

(defvar rng-ns-state nil
  "Contains the state of namespace processing.  The state
is never modified destructively and so can be saved and restored
without copying.

The value is a stack represented by a list. The list has length N + 1
where N is the number of open elements.  Each member of the list
represents the bindings in effect for a particular element.  Each
member is itself a list whose car is the default namespace
\(a symbol or nil) and whose cdr is an alist of (PREFIX . NS) pairs
where PREFIX is a string (never nil) and NS is the namespace URI
symbol.")

(defconst rng-ns-initial-state
  (list (list nil (cons "xml" rng-xml-namespace-uri)))
  "A list to be used as the initial value of rng-ns-state.  This
represents the state with no open elements and with the default
namespace bindings (no default namespace and only the xml prefix bound).")

(defsubst rng-ns-state () rng-ns-state)

(defsubst rng-set-ns-state (state)
  (setq rng-ns-state state))

(defsubst rng-ns-state-equal (state)
  (equal rng-ns-state state))

(defun rng-ns-init ()
  (setq rng-ns-state rng-ns-initial-state))

(defun rng-ns-push-state ()
  "Change the state by starting a new element. Namespace declarations
are inherited from the parent state."
  (setq rng-ns-state (cons (car rng-ns-state) rng-ns-state)))

(defun rng-ns-pop-state ()
  "Change the state by ending an element.  The behaviour is undefined
if there is no open element."
  (setq rng-ns-state (cdr rng-ns-state)))

(defun rng-ns-get-prefix (prefix)
  "Return the symbol for namespace bound to PREFIX, or nil if PREFIX
is unbound. PREFIX is a string, never nil."
  (let ((binding (assoc prefix (cdar rng-ns-state))))
    (and binding (cdr binding))))

(defun rng-ns-set-prefix (prefix ns)
  "Change the binding of PREFIX. PREFIX is a string (never nil).  NS
is a symbol (never nil). The change will be in effect until the end of
the current element."
  (setq rng-ns-state
	(let ((bindings (car rng-ns-state)))
	  (cons (cons (car bindings)
		      (cons (cons prefix ns) (cdr bindings)))
		(cdr rng-ns-state)))))

(defun rng-ns-get-default ()
  "Return the current default namespace as a symbol, or nil
if there is no default namespace."
  (caar rng-ns-state))

(defun rng-ns-set-default (ns)
  "Changes the current default namespace.  The change
will be in effect until the end of the current element.
NS is a symbol or nil."
  (setq rng-ns-state
	(cons (cons ns (cdar rng-ns-state))
	      (cdr rng-ns-state))))

(defun rng-ns-get-context ()
  (car rng-ns-state))

(defun rng-ns-in-scope ()
  "Return a list of the namespace symbols bound to some prefix.
The list will include nil if there is no default namespace."
  (cons (rng-ns-get-default) (mapcar 'cdr (cdar rng-ns-state))))

(defun rng-ns-prefixes-for (ns &optional attributep)
  (let ((current (car rng-ns-state))
	prefixes)
    (when (if attributep
	      (not ns)
	    (eq (car current) ns))
      (setq prefixes '(nil)))
    (setq current (cdr current))
    (while (let ((binding (rassq ns current)))
	     (when binding
	       (setq prefixes
		     (cons (car binding)
			   prefixes))
	       (setq current
		     (cdr (member binding current))))))
    prefixes))

(defun rng-ns-prefix-for (ns)
  (cdr (rassq ns (cdar rng-ns-state))))

(provide 'rng-ns)

;;; rng-ns.el ends here
