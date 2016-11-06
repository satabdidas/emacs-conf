;;; rng-auto.el --- automatically extracted autoloads for RELAX NG

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

(unless (and (fboundp 'make-hash-table)
	     (boundp 'fontification-functions))
  (error "FSF GNU Emacs version 21 or later required"))

(let ((dir (file-name-directory load-file-name)))
  (unless (member dir load-path)
    (setq load-path (cons dir load-path)))
  (setq rng-auto-file-name-alist-default
	(cons '("\\.\\(xml\\)\\'" "rnc" . 1)
	      (mapcar (lambda (rule)
			(list (car rule)
			      (expand-file-name (cdr rule) dir)))
		      '((".*\\.xsl\\'" . "schema/xslt.rnc")
			(".*\\.x?html?\\'" . "schema/xhtml.rnc")
			(".*\\.rng\\'" . "schema/relaxng.rnc")
			(".*\\.rdf\\'" . "schema/rdfxml.rnc")))))
  (setq rng-auto-element-alist-default
	(mapcar (lambda (rule)
		  (cons (car rule)
			(expand-file-name (cdr rule) dir)))
		'((("http://www.w3.org/1999/XSL/Transform" t t) . "schema/xslt.rnc")
		  ((t "xsl" "transform") . "schema/xslt.rnc")
		  ((t t "stylesheet") . "schema/xslt.rnc")
		  (("http://www.w3.org/1999/xhtml" t t) . "schema/xhtml.rnc")
		  ((t t "html") . "schema/xhtml.rnc")
		  (("http://relaxng.org/ns/structure/1.0" t t) . "schema/relaxng.rnc")
		  ((t t "grammar") . "schema/relaxng.rnc")
		  ((nil nil "article") . "schema/docbook.rnc")
		  ((nil nil "book") . "schema/docbook.rnc")
		  (("http://www.w3.org/1999/02/22-rdf-syntax-ns#" t t)
		   . "schema/rdfxml.rnc")
		  ((t t "RDF") . "schema/rdfxml.rnc")
		  ((t "rdf" t) . "schema/rdfxml.rnc")))))


;;;### (autoloads (nxml-mode) "nxml-mode" "nxml-mode.el" (16192 46226))
;;; Generated autoloads from nxml-mode.el

(autoload (quote nxml-mode) "nxml-mode" "\
Major mode for editing XML.

Syntax highlighting is performed unless the variable
`nxml-syntax-highlight-flag' is nil.

\\[nxml-insert-end-tag] inserts an end-tag for the current element.

The Emacs commands that normally operate on balanced expressions will
operate on XML markup items.  Thus \\[forward-sexp] will move forward
across one markup item; \\[backward-sexp] will move backward across
one markup item; \\[kill-sexp] will kill the following markup item;
\\[mark-sexp] will mark the following markup item.  Different things
are recognized as markup items in different context.  Within element
content, the markup items are tags, entity and chaoracter references,
processing instructions, strings with no markup, comments.  Within a
start-tag, the markup items are the element name and the attributes.
For more details, see the function `nxml-forward-balanced-item'.

\\[nxml-backward-up-element] moves up to the start-tag of the
containing element." t nil)

;;;***

;;;### (autoloads (rng-c-set-schema) "rng-cmpct" "rng-cmpct.el" (16191
;;;;;;  14634))
;;; Generated autoloads from rng-cmpct.el

(autoload (quote rng-c-set-schema) "rng-cmpct" "\
Set the current schema to be the RELAX NG compact syntax schema
in FILENAME." t nil)

;;;***

;;;### (autoloads (rng-byte-compile-load rng-update-autoloads) "rng-maint"
;;;;;;  "rng-maint.el" (16191 11245))
;;; Generated autoloads from rng-maint.el

(autoload (quote rng-update-autoloads) "rng-maint" "\
Update the autoloads in rng-auto.el." t nil)

(autoload (quote rng-byte-compile-load) "rng-maint" "\
Byte-compile and load all of the RELAX NG library in an appropriate order." t nil)

;;;***

;;;### (autoloads (rng-validate-mode) "rng-valid" "rng-valid.el"
;;;;;;  (16192 45330))
;;; Generated autoloads from rng-valid.el

(autoload (quote rng-validate-mode) "rng-valid" "\
Minor mode performing continuous validation against a RELAX NG schema." t nil)

;;;***

;;;### (autoloads (rng-xsd-compile) "rng-xsd" "rng-xsd.el" (16192
;;;;;;  20597))
;;; Generated autoloads from rng-xsd.el

(put (quote http://www\.w3\.org/2001/XMLSchema-datatypes) (quote rng-dt-compile) (quote rng-xsd-compile))

(autoload (quote rng-xsd-compile) "rng-xsd" "\
Provides W3C XML Schema as a RELAX NG datatypes library. NAME is a
symbol giving the local name of the datatype.  PARAMS is a list of
pairs (PARAM-NAME . PARAM-VALUE) where PARAM-NAME is a symbol giving
the name of the parameter and PARAM-VALUE is a string giving its
value.  If NAME or PARAMS are invalid, it calls rng-dt-error passing
it arguments in the same style as format; the value from rng-dt-error
will be returned.  Otherwise, it returns a list.  The first member of
the list is t if any string is a legal value for the datatype and nil
otherwise.  The second argument is a symbol; this symbol will be
called as a function passing it a string followed by the remaining
members of the list.  The function must return an object representing
the value of the datatype that was represented by the string, or nil
if the string is not a representation of any value. The object
returned can be any convenient non-nil value, provided that, if two
strings represent the same value, the returned objects must be equal." nil nil)

;;;***

;;; rng-auto.el ends here
