;;; rng-valid.el --- real-time validation of XML using RELAX NG

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

;; For usage information, see the documentation for rng-validate-mode.
;;
;; This file provides a minor mode that continually validates a buffer
;; against a RELAX NG schema. The validation state is used to support
;; schema-sensitive editing as well as validation. Validation is
;; performed while Emacs is idle.  XML parsing is done using
;; xmltok.el. This file is responsible for checking that end-tags
;; match their start-tags.  Namespace processing is handled by
;; rng-ns.el. The RELAX NG Compact Syntax schema is parsed into
;; internal form by rng-cmpct.el.  This internal form is described by
;; rng-pttrn.el.  Validation of the document by matching against this
;; internal form is done by rng-match.el. Handling of W3C XML Schema
;; datatypes is delegated by rng-match.el to rng-xsd.el.  The minor
;; mode is intended to be used in conjunction with the nxml major
;; mode, but does not have to be.
;;
;; The major responsibility of this file is to allow validation to
;; happen incrementally.  If a buffer has been validated and is then
;; changed, we can often revalidate it without having to completely
;; parse and validate it from start to end.  As we parse and validate
;; the buffer, we periodically cache the state.  The state has three
;; components: the stack of open elements, the namespace processing
;; state and the RELAX NG validation state. The state is cached as the
;; value of the rng-state text property on the closing greater-than of
;; tags (but at intervals, not on every tag).  We keep track of the
;; position up to which cached state is known to be correct by adding
;; a function to the buffer's after-change-functions. This is stored
;; in the rng-validate-up-to-date-end variable.  The first way in
;; which we make validation incremental is obvious: we start
;; validation from the first cached state before
;; rng-validate-up-to-date-end.
;;
;; To make this work efficiently, we have to be able to copy the
;; current parsing and validation state efficiently.  We do this by
;; minimizing destructive changes to the objects storing the state.
;; When state is changed, we use the old state to create new objects
;; representing the new state rather than destructively modifying the
;; objects representing the old state. Copying the state is just a
;; matter of making a list of three objects, one for each component of
;; the state; the three objects themselves can be shared and do not
;; need to be copied.
;;
;; There's one other idea that is used to make validation incremental.
;; Suppose we have a buffer that's 4000 bytes long and suppose we
;; validated it, caching state at positions 1000, 2000 and 3000.  Now
;; suppose we make a change at position 1500 inserting 100 characters.
;; rng-validate-up-to-date-end will be changed to 1500.  When Emacs
;; becomes idle and we revalidate, validation will restart using the
;; cached state at position 1000.  However, we take advantage of the
;; cached state beyond rng-validate-up-to-date-end as follows.  When
;; our validation reaches position 2100 (the current position of the
;; character that was at 2000), we compare our current state with the
;; cached state.  If they are the same, then we can stop parsing
;; immediately and set rng-validate-up-to-date-end to the end of the
;; buffer: we already know that the state cached at position 3100 is
;; correct.  If they are not the same, then we have to continue
;; parsing.  After the change, but before revalidation, we call the
;; region from 1600 to the end of the buffer "conditionally
;; up-to-date".
;;
;; As well as the cached parsing and validation state, we also keep
;; track of the errors in the file.  Errors are stored as overlays
;; with a category of rng-error.  The number of such overlays in the
;; buffer must always be equal to rng-error-count.

;;; Code:

(require 'xmltok)
(require 'rng-match)
(require 'rng-ns)
(require 'rng-util)

;;; Customizable variables

(defgroup relax-ng nil
  "Validation of XML using RELAX NG."
  :group 'wp
  :group 'nxml
  :group 'languages)

(defface rng-error-face '((t (:underline "red")))
  "Face for highlighting XML errors."
  :group 'relax-ng)

(defvar rng-auto-file-name-alist-default nil
  "Default value for variable `rng-auto-file-name-alist'.")

(defcustom rng-auto-file-name-alist rng-auto-file-name-alist-default
  "*Alist of buffer file names vs schema file names.

Each member of the alist has the form \(REGEXP SCHEMA . SUBEXP).  If
the buffer file name matches REGEXP, then SCHEMA is used as the schema
file name: `\\' is special as with the function `replace-match'. The
resulting string will be used only if it is the name of a readable
file.

SUBEXP has the same meaning as with the function `replace-match'.  If
non-nil, it says to replace that subexpression rather than the entire
buffer file name.

For example, if the alist contains

  \(\"\\\\.\\\\(xml\\\\)\\\\'\" \"rnc\" . 1)

and the buffer file name is `~/chapter01.xml', then a schema of
`~/chapter01.rnc' will be used if it exists and is readable.  If
instead you wanted to use a schema `~/chapter.rnc', then you could
create a file `~/chapter01.rnc' containing the single line

  external \"chapter.rnc\"

Alternatively, you could create a symbolic link `~/chapter01.rnc'
pointing to `~/chapter.rnc'."
  :type '(repeat (cons :tag "Rule"
		       (regexp :tag "File name regexp")
		       (cons :tag "Schema file name"
			     (string :tag "New string")
			     (choice :tag "What to replace with new string"
				     (const :tag "Entire match" nil)
				     (integer :tag "N-th subexpression")))))
  :group 'relax-ng)

(defvar rng-auto-element-alist-default nil
  "Default value for variable `rng-auto-element-alist'.")

(defcustom rng-auto-element-alist rng-auto-element-alist-default
  "*Alist of root element names vs schema file names.
Root element names are specified as \(NAMESPACE-URI PREFIX LOCAL-NAME)
lists.  NAMESPACE-URI is either nil, meaning no namespace URI, t,
meaning any namespace URI \(including no namespace URI) or a
string. PREFIX is either nil, meaning no prefix, t, meaning any prefix
\(including no prefix), or a string.  LOCAL-NAME is either t, meaning
any local-name or a string. The local-name is the part of the element
name after the colon, if any."
  :type '(repeat (cons :tag "Rule"
		       (list :tag "With this root element"
			     (choice :tag "namespace URI"
				     :value t
				     (string :tag "Specific")
				     (const :tag "None" nil)
				     (const :tag "Any" t))
			     (choice :tag "prefix"
				     :value t
				     (string :tag "Specific")
				     (const :tag "None" nil)
				     (const :tag "Any" t))
			     (choice :tag "local name"
				     :value t
				     (string :tag "Specific")
				     (const :tag "Any" t)))
		       (file :tag "Use this schema file" :must-match t)))
  :group 'relax-ng)

(defcustom rng-state-cache-distance 2000
  "*Distance in characters between each parsing and validation state cache."
  :type 'integer
  :group 'relax-ng)

(defcustom rng-validate-chunk-size 8000
  "*Number of characters in a RELAX NG validation chunk.
A validation chunk will be the smallest chunk that is at least this
size and ends with a tag.  After validating a chunk, validation will
continue only if Emacs is still idle."
  :type 'integer
  :group 'relax-ng)

(defcustom rng-validate-delay 1.5
  "*Time in seconds that Emacs must be idle before starting a full validation.
A full validation continues until either validation is up to date
or Emacs is no longer idle."
  :type 'number
  :group 'relax-ng)

(defcustom rng-validate-quick-delay 0.3
  "*Time in seconds that Emacs must be idle before starting a quick validation.
A quick validation validates at most one chunk."
  :type 'number
  :group 'relax-ng)

;; Global variables

(defvar rng-validate-timer nil)
(make-variable-buffer-local 'rng-validate-timer)
;; ensure that we can cancel the timer even after a kill-all-local-variables
(put 'rng-validate-timer 'permanent-local t)

(defvar rng-validate-quick-timer nil)
(make-variable-buffer-local 'rng-validate-quick-timer)
;; ensure that we can cancel the timer even after a kill-all-local-variables
(put 'rng-validate-quick-timer 'permanent-local t)

(defvar rng-error-count nil
  "Number of errors in the current buffer.  Always equal to number of
overlays with category rng-error.")
(make-variable-buffer-local 'rng-error-count)

(defvar rng-open-elements nil
  "Stack of names of open elements represented as a list
of (PREFIX . LOCAL-NAME) pairs.")

(defvar rng-pending-contents nil
  "Text content of current element that has yet to be processed.
Value is a list of segments (VALUE START END) positions in reverse
order.  VALUE is a string or nil.  If VALUE is nil, then the value is
the string between START and END.  A segment can also be nil
indicating an unresolvable entity or character reference.")

(defvar rng-collecting-text nil)

(defvar rng-validate-up-to-date-end nil
  "Last position where validation is known to be up to date.")
(make-variable-buffer-local 'rng-validate-up-to-date-end)

(defvar rng-conditional-up-to-date-start nil
  "Marker for the start of the conditionally up-to-date region.
Nil if there is no conditionally up-to-date region.  The conditionally
up-to-date region must be such that for any cached state S with
position P in the conditionally up-to-date region, if at some point it
is determined that S becomes correct for P, then all states with
position >= P in the conditionally up to date region must also then be
correct and all errors between P and the end of the region must then
be correctly marked.")
(make-variable-buffer-local 'rng-conditional-up-to-date-start)

(defvar rng-conditional-up-to-date-end nil
  "Marker for the end of the conditionally up-to-date region.
Nil if there is no conditionally up-to-date region.  See the variable
`rng-conditional-up-to-date-start'.")
(make-variable-buffer-local 'rng-conditional-up-to-date-end)

(defvar rng-parsing-for-state nil
  "Non-nil means we are currently parsing just to compute the state.
Should be dynamically bound.")

(defvar rng-validate-mode nil)
(make-variable-buffer-local 'rng-validate-mode)

(defvar rng-dtd nil)
(make-variable-buffer-local 'rng-dtd)

(defvar rng-validate-mode-map nil)
(unless rng-validate-mode-map
  (setq rng-validate-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map "\M-\t" 'rng-complete-symbol)
	  (define-key map "\C-c\C-n" 'rng-next-error)
	  (define-key map "\C-c\C-s" 'rng-c-set-schema)
	  map)))

;;;###autoload
(defun rng-validate-mode (&optional arg)
  "Minor mode performing continual validation against a RELAX NG schema.

Checks whether the buffer is a well-formed XML 1.0 document,
conforming to the XML Namespaces Recommendation and valid against a
RELAX NG schema. The mode-line indicates whether it is or not.  Any
parts of the buffer that cause it not to be are considered errors and
are highlighted with `rng-error-face'. A description of each error is
available as a tooltip.  \\[rng-next-error] goes to the next error
after point. Clicking mouse-1 on the word `Invalid' in the mode-line
goes to the first error in the buffer. If the buffer changes, then it
will be automatically rechecked when Emacs becomes idle; the
rechecking will be paused whenever there is input pending..

By default, uses a vacuous schema that allows any well-formed XML
document. A schema can be specified explictly using
\\[rng-c-set-schema], or implicitly based on the buffer's file name or
on the root element name.  In each case the schema must be a RELAX NG
schema using the compact schema \(such schemas conventionally have a
suffix of `.rnc').  Uses the variable `rng-auto-file-name-alist' to
implicitly locate a schema based on the buffer's file name.  If that
fails to locate a schema, uses the variable `rng-auto-element-alist'
to locate a schema based on the root element name.

\\[rng-complete-symbol] performs schema- and context-sensitive
completion on start-tag names, attribute names and enumerated
attribute values.  This will not do anything useful with a vacuous
schema."
  (interactive "P")
  (setq rng-validate-mode
	(if (null arg)
	    (not rng-validate-mode)
	  (> (prefix-numeric-value arg) 0)))
  (setq minor-mode-alist
	(cons '(rng-validate-mode
		(:eval (rng-compute-mode-line-string)))
	      (assq-delete-all 'rng-validate-mode
			       minor-mode-alist)))
  (unless (assq 'rng-validate-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	(cons (cons 'rng-validate-mode rng-validate-mode-map)
	      minor-mode-map-alist)))
  (save-restriction
    (widen)
    (rng-with-unmodifying-text-property-changes
      (rng-clear-cached-state (point-min) (point-max)))
    ;; 1+ to clear empty overlays at (point-max)
    (rng-clear-overlays (point-min) (1+ (point-max))))
  (setq rng-validate-up-to-date-end 1)
  (rng-clear-conditional-region)
  (setq rng-error-count 0)
  ;; do this here to avoid infinite loop if we set the schema
  (remove-hook 'rng-schema-change-hook 'rng-validate-clear t)
  (cond (rng-validate-mode
	 (unwind-protect
	     (when (or (not rng-current-schema)
		       (eq rng-current-schema rng-any-element))
	       (let ((schema (or (rng-choose-schema-file-using-file-name)
				 (rng-choose-schema-file-using-root-element))))
		 (when schema
		   (rng-c-set-schema schema)
		   (message "Used schema %s" schema))))
	   (unless rng-current-schema
	     (message "Using vacuous schema")
	     (setq rng-current-schema rng-any-element)
	     (run-hooks 'rng-schema-change-hook))
	   (add-hook 'rng-schema-change-hook 'rng-validate-clear nil t)
	   (add-hook 'after-change-functions 'rng-after-change-function nil t)
	   (add-hook 'kill-buffer-hook 'rng-cancel-timers nil t)
	   (rng-activate-timers)
	   ;; Don't wait for Emacs to become idle.
	   (rng-validate-while-idle (current-buffer))))
	(t
	 (rng-cancel-timers)
	 (remove-hook 'kill-buffer-hook 'rng-cancel-timers t)
	 (remove-hook 'after-change-functions 'rng-after-change-function t))))

(defun rng-choose-schema-file-using-file-name ()
  (let ((rules rng-auto-file-name-alist)
	(file-name (buffer-file-name))
	(case-fold-search nil)
	rule schema-file)
    (while (and rules (not schema-file))
      (setq rule (car rules))
      (when (string-match (car rule) file-name)
	(setq schema-file
	      (replace-match (cadr rule)
			     t
			     nil
			     file-name
			     (cddr rule)))
	(unless (file-readable-p schema-file)
	  (setq schema-file nil)))
      (setq rules (cdr rules)))
    schema-file))

(defun rng-choose-schema-file-using-root-element ()
  (let ((rules rng-auto-element-alist)
	(root-element (rng-root-element))
	rule schema-file)
    (when root-element
      (when (car root-element)
	(setcar root-element (symbol-name (car root-element))))
      (while rules
	(setq rule (car rules))
	(setq rules (cdr rules))
	(when (let ((actual-element root-element)
		    (spec-element (car rule))
		    (match t))
		(while (if (or (equal (car actual-element)
				      (car spec-element))
			       (eq (car spec-element) t))
			   (and (setq actual-element
				      (cdr actual-element))
				(setq spec-element
				      (cdr spec-element)))
			 (setq match nil)))
		match)
	  (setq schema-file (cdr rule))
	  (setq rules nil))))
    schema-file))

(defun rng-root-element ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (xmltok-save
	(xmltok-forward-prolog)
	(xmltok-forward)
	(when (memq xmltok-type '(start-tag
				  partial-start-tag
				  empty-element
				  partial-empty-element))
	  (rng-process-namespaces)
	  (let ((prefix (xmltok-start-tag-prefix)))
	    (list (if prefix
		      (or (rng-ns-get-prefix prefix) t)
		    (rng-ns-get-default))
		  prefix
		  (xmltok-start-tag-local-name))))))))

(defun rng-after-change-function (start end pre-change-len)
  (rng-with-unmodifying-text-property-changes
    (rng-clear-cached-state start end))
  ;; rng-validate-up-to-date-end holds the position before the change
  ;; Adjust it to reflect the change.
  (if (< start rng-validate-up-to-date-end)
      (setq rng-validate-up-to-date-end
	    (+ rng-validate-up-to-date-end
	       (- end start pre-change-len))))
  ;; Adjust the conditional zone
  (cond (rng-conditional-up-to-date-start
	 (when (< rng-conditional-up-to-date-start end)
	   (if (< end rng-conditional-up-to-date-end)
	       (set-marker rng-conditional-up-to-date-start end)
	     (rng-clear-conditional-region))))
	((< end rng-validate-up-to-date-end)
	 (setq rng-conditional-up-to-date-end
	       (copy-marker rng-validate-up-to-date-end nil))
	 (setq rng-conditional-up-to-date-start
	       (copy-marker end t))))
  ;; Adjust rng-validate-up-to-date-end
  (if (< start rng-validate-up-to-date-end)
      (setq rng-validate-up-to-date-end start))
  ;; Must make rng-validate-up-to-date-end < point-max
  ;; (unless the buffer is empty).
  ;; otherwise validate-prepare will say there's nothing to do.
  ;; Don't use (point-max) because we may be narrowed.
  (if (> rng-validate-up-to-date-end (buffer-size))
      (setq rng-validate-up-to-date-end
	    (max 1 (1- rng-validate-up-to-date-end))))
  ;; Arrange to revalidate
  (rng-activate-timers)
  ;; Need to do this after activating the timer
  (force-mode-line-update))
  
(defun rng-compute-mode-line-string ()
  (cond (rng-validate-timer
	 (concat " Validated:"
		 (number-to-string
		  ;; Use floor rather than round because we want
		  ;; to show 99% rather than 100% for changes near
		  ;; the end.
		  (floor (/ (* (- rng-validate-up-to-date-end 1) 100.0)
			    (buffer-size))))
		 "%%"))
	((> rng-error-count 0)
	 (concat " "
		 (propertize "Invalid"
			     'help-echo "mouse-1: go to first error"
			     'local-map (make-mode-line-mouse-map
					 'mouse-1
					 'rng-mouse-first-error))))
	(t " Valid")))
   
(defun rng-cancel-timers ()
  (when rng-validate-timer
    (let ((inhibit-quit t))
      (cancel-timer rng-validate-timer)
      (cancel-timer rng-validate-quick-timer)
      (setq rng-validate-timer nil))))

(defun rng-activate-timers ()
  (unless rng-validate-timer
    (let ((inhibit-quit t))
      (setq rng-validate-timer
	    (run-with-idle-timer rng-validate-delay
				 t
				 'rng-validate-while-idle
				 (current-buffer)))
      (setq rng-validate-quick-timer
	    (run-with-idle-timer rng-validate-quick-delay
				 t
				 'rng-validate-quick-while-idle
				 (current-buffer))))))

(defun rng-validate-clear ()
  (rng-validate-mode 1))

(defun rng-validate-while-idle (buffer)
  (with-current-buffer buffer
    (if rng-validate-mode
	(while (if (rng-do-some-validation)
		   ;; input-pending-p and sit-for run timers that are
		   ;; ripe.  Binding timer-idle-list to nil prevents
		   ;; this.  If we don't do this, then any ripe timers
		   ;; will get run, and we won't get any chance to
		   ;; validate until Emacs becomes idle again or until
		   ;; the other lower priority timers finish (which
		   ;; can take a very long time in the case of
		   ;; jit-lock).
		   (let ((timer-idle-list nil))
		     (and (not (input-pending-p))
			  (progn
			    (force-mode-line-update)
			    (sit-for 0))))
		 (rng-validate-done)
		 nil))
      ;; must have done kill-all-local-variables
      (cancel-timer rng-validate-timer)
      (kill-local-variable 'rng-validate-timer))))

(defun rng-validate-quick-while-idle (buffer)
  (with-current-buffer buffer
    (if rng-validate-mode
	(if (rng-do-some-validation)
	    (force-mode-line-update)
	  (rng-validate-done))
      (cancel-timer rng-validate-quick-timer)
      (kill-local-variable 'rng-validate-quick-timer))))

(defun rng-validate-done ()
  (rng-cancel-timers)
  (force-mode-line-update))

(defun rng-do-some-validation ()
  "Do some validation work. Return t if more to do, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (rng-with-invisible-motion
	(condition-case err
	    (and (rng-validate-prepare)
		 (let ((rng-dt-namespace-context-getter '(rng-ns-get-context)))
		   (rng-with-unmodifying-text-property-changes
		     (rng-do-some-validation1))))
	  ;; errors signalled from a function run by an idle timer
	  ;; are ignored; if we don't catch them, validation
	  ;; will get mysteriously stuck at a single place
	  (rng-compile-error
	   (message "Incorrect schema. %s" (nth 1 err))
	   (rng-validate-mode 0)
	   nil)
	  (error
	   (message "Internal error at %s in rng-validation-mode. %s"
		    (point)
		    (error-message-string err))
	   (rng-validate-mode 0)
	   nil))))))

(defun rng-validate-prepare ()
  "Prepare to do some validation, initializing point and the state.
Return t if there is work to do, nil otherwise."
  (cond ((= rng-validate-up-to-date-end (point-min))
	 (rng-set-initial-state)
	 t)
	((= rng-validate-up-to-date-end (point-max))
	 nil)
	(t (let ((state (get-text-property (1- rng-validate-up-to-date-end)
					   'rng-state)))
	     (cond (state
		    (rng-restore-state state)
		    (goto-char rng-validate-up-to-date-end))
		   (t
		    (let ((pos (previous-single-property-change
				rng-validate-up-to-date-end
				'rng-state)))
		      (cond (pos
			     (rng-restore-state
			      (or (get-text-property (1- pos) 'rng-state)
				  (error "Internal error: state null")))
			     (goto-char pos))
			    (t (rng-set-initial-state))))))))))


(defun rng-do-some-validation1 ()
  (let ((limit (+ rng-validate-up-to-date-end
		  rng-validate-chunk-size))
	(remove-start rng-validate-up-to-date-end)
	(next-cache-point (+ (point) rng-state-cache-distance))
	(continue t)
	(xmltok-dtd rng-dtd)
	have-remaining-chars
	xmltok-type
	xmltok-start
	xmltok-name-colon
	xmltok-name-end
	xmltok-replacement
	xmltok-attributes
	xmltok-namespace-attributes
	xmltok-dependent-regions
	xmltok-errors)
    (when (= (point) 1)
      (xmltok-forward-prolog)
      (rng-clear-overlays 1 (point))
      (unless (equal rng-dtd xmltok-dtd)
	(rng-clear-conditional-region))
      (setq rng-dtd xmltok-dtd))
    (while continue
      (setq have-remaining-chars (rng-forward))
      (let ((pos (point)))
	(setq continue
	      (and have-remaining-chars
		   (< pos limit)))
	(cond ((and rng-conditional-up-to-date-start
		    ;; > because we are getting the state from (1- pos)
		    (> pos rng-conditional-up-to-date-start)
		    (< pos rng-conditional-up-to-date-end)
		    (rng-state-matches-current (get-text-property (1- pos)
								  'rng-state)))
	       (when (< remove-start (1- pos))
		 (rng-clear-cached-state remove-start (1- pos)))
	       ;; sync up with cached validation state
	       (setq continue nil)
	       ;; do this before settting rng-validate-up-to-date-end
	       ;; in case we get a quit
	       (rng-mark-xmltok-errors)
	       (rng-mark-xmltok-dependent-regions)
	       (setq rng-validate-up-to-date-end
		     (marker-position rng-conditional-up-to-date-end))
	       (rng-clear-conditional-region)
	       (setq have-remaining-chars
		     (< rng-validate-up-to-date-end (point-max))))
	      ((or (>= pos next-cache-point)
		   (not continue))
	       (setq next-cache-point (+ pos rng-state-cache-distance))
	       (rng-clear-cached-state remove-start pos)
	       (when have-remaining-chars
		 (rng-cache-state (1- pos)))
	       (setq remove-start pos)
	       (unless continue
		 ;; if we have just blank chars skip to the end
		 (when have-remaining-chars
		   (skip-chars-forward " \t\r\n")
		   (when (= (point) (point-max))
		     (rng-clear-overlays pos (point))
		     (rng-clear-cached-state pos (point))
		     (setq have-remaining-chars nil)
		     (setq pos (point))))
		 (when (not have-remaining-chars)
		   (rng-process-end-document))
		 (rng-mark-xmltok-errors)
		 (rng-mark-xmltok-dependent-regions)
		 (setq rng-validate-up-to-date-end pos)
		 (when rng-conditional-up-to-date-end
		   (cond ((<= rng-conditional-up-to-date-end pos)
			  (rng-clear-conditional-region))
			 ((< rng-conditional-up-to-date-start pos)
			  (set-marker rng-conditional-up-to-date-start
				      pos)))))))))
    have-remaining-chars))
    
(defun rng-clear-conditional-region ()
  (when rng-conditional-up-to-date-start
    (set-marker rng-conditional-up-to-date-start nil)
    (setq rng-conditional-up-to-date-start nil))
  (when rng-conditional-up-to-date-end
    (set-marker rng-conditional-up-to-date-end nil)
    (setq rng-conditional-up-to-date-end nil)))

(defun rng-clear-cached-state (start end)
  "Clear cached state between START and END."
  (remove-text-properties start end '(rng-state nil)))

(defun rng-cache-state (pos)
  "Save the current state in a text property on the character at pos."
  (put-text-property pos
		     (1+ pos)
		     'rng-state
		     (rng-get-state)))

(defun rng-state-matches-current (state)
  (and state
       (rng-match-state-equal (car state))
       (rng-ns-state-equal (nth 1 state))
       (equal (nth 2 state) rng-open-elements)))

(defun rng-get-state ()
  (list (rng-match-state)
	(rng-ns-state)
	rng-open-elements))

(defun rng-restore-state (state)
  (rng-set-match-state (car state))
  (setq state (cdr state))
  (rng-set-ns-state (car state))
  (setq rng-open-elements (cadr state))
  (setq rng-pending-contents nil)
  (setq rng-collecting-text (rng-match-text-typed-p)))

(defun rng-set-initial-state ()
  (rng-ns-init)
  (rng-match-start-document)
  (setq rng-open-elements nil)
  (setq rng-pending-contents nil)
  (goto-char (point-min)))

(defun rng-clear-overlays (beg end)
  (unless rng-parsing-for-state
    (let ((overlays (overlays-in beg end)))
      (while overlays
	(let* ((overlay (car overlays))
	       (category (overlay-get overlay 'category)))
	  (cond ((eq category 'rng-error)
		 (let ((inhibit-quit t))
		   (delete-overlay overlay)
		   ;; rng-error-count could be nil
		   ;; if overlays left over from a previous use
		   ;; of rng-validate-mode that ended with a change of mode
		   (when rng-error-count
		     (setq rng-error-count (1- rng-error-count)))))
		((and (eq category 'rng-dependent)
		      (<= beg (overlay-start overlay)))
		 (delete-overlay overlay))))
	(setq overlays (cdr overlays))))))

;;; Dependent regions

(defun rng-mark-xmltok-dependent-regions ()
  (while xmltok-dependent-regions
    (apply 'rng-mark-xmltok-dependent-region
	   (car xmltok-dependent-regions))
    (setq xmltok-dependent-regions
	  (cdr xmltok-dependent-regions))))

(defun rng-mark-xmltok-dependent-region (fun start end &rest args)
  (let ((overlay (make-overlay start end nil t t)))
    (overlay-put overlay 'category 'rng-dependent)
    (overlay-put overlay 'rng-funargs (cons fun args))))

(put 'rng-dependent 'evaporate t)
(put 'rng-dependent 'modification-hooks '(rng-dependent-region-changed))
(put 'rng-dependent 'insert-behind-hooks '(rng-dependent-region-changed))

(defun rng-dependent-region-changed (overlay
				     after-p
				     change-start
				     change-end
				     &optional pre-change-length)
  (when (and after-p
	     ;; Emacs sometimes appears to call deleted overlays
	     (overlay-start overlay)
	     (let ((funargs (overlay-get overlay 'rng-funargs)))
	       (save-match-data
		 (save-excursion
		   (save-restriction
		     (widen)
		     (apply (car funargs)
			    (append (list change-start
					  change-end
					  pre-change-length
					  (overlay-start overlay)
					  (overlay-end overlay))
				    (cdr funargs))))))))
    (rng-after-change-function (overlay-start overlay)
			       change-end
			       (+ pre-change-length
				  (- (overlay-start overlay)
				     change-start)))
    (delete-overlay overlay)))

;;; Error state

(defun rng-mark-xmltok-errors ()
  (while xmltok-errors
    (let ((err (car xmltok-errors)))
      (rng-mark-not-well-formed (xmltok-error-message err)
				(xmltok-error-start err)
				(xmltok-error-end err)))
    (setq xmltok-errors (cdr xmltok-errors))))

(defun rng-mark-invalid (message beg end)
  (rng-mark-error message beg end))

(defun rng-mark-not-well-formed (message beg end)
  ;; Don't try to validate further
  ;;(rng-set-match-state rng-not-allowed-ipattern)
  (rng-mark-error message beg end))

(defun rng-mark-error (message beg end)
  (unless rng-parsing-for-state
    (let ((overlays (overlays-in beg end)))
      (while (and overlays message)
	(let ((o (car overlays)))
	  (when (and (eq (overlay-get o 'category) 'rng-error)
		     (= (overlay-start o) beg)
		     (= (overlay-end o) end))
	    (overlay-put o
			 'help-echo
			 (concat (overlay-get o 'help-echo)
				 "\n"
				 message))
	    (setq message nil)))
	(setq overlays (cdr overlays))))
    (when message
      (let ((inhibit-quit t))
	(setq rng-error-count (1+ rng-error-count))
	(let ((overlay
	       (make-overlay beg end nil t
			     ;; Need to make the rear delimiter advance
			     ;; with the front delimiter when the overlay
			     ;; is empty, otherwise the front delimiter
			     ;; will move past the rear delimiter.
			     (= beg end))))
	  ;; Ensure when we have two overlapping messages, the help-echo
	  ;; of the one that starts first is shown
	  (overlay-put overlay 'priority beg)
	  (overlay-put overlay 'category 'rng-error)
	  (overlay-put overlay 'help-echo message))))))

(put 'rng-error 'face 'rng-error-face)
(put 'rng-error 'modification-hooks '(rng-error-modified))

;; If we don't do this, then the front delimiter can move
;; past the end delimiter.
(defun rng-error-modified (overlay after-p beg end &optional pre-change-len)
  (when (and after-p
	     (overlay-start overlay)	; check not deleted
	     (>= (overlay-start overlay)
		 (overlay-end overlay)))
    (let ((inhibit-quit t))
      (delete-overlay overlay)
      (setq rng-error-count (1- rng-error-count)))))

;;; Error navigation
	     
(defun rng-show-current-error ()
  "Display the error message associated with the error at point."
  (interactive)
  (let ((overlays (overlays-in (point) (1+ (point))))
	(best nil))
    (while overlays
      (let ((overlay (car overlays)))
	(when (and (eq (overlay-get overlay 'category)
		       'rng-error)
		   (or (not best)
		       (< (overlay-start best)
			  (overlay-start overlay))))
	  (setq best overlay)))
      (setq overlays (cdr overlays)))
    (when best
      (rng-error-overlay-message best))))
		  
(defun rng-first-error ()
  "Go to the first validation error."
  (interactive)
  (when (and (eq rng-validate-up-to-date-end 1)
	     (< rng-validate-up-to-date-end (point-max)))
    (rng-do-some-validation))
  (let ((err (rng-find-next-error-overlay (1- (point-min)))))
    (if err
	(rng-goto-error-overlay err)
      (let ((pos (save-excursion
		   (goto-char (point-min))
		   (rng-next-error 1))))
	(when pos
	  (goto-char pos))))))

(defun rng-mouse-first-error (event)
  "Go to the first validation error from a mouse click."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (rng-first-error))

(defun rng-next-error (arg)
  "Go to the next validation error after point.
A prefix ARG specifies how many errors to move. A negative ARG
moves backwards."
  (interactive "p")
  (if (< arg 0)
      (rng-previous-error (- arg))
    (let* ((pos (point))
	   err last-err)
      (while (and (> arg 0)
		  (setq err (rng-find-next-error-overlay pos)))
	(setq arg (1- arg))
	(setq last-err err)
	(setq pos (overlay-start err)))
      (when (> arg 0)
	(setq pos (max pos (1- rng-validate-up-to-date-end)))      
	(when (< rng-validate-up-to-date-end (point-max))
	  (message "Parsing...")
	  (while (let ((more-to-do (rng-do-some-validation)))
		   (while (and (> arg 0)
			       (setq err (rng-find-next-error-overlay pos)))
		     (setq arg (1- arg))
		     (setq last-err err)
		     (setq pos (overlay-start err)))
		   (when (and (> arg 0)
			      more-to-do
			      (< rng-validate-up-to-date-end (point-max)))
		     ;; Display percentage validated.
		     (force-mode-line-update)
		     ;; Force redisplay but don't allow idle timers to run.
		     (let ((timer-idle-list nil))
		       (sit-for 0))
		     (setq pos
			   (max pos (1- rng-validate-up-to-date-end)))
		     t)))))
      (if last-err
	  (rng-goto-error-overlay last-err)
	(message "No more errors")
	nil))))

(defun rng-previous-error (arg)
  "Go to the previous validation error before point.
A prefix ARG specifies how many errors to move. A negative ARG
moves forwards."
  (interactive "p")
  (if (< arg 0)
      (rng-next-error (- arg))
    (let* ((pos (point))
	   err last-err)
      (while (and (> arg 0)
		  (setq err (rng-find-previous-error-overlay pos)))
	(setq pos (overlay-start err))
	(setq last-err err)
	(setq arg (1- arg)))
      (when (and (> arg 0)
		 (< rng-validate-up-to-date-end (min pos (point-max))))
	(message "Parsing...")
	(while (and (rng-do-some-validation)
		    (< rng-validate-up-to-date-end (min pos (point-max))))
	  (force-mode-line-update)
	  ;; Force redisplay but don't allow idle timers to run.
	  (let ((timer-idle-list nil))
	    (sit-for 0)))
	(while (and (> arg 0)
		    (setq err (rng-find-previous-error-overlay pos)))
	  (setq pos (overlay-start err))
	  (setq last-err err)
	  (setq arg (1- arg))))
      (if last-err
	  (rng-goto-error-overlay last-err)
	(message "No previous errors")
	nil))))
      
(defun rng-goto-error-overlay (err)
  (rng-error-overlay-message err)
  (goto-char (overlay-start err)))

(defun rng-error-overlay-message (err)
  (message "%s" (overlay-get err 'help-echo)))

(defun rng-find-next-error-overlay (pos)
  "Return the overlay for the next error starting after POS.
Return nil if there is no such overlay or it is out of date.
Do not do any additional validation."
  (when rng-error-count
    (let (done found overlays)
      (while (not done)
	(cond (overlays
	       (let ((overlay (car overlays)))
		 (setq overlays (cdr overlays))
		 (when (and (eq (overlay-get overlay 'category) 'rng-error)
			    ;; Is it the first?
			    (= (overlay-start overlay) pos)
			    ;; Is it up to date?
			    (<= (overlay-end overlay)
				rng-validate-up-to-date-end))
		   (setq done t)
		   (setq found overlay))))
	      ((or (= pos (point-max))
		   (> (setq pos (next-overlay-change pos))
		      rng-validate-up-to-date-end))
	       (setq done t))
	      (t (setq overlays (overlays-in pos (1+ pos))))))
      found)))

(defun rng-find-previous-error-overlay (pos)
  "Return the overlay for the last error starting before POS.
Return nil if there is no such overlay or it is out of date.
Do not do any additional validation."
  (when (and rng-error-count
	     (<= pos rng-validate-up-to-date-end))
    (let (done found overlays)
      (while (not done)
	(cond (overlays
	       (let ((overlay (car overlays)))
		 (setq overlays (cdr overlays))
		 (when (and (eq (overlay-get overlay 'category) 'rng-error)
			    ;; Is it the first?
			    (= (overlay-start overlay) pos))
		   (setq done t)
		   (setq found overlay))))
	      ((= pos (point-min))
	       (setq done t))
	      (t
	       (setq pos (previous-overlay-change pos))
	       (setq overlays (overlays-in pos (1+ pos))))))
      found)))

;;; Parsing

(defun rng-forward (&optional limit)
  "Move forward over one or more tokens updating the state.
If LIMIT is nil, stop after tags.
If LIMIT is non-nil, stop when end of last token parsed is >= LIMIT.
Return nil at end of buffer, t otherwise."
  (let (type)
    (while (progn
	     (setq type (xmltok-forward))
	     (rng-clear-overlays xmltok-start (point))
	     (let ((continue
		    (cond ((memq type '(start-tag empty-element))
			   (rng-process-start-tag type)
			   nil)
			  ((eq type 'end-tag)
			   (rng-process-end-tag)
			   nil)
			  ((eq type 'empty-element)
			   (rng-process-start-tag 'empty-element)
			   nil)
			  ((eq type 'space)
			   (rng-process-text xmltok-start nil t)
			   t)
			  ((eq type 'data)
			   (rng-process-text xmltok-start nil nil)
			   t)
			  ((memq type '(entity-ref char-ref))
			   (cond (xmltok-replacement
				  (rng-process-text xmltok-start
						    nil
						    'maybe
						    xmltok-replacement))
				 ((eq type 'char-ref)
				  (rng-process-unknown-char))
				 (t
				  (rng-process-unknown-entity)))
			   t)
			  ((eq type 'cdata-section)
			   (rng-process-text (+ xmltok-start 9)	; "<![CDATA["
					     (- (point) 3) ; "]]>"
					     'maybe)
			   t)
			  ((eq type 'partial-start-tag)
			   (rng-process-start-tag 'partial-start-tag)
			   t)
			  ((eq type 'partial-empty-element)
			   (rng-process-start-tag 'empty-element)
			   t)
			  ((eq type 'partial-end-tag)
			   (rng-process-end-tag 'partial)
			   t)
			  (t type))))
	       (if limit
		   (< (point) limit)
		 continue))))
    (and type t)))

(defun rng-process-start-tag (tag-type)
  "TAG-TYPE is `start-tag' for a start-tag, `empty-element' for
an empty element.  partial-empty-element should be passed
as empty-element."
  (and rng-collecting-text (rng-flush-text))
  (setq rng-collecting-text nil)
  (setq rng-pending-contents nil)
  (rng-process-namespaces)
  (let ((tag (rng-process-tag-name)))
    (rng-process-attributes)
    ;; set the state appropriately
    (cond ((eq tag-type 'empty-element)
	   (rng-process-start-tag-close)
	   ;; deal with missing content with empty element
	   (when (not (rng-match-empty-content))
	     (rng-match-after)
	     (rng-mark-start-tag-close "Empty content not allowed"))
	   (rng-ns-pop-state))
	  ((eq tag-type 'start-tag)
	   (rng-process-start-tag-close)
	   (setq rng-collecting-text (rng-match-text-typed-p))
	   (rng-push-tag tag))
	  ((eq tag-type 'partial-start-tag)
	   (rng-process-start-tag-close)
	   (rng-match-after)
	   (rng-ns-pop-state)))))

(defun rng-process-namespaces ()
  (let ((nsatts xmltok-namespace-attributes)
	prefixes)
    (rng-ns-push-state)
    (while nsatts
      (let* ((att (car nsatts))
	     (value (xmltok-attribute-value att)))
	(when value
	  (let ((ns (if (= (length value) 0)
			nil
		      (intern value)))
		(prefix (and (xmltok-attribute-prefix att)
			     (xmltok-attribute-local-name att))))
	    (cond ((member prefix prefixes)
		   (rng-mark-invalid "Duplicate namespace declaration"
				     (xmltok-attribute-name-start att)
				     (xmltok-attribute-name-end att)))
		  ((not prefix)
		   (rng-ns-set-default ns))
		  (ns
		   (rng-ns-set-prefix prefix ns))
		  (t
		   ;; cannot have xmlns:foo=""
		   (rng-mark-invalid "Namespace prefix cannot be undeclared"
				     (1- (xmltok-attribute-value-start att))
				     (1+ (xmltok-attribute-value-end att)))))
	    (setq prefixes (cons prefix prefixes)))))
      (setq nsatts (cdr nsatts)))))

(defun rng-process-tag-name ()
  (let* ((prefix (xmltok-start-tag-prefix))
	 (local-name (xmltok-start-tag-local-name))
	 (name
	  (if prefix
	      (let ((ns (rng-ns-get-prefix prefix)))
		(cond (ns (cons ns local-name))
		      ((and (setq ns
				  (rng-match-infer-start-tag-namespace
				   local-name))
			    (rng-match-start-tag-open (cons ns local-name)))
		       (rng-ns-set-prefix prefix ns)
		       (rng-mark-start-tag-close "Missing xmlns:%s=\"%s\""
						 prefix
						 ns)
		       nil)
		      (t
		       (rng-recover-bad-element-prefix)
		       nil)))
	    (cons (rng-ns-get-default) local-name))))
    (when (and name
	       (not (rng-match-start-tag-open name)))
      (unless (and (not (car name))
		   (let ((ns (rng-match-infer-start-tag-namespace (cdr name))))
		     (and ns
			  (rng-match-start-tag-open (cons ns local-name))
			  (progn
			    (rng-ns-set-default ns)
			    ;; XXX need to check we don't have xmlns=""
			    (rng-mark-start-tag-close "Missing xmlns=\"%s\""
						      ns)
			    t))))
	(rng-recover-start-tag-open name)))
    (cons prefix local-name)))

(defun rng-process-attributes ()
  (let ((atts xmltok-attributes)
	names)
    (while atts
      (let* ((att (car atts))
	     (prefix (xmltok-attribute-prefix att))
	     (local-name (xmltok-attribute-local-name att))
	     (name
	      (if prefix
		  (let ((ns (rng-ns-get-prefix prefix)))
		    (and ns
			 (cons ns local-name)))
		(cons nil local-name))))
	(cond ((not name)
	       (rng-recover-bad-attribute-prefix att))
	      ((member name names)
	       (rng-recover-duplicate-attribute-name att))
	      ((not (rng-match-attribute-name name))
	       (rng-recover-attribute-name att))
	      ((rng-match-text-typed-p)
	       (let ((value (xmltok-attribute-value att)))
		 (if value
		     (or (rng-match-attribute-value value)
			 (rng-recover-attribute-value att))
		   (rng-match-after))))
	      (t (or (rng-match-end-tag)
		     (error "Internal error:\
 invalid on untyped attribute value"))))
	(setq names (cons name names)))
      (setq atts (cdr atts)))))

(defun rng-process-start-tag-close ()
  ;; deal with missing attributes
  (unless (rng-match-start-tag-close)
    (rng-mark-start-tag-close (rng-missing-attributes-message))
    (rng-match-ignore-attributes)))

(defun rng-mark-start-tag-close (&rest args)
  (when (not (eq xmltok-type 'partial-start-tag))
    (rng-mark-invalid (apply 'format args)
		      (- (point)
			 (if (eq xmltok-type 'empty-element)
			     2
			   1))
		      (point))))

(defun rng-recover-bad-element-prefix ()
  (rng-mark-invalid "Prefix not declared"
		    (1+ xmltok-start)
		    xmltok-name-colon)
  (rng-match-unknown-start-tag-open))

(defun rng-recover-bad-attribute-prefix (att)
  (rng-mark-invalid "Prefix not declared"
		    (xmltok-attribute-name-start att)
		    (xmltok-attribute-name-colon att)))

(defun rng-recover-duplicate-attribute-name (att)
  (rng-mark-invalid "Duplicate attribute"
		    (xmltok-attribute-name-start att)
		    (xmltok-attribute-name-end att)))

(defun rng-recover-start-tag-open (name)
  (let ((required (rng-match-required-element-name)))
    (cond ((and required
		(rng-match-start-tag-open required)
		(rng-match-after)
		(rng-match-start-tag-open name))
	   (rng-mark-invalid (concat "Missing element "
				     (rng-quote-string
				      (rng-name-to-string required)))
			     xmltok-start
			     (1+ xmltok-start)))
	  ((and (rng-match-optionalize-elements)
		(rng-match-start-tag-open name))
	   (rng-mark-invalid "Required elements missing"
			     xmltok-start
			     (1+ xmltok-start)))
	  ((rng-match-out-of-context-start-tag-open name)
	   (rng-mark-invalid "Element not allowed in this context"
			     (1+ xmltok-start)
			     xmltok-name-end))
	  (t
	   (rng-match-unknown-start-tag-open)
	   (rng-mark-invalid "Unknown element"
			     (1+ xmltok-start)
			     xmltok-name-end)))))

(defun rng-recover-attribute-value (att)
  (let ((start (xmltok-attribute-value-start att))
	(end (xmltok-attribute-value-end att)))
    (if (= start end)
	(rng-mark-invalid "Empty attribute value invalid" start (1+ end))
      (rng-mark-invalid "Attribute value invalid" start end)))
  (rng-match-after))

(defun rng-recover-attribute-name (att)
  (rng-mark-invalid "Attribute not allowed"
		    (xmltok-attribute-name-start att)
		    (xmltok-attribute-name-end att)))

(defun rng-missing-attributes-message ()
  (let ((required-attributes
	 (rng-match-required-attribute-names)))
    (cond ((not required-attributes)
	   "Required attributes missing")
	  ((not (cdr required-attributes))
	   (concat "Missing attribute "
		   (rng-quote-string
		    (rng-name-to-string (car required-attributes) t))))
	  (t
	   (concat "Missing attributes "
		   (mapconcat (lambda (nm)
				(rng-quote-string
				 (rng-name-to-string nm t)))
			      required-attributes
			      ", "))))))
      
(defun rng-process-end-tag (&optional partial)
  (cond ((not rng-open-elements)
	 (rng-mark-not-well-formed "Extra end-tag"
				   xmltok-start
				   (point)))
	((or partial
	     (equal (cons (xmltok-end-tag-prefix)
			  (xmltok-end-tag-local-name))
		    (car rng-open-elements)))
	 (rng-end-element))
	(t (rng-recover-mismatched-end-tag))))

(defun rng-end-element ()
  (if rng-collecting-text
      (let ((contents (rng-contents-string)))
	(cond ((not contents) (rng-match-after))
	      ((not (rng-match-element-value contents))
	       (let* ((region (rng-contents-region)))
		 (if (not region)
		     (rng-mark-invalid "Empty content not allowed"
				       xmltok-start
				       (+ xmltok-start 2))
		   (rng-mark-invalid "Invalid data"
				     (car region)
				     (cdr region))))
	       (rng-match-after)))
	(setq rng-collecting-text nil)
	(setq rng-pending-contents nil))
    (or (rng-match-end-tag)
	(and (rng-mark-invalid (rng-missing-element-message)
			       xmltok-start
			       (+ xmltok-start 2))
	     (rng-match-after))))
  (rng-ns-pop-state)
  (when (eq (car rng-open-elements) t)
    (rng-pop-tag))
  (rng-pop-tag))

(defun rng-missing-element-message ()
  (let ((element (rng-match-required-element-name)))
    (if element
	(concat "Missing element "
		(rng-quote-string (rng-name-to-string element)))
      "Required child elements missing")))

(defun rng-recover-mismatched-end-tag ()
  (let* ((name (cons (xmltok-end-tag-prefix)
		     (xmltok-end-tag-local-name))))
    (cond ((member name (cdr rng-open-elements))
	   (let* ((suppress-error (eq (car rng-open-elements) t))
		  missing top)
	     (while (progn
		      (setq top (car rng-open-elements))
		      (rng-pop-tag)
		      (unless (eq top t)
			(setq missing (cons top missing))
			(rng-ns-pop-state)
			(rng-match-after)
			(not (equal top name)))))
	     (unless suppress-error
	       (rng-mark-missing-end-tags (cdr missing)))))
	  ((rng-match-empty-before-p)
	   (rng-mark-mismatched-end-tag)
	   (rng-end-element))
	  (t (rng-mark-mismatched-end-tag)
	     (setq rng-open-elements
		   (cons t rng-open-elements))))))

(defun rng-mark-missing-end-tags (missing)
  (rng-mark-not-well-formed
   (format "Missing end-tag%s %s"
	   (if (null (cdr missing)) "" "s")
	   (mapconcat (lambda (name)
			(rng-quote-string
			 (if (car name)
			     (concat (car name)
				     ":"
				     (cdr name))
			   (cdr name))))
		      missing
		      ", "))
   xmltok-start
   (+ xmltok-start 2)))

(defun rng-mark-mismatched-end-tag ()
  (rng-mark-not-well-formed "Mismatched end-tag"
			    (+ xmltok-start 2)
			    xmltok-name-end))

(defun rng-push-tag (prefix-local-name)
  (setq rng-open-elements
	(cons prefix-local-name rng-open-elements)))

(defun rng-pop-tag ()
  (setq rng-open-elements (cdr rng-open-elements)))

(defun rng-contents-string ()
  (let ((contents rng-pending-contents))
    (cond ((not contents) "")
	  ((memq nil contents) nil)
	  ((not (cdr contents))
	   (rng-segment-string (car contents)))
	  (t (apply 'concat
		    (nreverse (mapcar 'rng-segment-string
				      contents)))))))

(defun rng-segment-string (segment)
  (or (car segment)
      (apply 'buffer-substring-no-properties
	     (cdr segment))))

(defun rng-segment-blank-p (segment)
  (if (car segment)
      (rng-blank-p (car segment))
    (apply 'rng-region-blank-p
	   (cdr segment))))

(defun rng-contents-region ()
  (if (null rng-pending-contents)
      nil
    (let* ((contents rng-pending-contents)
	   (head (cdar contents))
	   (start (car head))
	   (end (cadr head)))
      (while (setq contents (cdr contents))
	(setq start (car (cdar contents))))
      (cons start end))))

(defun rng-process-text (start end whitespace &optional value)
  "Process characters between position START and END as text.
END nil means point. WHITESPACE t means known to be whitespace, nil
means known not to be, anything else means unknown whether whitespace
or not. END must not be nil if WHITESPACE is neither t nor nil.
VALUE is a string or nil; nil means the value is equal to the
string between START and END."
  (cond (rng-collecting-text
	 (setq rng-pending-contents (cons (list value start (or end (point)))
					  rng-pending-contents)))
	((not (or (and whitespace
		       (or (eq whitespace t)
			   (if value
			       (rng-blank-p value)
			     (rng-region-blank-p start end))))
		  (rng-match-mixed-text)))
	 (rng-mark-invalid "Text not allowed" start (or end (point))))))

(defun rng-process-unknown-char ()
  (when rng-collecting-text
    (setq rng-pending-contents
	  (cons nil rng-pending-contents))))

(defun rng-process-unknown-entity ()
  (rng-process-unknown-char)
  (rng-match-optionalize-elements))

(defun rng-region-blank-p (beg end)
  (save-excursion
    (goto-char beg)
    (= (skip-chars-forward " \n\r\t" end)
       (- end beg))))

(defun rng-flush-text ()
  (while rng-pending-contents
    (let ((segment (car rng-pending-contents)))
      (unless (or (rng-segment-blank-p segment)
		  (rng-match-mixed-text))
	(let ((region (cdr segment)))
	  (rng-mark-invalid "In this context text cannot be mixed with elements"
			    (car region)
			    (cdr region)))))
    (setq rng-pending-contents (cdr rng-pending-contents))))

(defun rng-process-end-document ()
  ;; this is necessary to clear empty overlays at (point-max)
  (rng-clear-overlays (point) (point))
  (let ((start (save-excursion
		 (skip-chars-backward " \t\r\n")
		 (point))))
    (cond (rng-open-elements
	   (unless (eq (car rng-open-elements) t)
	     (rng-mark-not-well-formed "Missing end-tag"
				       start
				       (point))))
	  ((not (rng-match-nullable-p))
	   (rng-mark-not-well-formed "No document element"
				     start
				     (point))))))

(defun rng-name-to-string (name &optional attributep)
  (let ((ns (car name))
	(local-name (cdr name)))
    (if (or (not ns)
	    (and (not attributep)
		 (eq (rng-ns-get-default) ns)))
	local-name
      (let ((prefix (rng-ns-prefix-for ns)))
	(if prefix
	    (concat prefix ":" local-name)
	  (concat "{" (symbol-name ns) "}" local-name))))))

;;; Completion

(defconst rng-in-start-tag-name-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<\\(?:w\\(?::w?\\)?\\)?\\="
   t
   t))

(defconst rng-in-attribute-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<w\\(?::w\\)?\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
[ \t\r\n]+\\(\\(?:w\\(?::w?\\)?\\)?\\)\\="
   t
   t))

(defconst rng-in-attribute-value-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<w\\(?::w\\)?\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\(\"[^\"]*\\|'[^']*\\)\\="
   t
   t))

(defun rng-complete-symbol ()
  (interactive)
  (let ((lt-pos (save-excursion (search-backward "<" nil t)))
	xmltok-dtd)
    (when (and lt-pos
	       (= (rng-set-state-after lt-pos)
		  lt-pos))
      (cond ((save-excursion
	       (re-search-backward rng-in-start-tag-name-regex
				   lt-pos
				   t))
	     (and rng-collecting-text (rng-flush-text))
	     (let ((name
		    (rng-do-completion (1+ lt-pos)
				       (rng-names-to-completion-alist
					(rng-match-possible-start-tag-names
					 (rng-ns-in-scope)))
				       "Start-tag: ")))
	       (when (and name
			  (rng-match-start-tag-open name))
		 (unless (rng-match-start-tag-close)
		   ;; attributes are required
		   (insert " ")
		   (rng-match-ignore-attributes))
		 (save-excursion
		   (insert (if (rng-match-empty-before-p)
			       "/>"
			     ">"))))))
	    ((save-excursion
	       (re-search-backward rng-in-attribute-regex lt-pos t))
	     (let ((attribute-start (match-beginning 1)))
	       (and (rng-adjust-state-for-attribute lt-pos
						    attribute-start)
		    (rng-do-completion attribute-start
				       (rng-names-to-completion-alist
					(rng-match-possible-attribute-names
					 (rng-ns-in-scope))
					t)
				       "Attribute: ")
		    (progn
		      (insert "=\"\"")
		      (goto-char (1- (point)))))))
	    ((save-excursion
	       (re-search-backward rng-in-attribute-value-regex lt-pos t))
	     (let ((name-start (match-beginning 1))
		   (name-end (match-end 1))
		   (colon (match-beginning 2))
		   (value-start (1+ (match-beginning 3))))
	       (and (rng-adjust-state-for-attribute lt-pos
						    name-start)
		    (rng-adjust-state-for-attribute-value name-start
							  colon
							  name-end)
		    (rng-do-completion value-start
				       (rng-strings-to-completion-alist
					(rng-match-possible-value-strings))
				       "Value: "))))))))
		     
(defun rng-do-completion (start alist prompt)
  (let* ((orig (buffer-substring-no-properties start
					       (point)))
	 (completion (try-completion orig alist)))
    (cond ((not completion)
	   (if (string= orig "")
	       (message "No completions available")
	     (message "No completion for %s"
		      (rng-quote-string orig)))
	   (ding)
	   nil)
	  ((eq completion t)
	   (assoc orig alist))
	  ((not (string= completion orig))
	   (delete-region start (point))
	   (insert completion)
	   (let ((value (cdr (assoc completion alist))))
	     (cond ((not value)
		    (message "Incomplete")
		    nil)
		   ((> (length (all-completions completion alist)) 1)
		    (message "Complete but not unique")
		    nil)
		   ((and (consp value) (eq (cdr value) t))
		    (message "Can only complete prefix")
		    nil)
		   (t value))))
	  (t
	   (setq completion
		 (let ((saved-minibuffer-setup-hook
			(default-value 'minibuffer-setup-hook)))
		   (add-hook 'minibuffer-setup-hook
			     'minibuffer-completion-help
			     t)
		   (unwind-protect
		       (completing-read prompt
					alist
					nil
					nil
					orig)
		     (setq-default minibuffer-setup-hook
				   saved-minibuffer-setup-hook))))
	   (delete-region start (point))
	   (insert completion)
	   (let ((value (cdr (assoc completion alist))))
	     (and (not (and (consp value) (eq (cdr value) t)))
		  value))))))

(defun rng-set-state-after (&optional pos)
  "Set the state for after parsing the first token with endpoint >= POS.
This does not change the xmltok state or point.  However, it does
set `xmltok-dtd'. Returns the position of the end of the token."
  (unless pos (setq pos (point)))
  (when (< rng-validate-up-to-date-end pos)
    (message "Parsing...")
    (while (and (rng-do-some-validation)
		(< rng-validate-up-to-date-end pos))
      ;; Display percentage validated.
      (force-mode-line-update)
      ;; Force redisplay but don't allow idle timers to run.
      (let ((timer-idle-list nil))
	(sit-for 0)))
    (message "Parsing...done"))
  (save-excursion
    (save-restriction
      (widen)
      (rng-with-invisible-motion
	(if (= pos 1)
	    (rng-set-initial-state)
	  (let ((state (get-text-property (1- pos) 'rng-state)))
	    (cond (state
		   (rng-restore-state state)
		   (goto-char pos))
		  (t
		   (let ((start (previous-single-property-change pos
								 'rng-state)))
		     (cond (start
			    (rng-restore-state (get-text-property (1- start)
								  'rng-state))
			    (goto-char start))
			   (t (rng-set-initial-state))))))))
	(xmltok-save
	  (if (= (point) 1)
	      (xmltok-forward-prolog)
	    (setq xmltok-dtd rng-dtd))
	  (cond ((and (< pos (point))
		      ;; This handles the case where the prolog ends
		      ;; with a < without any following name-start
		      ;; character. This will be treated by the parser
		      ;; as part of the prolog, but we want to treat
		      ;; it as the start of the instance.
		      (eq (char-after pos) ?<)
		      (<= (point)
			  (save-excursion
			    (goto-char (1+ pos))
			    (skip-chars-forward " \t\r\n")
			    (point))))
		 pos)
		((< (point) pos)
		 (let ((rng-dt-namespace-context-getter
			'(rng-ns-get-context))
		       (rng-parsing-for-state t))
		   (rng-forward pos))
		 (point))
		(t pos)))))))

(defun rng-adjust-state-for-attribute (lt-pos start)
  (xmltok-save
    (save-excursion
      (goto-char lt-pos)
      (when (memq (xmltok-forward)
		  '(start-tag
		    partial-start-tag
		    empty-element
		    partial-empty-element))
	(when (< start (point))
	  (setq xmltok-namespace-attributes
		(rng-prune-attribute-at start
					xmltok-namespace-attributes))
	  (setq xmltok-attributes
		(rng-prune-attribute-at start
					xmltok-attributes)))
	(let ((rng-parsing-for-state t)
	      (rng-dt-namespace-context-getter '(rng-ns-get-context)))
	  (rng-process-start-tag 'stop)
	  t)))))
	
(defun rng-prune-attribute-at (start atts)
  (when atts
    (let ((cur atts))
      (while (if (eq (xmltok-attribute-name-start (car cur)) start)
		 (progn
		   (setq atts (delq (car cur) atts))
		   nil)
	       (setq cur (cdr cur)))))
    atts))

(defun rng-adjust-state-for-attribute-value (name-start
					     colon
					     name-end)
  (let* ((prefix (if colon
		     (buffer-substring-no-properties name-start colon)
		   nil))
	 (local-name (buffer-substring-no-properties (if colon
							 (1+ colon)
						       name-start)
						     name-end))
	 (ns (and prefix (rng-ns-get-prefix prefix))))
    (and (or (not prefix) ns)
	 (rng-match-attribute-name (cons ns local-name)))))

(defun rng-names-to-completion-alist (names &optional attributep)
  (let (alist)
    (while names
      (let* ((name (car names))
	     (ns (car name))
	     (local-name (cdr name)))
	(when (not (or (eq ns t)
		       (and (not ns)
			    (eq local-name t))))
	  (let ((prefixes (rng-ns-prefixes-for ns attributep)))
	    (while prefixes
	      (let ((prefix (car prefixes)))
		(setq alist
		      (cons (cons (cond ((not prefix) local-name)
					((eq local-name t)
					 (concat prefix ":"))
					(t (concat prefix ":" local-name)))
				  name)
			    alist)))
	      (setq prefixes (cdr prefixes))))))
      (setq names (cdr names)))
    (rng-uniquify-equal (sort alist (lambda (x y)
				      (string< (car x) (car y)))))))

(defun rng-strings-to-completion-alist (strings)
  (mapcar (lambda (s) (cons s s))
	  (rng-uniquify-equal (sort (mapcar 'rng-escape-string strings)
				    'string<))))

(defun rng-escape-string (s)
  (replace-regexp-in-string "[&\"<>]"
			    (lambda (match)
			      (cdr (assoc match
					  '(("&" . "&amp;")
					    ("\"" . "&quot;")
					    (">" . "&gt;")
					    ("<" . "&lt;")))))
			    s
			    t))

(provide 'rng-valid)

;;; rng-valid.el ends here
