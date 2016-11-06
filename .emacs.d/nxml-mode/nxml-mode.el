;;; nxml-mode.el --- a new XML mode

;; Copyright (C) 2003 Thai Open Source Software Center Ltd

;; Author: James Clark <jjc@thaiopensource.com>
;; Keywords: XML

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

;; To use this include rng-auto.el in your .emacs.

;; This uses xmltok.el to do XML parsing. The fundamental problem is
;; how to handle changes. We don't want to maintain a complete parse
;; tree.  We also don't want to reparse from the start of the document
;; on every keystroke.  However, it is not possible in general to
;; parse an XML document correctly starting at a random point in the
;; middle.  The main problems are comments, CDATA sections and
;; processing instructions: these can all contain things that are
;; indistinguishable from elements. Literals in the prolog are also a
;; problem.  Attribute value literals are not a problem because
;; attribute value literals cannot contain less-than signs.
;;
;; Our strategy is to keep track of just the problematic things.
;; Specifically, we keep track of all comments, CDATA sections and
;; processing instructions in the instance.  We do this by marking all
;; except the first character of these with a non-nil nxml-inside text
;; property. The value of the nxml-inside property is comment,
;; cdata-section or processing-instruction.  The first character does
;; not have the nxml-inside property so we can find the beginning of
;; the construct by looking for a change in a text property value
;; (Emacs provides primitives for this).  We use text properties
;; rather than overlays, since the implementation of overlays doesn't
;; look like it scales to large numbers of overlays in a buffer.
;;
;; We don't in fact track all these constructs, but only track them in
;; some initial part of the instance. The variable `nxml-scan-end'
;; contains the limit of where we have scanned up to for them.
;;
;; Thus to parse some random point in the file we first ensure that we
;; have scanned up to that point.  Then we search backwards for a
;; <. Then we check whether the < has an nxml-inside property. If it
;; does we go backwards to first character that does not have an
;; nxml-inside property (this character must be a <).  Then we start
;; parsing forward from the < we have found.
;;
;; The prolog has to be parsed specially, so we also keep track of the
;; end of the prolog in `nxml-prolog-end'. The prolog is reparsed on
;; every change to the prolog.  This won't work well if people try to
;; edit huge internal subsets. Hopefully that will be rare.
;;
;; We keep track of the changes by adding to the buffer's
;; after-change-functions hook.  Scanning is also done as a
;; prerequisite to fontification by adding to fontification-functions
;; (in the same way as jit-lock).  This means that scanning for these
;; constructs had better be quick.  Fortunately it is. Firstly, the
;; typical proportion of comments, CDATA sections and processing
;; instructions is small relative to other things.  Secondly, to scan
;; we just search for the regexp <[!?].
;;
;; One problem is unclosed comments, processing instructions and CDATA
;; sections.  Suppose, for example, we encounter a <!-- but there's no
;; matching -->.  This is not an unexpected situation if the user is
;; creating a comment. It is not helpful to treat the whole of the
;; file starting from the <!-- onwards as a single unclosed comment
;; token. Instead we treat just the <!-- as a piece of not well-formed
;; markup and continue.  The problem is that if at some later stage a
;; --> gets added to the buffer after the unclosed <!--, we will need
;; to reparse the buffer starting from the <!--.  We need to keep
;; track of these reparse dependencies; they are called dependent
;; regions in the code.
;;
;; xmltok.el does not care about context-dependent aspects of the
;; instance document: whether the document contains one top-level
;; element, whether tags match, whether there are no duplicate
;; attributes, whether namespaces are declared.  We expect these
;; things to be dealt with as part of validation.
;;
;; The font locking here is independent of font-lock.el.  We want to
;; do more sophisticated handling of changes and we want to use the
;; same xmltok rather than regexps for parsing so that we parse
;; consistently and correctly.

;;; Code:

(require 'xmltok)
(require 'rng-util)

;;; Customization

(defgroup nxml nil
  "New XML editing mode"
  :group 'languages
  :group 'wp)

(defgroup nxml-highlighting-faces nil
  "Faces for XML syntax highlighting."
  :group 'nxml
  :group 'font-lock-highlighting-faces)

(defcustom nxml-syntax-highlight-flag t
  "*Non-nil means nxml-mode should perform syntax highlighting."
  :group 'nxml
  :type 'boolean)

(defcustom nxml-mode-hook '(rng-validate-mode)
  "Hook run by command `nxml-mode'."
  :group 'nxml
  :type 'hook)

(defcustom nxml-slash-auto-complete-flag t
  "*Non-nil means typing a slash automatically completes the end-tag.
This is used by `nxml-electric-slash'."
  :group 'nxml
  :type 'boolean)

(defcustom nxml-child-indent 2
  "*Indentation for the children of an element relative to the start-tag.
This only applies when the line or lines containing the start-tag contains
nothing else other than that start-tag."
  :group 'nxml
  :type 'integer)

(defcustom nxml-attribute-indent 1
  "*Indentation for the attributes of an element relative to the start-tag.
This only applies when the first attribute of a tag starts a line. In other
cases, the first attribute on one line is indented the same as the first
attribute on the previous line."
  :group 'nxml
  :type 'integer)

(defvar nxml-fontify-chunk-size 500)

;; The two blues have the same hue but contrasting saturation/value.
;; The hue of the green is 120 degrees different from that of the
;; blue.  The red used for highlighting errors is 120 degrees
;; different again.  We use the light blue only for refs and
;; delimiters, since these are short (long stretches in a light color
;; would be too hard to read).  The dark blue is closest to black
;; (which we use by default for text), so we use it for attribute
;; values, which are similar to text.

(defconst nxml-light-blue-color "#9292C9")
(defconst nxml-dark-blue-color "#3A3A7B")
(defconst nxml-green-color "#257A25")

(defface nxml-delimited-data-face
  `((((class color) (background light)) (:foreground ,nxml-dark-blue-color)))
  "Face used to highlight data enclosed between delimiters.
By default, this is inherited by `nxml-attribute-value-face'
and `nxml-processing-instruction-content-face'."
  :group 'nxml-highlighting-faces)

(defface nxml-name-face
  `((((class color) (background light)) (:foreground ,nxml-green-color)))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'nxml-highlighting-faces)

(defface nxml-ref-face
  `((((class color) (background light)) (:foreground ,nxml-light-blue-color)))
  "Face used to highlight character and entity references.
This is not used directly, but only via inheritance by other faces."
  :group 'nxml-highlighting-faces)

(defface nxml-delimiter-face
  `((((class color) (background light)) (:foreground ,nxml-light-blue-color))
    (t (:bold t)))
  "Face used to highlight delimiters.
This is not used directly, but only via inheritance by other faces."
  :group 'nxml-highlighting-faces)

(defface nxml-text-face
  nil
  "Face used to highlight text."
  :group 'nxml-highlighting-faces)

(defface nxml-comment-content-face
  '((t (:italic t)))
  "Face used to highlight the content of comments."
  :group 'nxml-highlighting-faces)

(defface nxml-comment-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of comments, i.e <!-- and -->."
  :group 'nxml-highlighting-faces)

(defface nxml-processing-instruction-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of processing instructions, i.e <? and ?>."
  :group 'nxml-highlighting-faces)

(defface nxml-processing-instruction-target-face
  '((t (:inherit nxml-name-face)))
  "Face used for the target of processing instructions."
  :group 'nxml-highlighting-faces)

(defface nxml-processing-instruction-content-face
  '((t (:inherit nxml-delimited-data-face)))
  "Face used for the content of processing instructions."
  :group 'nxml-highlighting-faces)

(defface nxml-cdata-section-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of CDATA sections, i.e <![, [, and ]]>."
  :group 'nxml-highlighting-faces)

(defface nxml-cdata-section-CDATA-face
  '((t (:inherit nxml-name-face)))
  "Face used for the CDATA keyword in CDATA sections."
  :group 'nxml-highlighting-faces)

(defface nxml-cdata-section-content-face
  '((t (:inherit nxml-text-face)))
  "Face used for the content of CDATA sections."
  :group 'nxml-highlighting-faces)

(defface nxml-char-ref-number-face
  '((t (:inherit nxml-ref-face)))
  "Face used for the number in character references.
This includes ths `x' in hex references."
  :group 'nxml-highlighting-faces)

(defface nxml-char-ref-delimiter-face
  '((t (:inherit nxml-ref-face)))
  "Face used for the delimiters of character references, i.e &# and ;."
  :group 'nxml-highlighting-faces)

(defface nxml-entity-ref-name-face
  '((t (:inherit nxml-ref-face)))
  "Face used for the entity name in general entity references."
  :group 'nxml-highlighting-faces)

(defface nxml-entity-ref-delimiter-face
  '((t (:inherit nxml-ref-face)))
  "Face used for the delimiters of entity references, i.e & and ;."
  :group 'nxml-highlighting-faces)

(defface nxml-tag-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the angle brackets delimiting tags.
`nxml-tag-slash-face' is used for slashes."
  :group 'nxml-highlighting-faces)

(defface nxml-tag-slash-face
  '((t (:inherit nxml-name-face)))
  "Face used for slashes in tags, both in end-tags and empty-elements."
  :group 'nxml-highlighting-faces)

(defface nxml-element-prefix-face
  '((t (:inherit nxml-name-face)))
  "Face used for the prefix of elements."
  :group 'nxml-highlighting-faces)

(defface nxml-element-colon-face
  '((t (:inherit nxml-name-face)))
  "Face used for the colon in element names."
  :group 'nxml-highlighting-faces)

(defface nxml-element-local-name-face
  '((t (:inherit nxml-name-face)))
  "Face used for the local name of elements."
  :group 'nxml-highlighting-faces)

(defface nxml-attribute-prefix-face
  '((t (:inherit nxml-name-face)))
  "Face used for the prefix of attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-attribute-colon-face
  '((t (:inherit nxml-name-face)))
  "Face used for the colon in attribute names."
  :group 'nxml-highlighting-faces)
  
(defface nxml-attribute-local-name-face
  '((t (:inherit nxml-name-face)))
  "Face used for the local name of attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-xmlns-face
  '((t (:inherit nxml-name-face)))
  "Face used for `xmlns' in namespace attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-colon-face
  '((t (:inherit nxml-name-face)))
  "Face used for the colon in namespace attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-prefix-face
  '((t (:inherit nxml-name-face)))
  "Face used for the prefix declared in namespace attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-attribute-value-face
  '((t (:inherit nxml-delimited-data-face)))
  "Face used for the value of attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-attribute-value-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of attribute values."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-value-face
  '((t (:inherit nxml-attribute-value-face)))
  "Face used for the value of namespace attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-value-delimiter-face
  '((t (:inherit nxml-attribute-value-delimiter-face)))
  "Face used for the delimiters of namespace attribute values."
  :group 'nxml-highlighting-faces)

(defface nxml-prolog-literal-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of literals in the prolog."
  :group 'nxml-highlighting-faces)

(defface nxml-prolog-literal-content-face
  '((t (:inherit nxml-delimited-data-face)))
  "Face used for the content of literals in the prolog."
  :group 'nxml-highlighting-faces)

(defface nxml-prolog-keyword-face
  '((t (:inherit nxml-name-face)))
  "Face used for keywords in the prolog."
  :group 'nxml-highlighting-faces)

(defface nxml-markup-declaration-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of markup declarations in the prolog.
The delimiters are <! and >."
  :group 'nxml-highlighting-faces)

(defface nxml-hash-face
  '((t (:inherit nxml-name-face)))
  "Face used for # before a name in the prolog."
  :group 'nxml-highlighting-faces)

;;; Global variables

(defvar nxml-prolog-end nil
  "Integer giving position following end of the prolog.")
(make-variable-buffer-local 'nxml-prolog-end)

(defvar nxml-prolog-regions nil
  "List of regions in the prolog to be fontified.
See the function `xmltok-forward-prolog' for more information.")
(make-variable-buffer-local 'nxml-prolog-regions)

(defvar nxml-scan-end nil
  "Marker giving position up to which we have scanned.
nxml-scan-end must be >= nxml-prolog-end.  Furthermore, nxml-scan-end
must not an inside position in the following sense. A position is
inside if the following character is a part of, but not the first
character of, a CDATA section, comment or processing instruction.
Furthermore all positions >= nxml-prolog-end and < nxml-scan-end that
are inside positions must have a non-nil nxml-inside property whose
value is a symbol specifying what it is inside. Any characters with a
non-nil fontified property must have position < nxml-scan-end and the
correct face. Dependent regions must also be established for any
unclosed constructs starting before nxml-scan-end.
There must be no nxml-inside properties after nxml-scan-end.")
(make-variable-buffer-local 'nxml-scan-end)

(defvar nxml-last-fontify-end nil
  "Position where fontification last ended.
Nil if the buffer changed since the last fontification.")
(make-variable-buffer-local 'nxml-last-fontify-end)

(defvar nxml-degraded nil
  "Non-nil if currently operating in degraded mode.
Degraded mode is enabled when an internal error is encountered in the
fontification or after-change functions.")
(make-variable-buffer-local 'nxml-degraded)

(defvar nxml-mode-map nil)

(defsubst nxml-get-inside (pos)
  (get-text-property pos 'nxml-inside))

(defsubst nxml-clear-inside (start end)
  (remove-text-properties start end '(nxml-inside nil)))

(defsubst nxml-set-inside (start end type)
  (put-text-property start end 'nxml-inside type))

(defun nxml-inside-end (pos)
  "Return the end of the inside region containing POS.
Return nil if the character at POS is not inside."
  (if (nxml-get-inside pos)
      (or (next-single-property-change pos 'nxml-inside)
	  (point-max))
    nil))

(defun nxml-inside-start (pos)
  "Return the start of the inside region containing POS.
Return nil if the character at POS is not inside."
  (if (nxml-get-inside pos)
      (or (previous-single-property-change (1+ pos) 'nxml-inside)
	  (point-min))
    nil))

(defsubst nxml-set-face (start end face)
  (when (and face (< start end))
    (put-text-property start end 'face face)))

(defsubst nxml-clear-face (start end)
  (remove-text-properties start end '(face nil)))

(defsubst nxml-set-fontified (start end)
  (put-text-property start end 'fontified t))

(defsubst nxml-clear-fontified (start end)
  (remove-text-properties start end '(fontified nil)))

;;;###autoload
(defun nxml-mode ()
  "Major mode for editing XML.

Syntax highlighting is performed unless the variable
`nxml-syntax-highlight-flag' is nil.

Inserting a slash automatically inserts the rest of the end-tag or
empty-element unless `nxml-slash-auto-complete-flag' is nil.  You can
insert an end-tag for the current element using
\\[nxml-insert-end-tag].

Validation and schema-sensitive editing is provided by the related
minor-mode `rng-validate-mode'. By default, this is enabled in the
`nxml-mode-hook'.  You can toggle it using \\[rng-validate-mode].

\\[indent-for-tab-command] indents the current line appropriately.
This can be customized using the variable `nxml-child-indent'
and the variable `nxml-attribute-indent'.

The Emacs commands that normally operate on balanced expressions will
operate on XML markup items.  Thus \\[forward-sexp] will move forward
across one markup item; \\[backward-sexp] will move backward across
one markup item; \\[kill-sexp] will kill the following markup item;
\\[mark-sexp] will mark the following markup item.  Different things
are recognized as markup items in different context.  Within element
content, the markup items are tags, entity and character references,
processing instructions, strings with no markup, comments.  Within a
start-tag, the markup items are the element name and the attributes.
For more details, see the function `nxml-forward-balanced-item'.

\\[nxml-backward-up-element] moves up to the start-tag of the
containing element."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'nxml-mode)
  (setq mode-name "nXML")
  (make-local-variable 'forward-sexp-function)
  (setq forward-sexp-function 'nxml-forward-balanced-item)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'nxml-indent-line)
  (unless nxml-mode-map
    (setq nxml-mode-map
	  (let ((map (make-sparse-keymap)))
	    (define-key map "\M-\C-u" 'nxml-backward-up-element)
	    (define-key map "\C-c/" 'nxml-insert-end-tag)
	    (define-key map "\C-c\C-v" 'rng-validate-mode)
	    (define-key map "/" 'nxml-electric-slash)
	    map)))
  (use-local-map nxml-mode-map)
  (save-excursion
    (save-restriction
      (widen)
      (nxml-clear-dependent-regions (point-min) (point-max))
      (setq nxml-scan-end (copy-marker (point-min) nil))
      (rng-with-unmodifying-text-property-changes
	(when nxml-syntax-highlight-flag
	  (nxml-clear-fontified (point-min) (point-max)))
	(nxml-clear-inside (point-min) (point-max))
	(rng-with-invisible-motion
	  (nxml-scan-prolog)))))
  (when nxml-syntax-highlight-flag
    (add-hook 'fontification-functions 'nxml-fontify))
  (add-hook 'after-change-functions 'nxml-after-change nil t)
  (run-hooks 'nxml-mode-hook))

(defun nxml-degrade (err)
  (message "Internal nXML mode error (%s), degrading"
	   (error-message-string err))
  (ding)
  (setq nxml-degraded t)
  (setq nxml-prolog-end 1)
  (save-excursion
    (save-restriction
      (widen)
      (rng-with-unmodifying-text-property-changes
	(nxml-clear-face (point-min) (point-max))
	(nxml-set-fontified (point-min) (point-max))
	(nxml-clear-inside (point-min) (point-max)))
      (setq mode-name "nXML/degraded"))))

;;; Change management

(defun nxml-after-change (start end pre-change-length)
  (unless nxml-degraded
    (condition-case err
	(save-excursion
	  (save-restriction
	    (widen)
	    (save-match-data
	      (rng-with-invisible-motion
		(rng-with-unmodifying-text-property-changes
		  (nxml-after-change1 start end pre-change-length))))))
      (error
       (nxml-degrade err)))))

(defun nxml-after-change1 (start end pre-change-length)
  (setq nxml-last-fontify-end nil)
  (setq start
	(nxml-adjust-start-for-dependent-regions start
						 end
						 pre-change-length))
  (when (<= start
	    ;; Add 2 so as to include the < and following char
	    ;; that start the instance, since changing these
	    ;; can change where the prolog ends.
	    (+ nxml-prolog-end 2))
    ;; end must be extended to at least the end of the old prolog
    (when (< (+ start pre-change-length) nxml-prolog-end)
      (setq end
	    (+ end (- nxml-prolog-end start pre-change-length))))
    (nxml-scan-prolog))
  (cond ((<= end nxml-prolog-end)
	 (setq end nxml-prolog-end)
	 (goto-char start)
	 ;; This is so that Emacs redisplay works
	 (setq start (line-beginning-position)))
	((and (<= start nxml-scan-end)
	      (> start (point-min))
	      (nxml-get-inside (1- start)))
	 ;; The closing delimiter might have been removed.
	 ;; So we may need to redisplay from the beginning
	 ;; of the token.
	 (goto-char (1- start))
	 (nxml-move-outside-backwards)
	 ;; This is so that Emacs redisplay works
	 (setq start (line-beginning-position))
	 (setq end (max (nxml-scan-after-change (point) end)
			end)))
	(t
	 (goto-char start)
	 ;; This is both for redisplay and to move back
	 ;; past any incomplete opening delimiters
	 (setq start (line-beginning-position))
	 (setq end (max (nxml-scan-after-change start end)
			end))))
  (when nxml-syntax-highlight-flag
    (when (>= start end)
      ;; Must clear at least one char so as to trigger redisplay.
      (cond ((< start (point-max))
	     (setq end (1+ start)))
	    (t
	     (setq end (point-max))
	     (goto-char end)
	     (setq start (line-beginning-position)))))
    (nxml-clear-fontified start end)))
  
(defun nxml-scan-after-change (start end)
  "Restore `nxml-scan-end' invariants after a change.
The change happened between START and END.
Return position after which lexical state is unchanged.
END must be > nxml-prolog-end."
  (if (>= start nxml-scan-end)
      nxml-scan-end
    (goto-char start)
    (nxml-move-outside-backwards)
    (setq start (point))
    (let ((inside-remove-start start)
	  xmltok-errors
	  xmltok-dependent-regions)
      (while (or (when (xmltok-forward-special (min end nxml-scan-end))
		   (when (memq xmltok-type
			       '(comment
				 cdata-section
				 processing-instruction))
		     (nxml-clear-inside inside-remove-start
					(1+ xmltok-start))
		     (nxml-set-inside (1+ xmltok-start)
				      (point)
				      xmltok-type)
		     (setq inside-remove-start (point)))
		   (if (< (point) (min end nxml-scan-end))
		       t
		     (setq end (point))
		     nil))
		 ;; The end of the change was inside but is now outside.
		 ;; Imagine something really weird like
		 ;; <![CDATA[foo <!-- bar ]]> <![CDATA[ stuff --> <!-- ]]> -->
		 ;; and suppose we deleted "<![CDATA[f"
		 (let ((inside-end (nxml-inside-end end)))
		   (when inside-end
		     (setq end inside-end)
		     t))))
      (nxml-clear-inside inside-remove-start end)
      (nxml-clear-dependent-regions start end)
      (nxml-mark-parse-dependent-regions))
    (when (> end nxml-scan-end)
      (set-marker nxml-scan-end end))
    end))

(defun nxml-scan-prolog ()
  (goto-char (point-min))
  (let (xmltok-dtd
	xmltok-errors
	xmltok-dependent-regions)
    (setq nxml-prolog-regions (xmltok-forward-prolog))
    (setq nxml-prolog-end (point))
    (nxml-clear-inside (point-min) nxml-prolog-end)
    (nxml-clear-dependent-regions (point-min) nxml-prolog-end)
    (nxml-mark-parse-dependent-regions))
  (when (< nxml-scan-end nxml-prolog-end)
    (set-marker nxml-scan-end nxml-prolog-end)))

;;; Dependent regions

(defun nxml-adjust-start-for-dependent-regions (start end pre-change-length)
  (let ((overlays (overlays-in (1- start) start))
	(adjusted-start start))
    (while overlays
      (let* ((overlay (car overlays))
	     (ostart (overlay-start overlay)))
	(when (and (eq (overlay-get overlay 'category) 'nxml-dependent)
		   (< ostart adjusted-start))
	  (let ((funargs (overlay-get overlay 'nxml-funargs)))
	    (when (apply (car funargs)
			 (append (list start
				       end
				       pre-change-length
				       ostart
				       (overlay-end overlay))
				 (cdr funargs)))
	      (setq adjusted-start ostart)))))
      (setq overlays (cdr overlays)))
    adjusted-start))
		  
(defun nxml-mark-parse-dependent-regions ()
  (while xmltok-dependent-regions
    (apply 'nxml-mark-parse-dependent-region
	   (car xmltok-dependent-regions))
    (setq xmltok-dependent-regions
	  (cdr xmltok-dependent-regions))))

(defun nxml-mark-parse-dependent-region (fun start end &rest args)
  (let ((overlay (make-overlay start end nil t t)))
    (overlay-put overlay 'category 'nxml-dependent)
    (overlay-put overlay 'nxml-funargs (cons fun args))))

(put 'nxml-dependent 'evaporate t)

(defun nxml-clear-dependent-regions (start end)
  (let ((overlays (overlays-in start end)))
    (while overlays
      (let* ((overlay (car overlays))
	     (category (overlay-get overlay 'category)))
	(when (and (eq category 'nxml-dependent)
		   (<= start (overlay-start overlay)))
	  (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))

;;; Fontification

(defun nxml-fontify (start)
  (condition-case err
      (save-excursion
	(save-restriction
	  (widen)
	  (save-match-data
	    (rng-with-invisible-motion
	      (rng-with-unmodifying-text-property-changes
		(if nxml-degraded
		    (nxml-set-fontified start (point-max))
		  (nxml-fontify1 start)))))))
    (error
     (nxml-degrade err))))

(defun nxml-fontify1 (start)
  (cond ((< start nxml-prolog-end)
	 (nxml-fontify-prolog)
	 (nxml-set-fontified (point-min)
			     nxml-prolog-end))
	(t
	 (goto-char start)
	 (when (not (eq nxml-last-fontify-end start))
	   (when (not (equal (char-after) ?\<))
	     (search-backward "<" nxml-prolog-end t))
	   (nxml-ensure-scan-up-to-date)
	   (nxml-move-outside-backwards))
	 (let ((start (point)))
	   (nxml-do-fontify (min (point-max)
				 (+ start nxml-fontify-chunk-size)))
	   (setq nxml-last-fontify-end (point))
	   (nxml-set-fontified start nxml-last-fontify-end)))))

(defun nxml-fontify-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (rng-with-invisible-motion
	(goto-char (point-min))
	(rng-with-unmodifying-text-property-changes
	  (nxml-fontify-prolog)
	  (goto-char nxml-prolog-end)
	  (nxml-do-fontify))))))

(defun nxml-fontify-prolog ()
  "Fontify the prolog.
The buffer is assumed to be prepared for fontification.
This does not set the fontified property, but it does clear
faces appropriately."
  (let ((regions nxml-prolog-regions))
    (nxml-clear-face (point-min) nxml-prolog-end)
    (while regions
      (let ((region (car regions)))
	(nxml-apply-fontify-rule (aref region 0)
				 (aref region 1)
				 (aref region 2)))
      (setq regions (cdr regions)))))

(defun nxml-do-fontify (&optional bound)
  "Fontify at least as far as bound.
Leave point after last fontified position."
  (unless bound (setq bound (point-max)))
  (let (xmltok-dependent-regions
	xmltok-errors)
    (while (and (< (point) bound)
		(nxml-tokenize-forward))
      (nxml-clear-face xmltok-start (point))
      (nxml-apply-fontify-rule))))

;; Vectors identify a substring of the token to be highlighted in some face.

;; Token types returned by xmltok-forward.

(put 'start-tag
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [-1 nil nxml-tag-delimiter-face]
       (element-qname . 1)
       attributes))

(put 'partial-start-tag
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       (element-qname . 1)
       attributes))

(put 'end-tag
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [1 2 nxml-tag-slash-face]
       [-1 nil nxml-tag-delimiter-face]
       (element-qname . 2)))

(put 'partial-end-tag
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [1 2 nxml-tag-slash-face]
       (element-qname . 2)))

(put 'empty-element
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [-2 -1 nxml-tag-slash-face]
       [-1 nil nxml-tag-delimiter-face]
       (element-qname . 1)
       attributes))

(put 'partial-empty-element
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [-1 nil nxml-tag-slash-face]
       (element-qname . 1)
       attributes))

(put 'char-ref
     'nxml-fontify-rule
     '([nil 2 nxml-char-ref-delimiter-face]
       [2 -1 nxml-char-ref-number-face]
       [-1 nil nxml-char-ref-delimiter-face]))

(put 'entity-ref
     'nxml-fontify-rule
     '([nil 1 nxml-entity-ref-delimiter-face]
       [1 -1 nxml-entity-ref-name-face]
       [-1 nil nxml-entity-ref-delimiter-face]))

(put 'comment
     'nxml-fontify-rule
     '([nil 4 nxml-comment-delimiter-face]
       [4 -3 nxml-comment-content-face]
       [-3 nil nxml-comment-delimiter-face]))

(put 'processing-instruction
     'nxml-fontify-rule
     '([nil 2 nxml-processing-instruction-delimiter-face]
       [-2 nil nxml-processing-instruction-delimiter-face]
       processing-instruction-content))

(put 'cdata-section
     'nxml-fontify-rule
     '([nil 3 nxml-cdata-section-delimiter-face] ; <![
       [3 8 nxml-cdata-section-CDATA-face] ; CDATA
       [8 9 nxml-cdata-section-delimiter-face] ; [
       [9 -3 nxml-cdata-section-content-face] ; ]]>
       [-3 nil nxml-cdata-section-delimiter-face]))

(put 'data
     'nxml-fontify-rule
     '([nil nil nxml-text-face]))

;; Prolog region types in list returned by xmltok-forward-prolog.

(put 'xml-declaration
     'nxml-fontify-rule
     '([nil 2 nxml-processing-instruction-delimiter-face]
       [2 5 nxml-processing-instruction-target-face]
       [-2 nil nxml-processing-instruction-delimiter-face]))

(put 'xml-declaration-attribute-name
     'nxml-fontify-rule
     '([nil nil nxml-attribute-local-name-face]))

(put 'xml-declaration-attribute-value
     'nxml-fontify-rule
     '([nil 1 nxml-attribute-value-delimiter-face]
       [1 -1 nxml-attribute-value-face]
       [-1 nil nxml-attribute-value-delimiter-face]))

(put 'processing-instruction-left
     'nxml-fontify-rule
     '([nil 2 nxml-processing-instruction-delimiter-face]
       [2 nil nxml-processing-instruction-target-face]))

(put 'processing-instruction-right
     'nxml-fontify-rule
     '([nil -2 nxml-processing-instruction-content-face]
       [-2 nil nxml-processing-instruction-delimiter-face]))

(put 'literal
     'nxml-fontify-rule
     '([nil 1 nxml-prolog-literal-delimiter-face]
       [1 -1 nxml-prolog-literal-content-face]
       [-1 nil nxml-prolog-literal-delimiter-face]))

(put 'keyword
     'nxml-fontify-rule
     '([nil nil nxml-prolog-keyword-face]))

(put 'markup-declaration-open
     'nxml-fontify-rule
     '([0 2 nxml-markup-declaration-delimiter-face]
       [2 nil nxml-prolog-keyword-face]))

(put 'markup-declaration-close
     'nxml-fontify-rule
     '([nil nil nxml-markup-declaration-delimiter-face]))

(put 'internal-subset-open
     'nxml-fontify-rule
     '([nil nil nxml-markup-declaration-delimiter-face]))

(put 'internal-subset-close
     'nxml-fontify-rule
     '([nil 1 nxml-markup-declaration-delimiter-face]
       [-1 nil nxml-markup-declaration-delimiter-face]))

(put 'hash-name
     'nxml-fontify-rule
     '([nil 1 nxml-hash-face]
       [1 nil nxml-prolog-keyword-face]))

(defun nxml-apply-fontify-rule (&optional type start end)
  (let ((rule (get (or type xmltok-type) 'nxml-fontify-rule)))
    (unless start (setq start xmltok-start))
    (unless end (setq end (point)))
    (while rule
      (let* ((action (car rule)))
	(setq rule (cdr rule))
	(cond ((vectorp action)
	       (nxml-set-face (let ((offset (aref action 0)))
				(cond ((not offset) start)
				      ((< offset 0) (+ end offset))
				      (t (+ start offset))))
			      (let ((offset (aref action 1)))
				(cond ((not offset) end)
				      ((< offset 0) (+ end offset))
				      (t (+ start offset))))
			      (aref action 2)))
	      ((and (consp action)
		    (eq (car action) 'element-qname))
	       (when xmltok-name-end ; maybe nil in partial-end-tag case
		 (nxml-fontify-qname (+ start (cdr action))
				     xmltok-name-colon
				     xmltok-name-end
				     'nxml-element-prefix-face
				     'nxml-element-colon-face
				     'nxml-element-local-name-face)))
	      ((eq action 'attributes)
	       (nxml-fontify-attributes))
	      ((eq action 'processing-instruction-content)
	       (nxml-set-face (+ start 2)
			      xmltok-name-end
			      'nxml-processing-instruction-target-face)
	       (nxml-set-face (save-excursion
				(goto-char xmltok-name-end)
				(skip-chars-forward " \t\r\n")
				(point))
			      (- end 2)
			      'nxml-processing-instruction-content-face))
	      (t (error "Invalid nxml-fontify-rule action %s" action)))))))

(defun nxml-fontify-attributes ()
  (while xmltok-namespace-attributes
    (nxml-fontify-attribute (car xmltok-namespace-attributes)
			    'namespace)
    (setq xmltok-namespace-attributes
	  (cdr xmltok-namespace-attributes)))
  (while xmltok-attributes
    (nxml-fontify-attribute (car xmltok-attributes))
    (setq xmltok-attributes
	  (cdr xmltok-attributes))))

(defun nxml-fontify-attribute (att &optional namespace-declaration)
  (if namespace-declaration
      (nxml-fontify-qname (xmltok-attribute-name-start att)
			  (xmltok-attribute-name-colon att)
			  (xmltok-attribute-name-end att)
			  'nxml-namespace-attribute-xmlns-face
			  'nxml-namespace-attribute-colon-face
			  'nxml-namespace-attribute-prefix-face
			  'nxml-namespace-attribute-xmlns-face)
    (nxml-fontify-qname (xmltok-attribute-name-start att)
			(xmltok-attribute-name-colon att)
			(xmltok-attribute-name-end att)
			'nxml-element-prefix-face
			'nxml-element-colon-face
			'nxml-element-local-name-face))
  (let ((start (xmltok-attribute-value-start att))
	(end (xmltok-attribute-value-end att))
	(refs (xmltok-attribute-refs att))
	(delimiter-face (if namespace-declaration
			    'nxml-namespace-attribute-value-delimiter-face
			  'nxml-attribute-value-delimiter-face))
	(value-face (if namespace-declaration
			'nxml-namespace-attribute-value-face
		      'nxml-attribute-value-face)))
    (when start
      (nxml-set-face (1- start) start delimiter-face)
      (nxml-set-face end (1+ end) delimiter-face)
      (while refs
	(let* ((ref (car refs))
	       (ref-type (aref ref 0))
	       (ref-start (aref ref 1))
	       (ref-end (aref ref 2)))
	  (nxml-set-face start ref-start value-face)
	  (nxml-apply-fontify-rule ref-type ref-start ref-end)
	  (setq start ref-end))
	(setq refs (cdr refs)))
      (nxml-set-face start end value-face))))

(defun nxml-fontify-qname (start
			   colon
			   end
			   prefix-face
			   colon-face
			   local-name-face
			   &optional
			   unprefixed-face)
  (cond (colon (nxml-set-face start colon prefix-face)
	       (nxml-set-face colon (1+ colon) colon-face)
	       (nxml-set-face (1+ colon) end local-name-face))
	(t (nxml-set-face start end (or unprefixed-face
					local-name-face)))))

;;; Editing

(defun nxml-electric-slash (arg)
  "Insert a slash.

With a prefix ARG, do nothing other than insert the slash.

Otherwise, if `nxml-slash-auto-complete-flag' is non-nil, insert the
rest of the end-tag or empty-element if the slash is potentially part
of an end-tag or the close of an empty-element.

If the slash is part of an end-tag that is the first non-whitespace
on the line, reindent the line."
  (interactive "*P")
  (nxml-ensure-scan-up-to-date)
  (let* ((slash-pos (point))
	 (end-tag-p (and (eq (char-before slash-pos) ?<)
			 (not (nxml-get-inside slash-pos))))
	 (at-indentation (save-excursion
			   (back-to-indentation)
			   (eq (point) (1- slash-pos)))))
    (self-insert-command (prefix-numeric-value arg))
    (unless arg
      (if nxml-slash-auto-complete-flag
	  (if end-tag-p
	      (condition-case err
		  (let ((start-tag-end
			 (nxml-scan-element-backward (1- slash-pos) t)))
		    (when start-tag-end
		      (insert (xmltok-start-tag-qname) ">")
		      ;; copy the indentation of the start-tag
		      (when (and at-indentation
				 (save-excursion
				   (goto-char xmltok-start)
				   (back-to-indentation)
				   (eq (point) xmltok-start)))
			(save-excursion
			  (indent-line-to (save-excursion
					    (goto-char xmltok-start)
					    (current-column)))))))
		(nxml-scan-error nil))
	    (when (and (eq (nxml-token-before) (point))
		       (eq xmltok-type 'partial-empty-element))
	      (insert ">")))
	(when (and end-tag-p at-indentation)
	  (nxml-indent-line))))))

(defun nxml-insert-end-tag ()
  "Insert an end-tag for the current element."
  (interactive)
  (let* ((token-end (nxml-token-before))
	 (start-tag-end
	  (save-excursion
	    (when (and (< (point) token-end)
		       (memq xmltok-type
			     '(cdata-section
			       processing-instruction
			       comment
			       start-tag
			       end-tag
			       empty-element)))
	      (error "Point is inside a %s"
		     (nxml-token-type-friendly-name xmltok-type)))
	    (nxml-scan-element-backward token-end t))))
    (unless (eq xmltok-type 'start-tag)
      (error "No matching start-tag"))
    (when (save-excursion
	    (and (progn
		   (goto-char xmltok-start)
		   (back-to-indentation)
		   (eq (point) xmltok-start))
		 (progn
		   (goto-char start-tag-end)
		   (looking-at "[ \t\r\n]*$"))))
      (unless (<= (point)
		  (save-excursion
		    (back-to-indentation)
		    (point)))
	(insert "\n"))
      (indent-line-to (save-excursion
			(goto-char xmltok-start)
			(current-column))))
    (insert "</" (xmltok-start-tag-qname) ">")))

;;; Indentation

(defun nxml-indent-line ()
  "Indent current line as XML."
  (let ((indent (nxml-compute-indent))
	(from-end (- (point-max) (point))))
    (when indent
      (beginning-of-line)
      (let ((bol (point)))
	(skip-chars-forward " \t")
	(delete-region bol (point)))
      (indent-to indent)
      (when (> (- (point-max) from-end) (point))
	(goto-char (- (point-max) from-end))))))

(defun nxml-compute-indent ()
  "Return the indent for the current line.
Return nil if the indent of the current line should not be changed."
  (nxml-ensure-scan-up-to-date)
  (let ((exdent 0) token-end target-line-start)
    (save-excursion
      (back-to-indentation)
      (setq target-line-start (point))
      (setq token-end (nxml-token-after))
      (when (and (eq xmltok-start (point))
		 (memq xmltok-type '(end-tag partial-end-tag))
		 (save-excursion
		   (goto-char token-end)
		   (looking-at "[ \t]*$")))
	(setq exdent (- nxml-child-indent)))
      (skip-chars-backward " \r\n\t")
      (back-to-indentation)
      (setq token-end (nxml-token-after))
      (+ exdent
	 (cond (;; preceding lines consisted entirely of a single token
		(and (memq xmltok-type '(start-tag
					 end-tag
					 empty-element
					 comment
					 cdata-section))
		     (< token-end target-line-start)
		     (save-excursion
		       (goto-char xmltok-start)
		       (back-to-indentation)
		       (eq (point) xmltok-start))
		     (save-excursion
		       (goto-char token-end)
		       (looking-at "[ \t]*$")))
		(goto-char xmltok-start)
		(+ (current-column)
		   (if (eq xmltok-type 'start-tag)
		       nxml-child-indent
		     0)))
	       (;; the second non-blank line of a multi-line tag
		(and (memq xmltok-type '(start-tag
					 empty-element
					 partial-empty-element
					 partial-start-tag))
		     (save-excursion
		       (goto-char xmltok-start)
		       (back-to-indentation)
		       (eq (point) xmltok-start))
		     (save-excursion
		       (goto-char target-line-start)
		       (skip-chars-backward " \t\r\n")
		       (back-to-indentation)
		       (eq (point) xmltok-start))
		     (<= target-line-start
			 (save-excursion
			   (goto-char token-end)
			   (skip-chars-forward " \t\r\n")
			   (point))))
		(goto-char xmltok-start)
		(or (save-excursion
		      (skip-chars-forward "^ \t\r\n") 
		      (skip-chars-forward " \t")
		      (and (looking-at ".") (current-column)))
		    (+ (current-column) nxml-attribute-indent)))
	       (t (current-column)))))))

;;; Movement

(defun nxml-forward-balanced-item (&optional arg)
  "Move forward across one balanced item.
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions.
This is the equivalent of `forward-sexp' for XML.

An element contains as items strings with no markup, elements,
processing instructions, comments, CDATA sections, entity references
and characters references.  A start-tag contains an element name
followed by one or more attributes.  An end-tag contains just an
element name.  An attribute value literals contains strings with no
markup, entity references and character references.  A processing
instruction consists of a target and a content string.  A comment or a
CDATA section contains a single string.  An entity reference contains
a single name.  A character reference contains a character number."
  (interactive "p")
  (or arg (setq arg 1))
  (cond ((> arg 0)
	 (while (progn
		  (nxml-forward-single-balanced-item)
		  (> (setq arg (1- arg)) 0))))
	((< arg 0)
	 (while (progn
		  (nxml-backward-single-balanced-item)
		  (< (setq arg (1+ arg)) 0))))))

(defun nxml-forward-single-balanced-item ()
  (condition-case err
      (goto-char (let ((end (nxml-token-after)))
		   (save-excursion
		     (while (eq xmltok-type 'space)
		       (goto-char end)
		       (setq end (nxml-token-after)))
		     (cond ((/= (point) xmltok-start)
			    (nxml-scan-forward-within end))
			   ((eq xmltok-type 'start-tag)
			    ;; can't ever return nil here
			    (nxml-scan-element-forward xmltok-start))
			   ((memq xmltok-type
				  '(end-tag partial-end-tag))
			    (error "Already at end of element"))
			   (t end)))))
    (nxml-scan-error
     (goto-char (cadr err))
     (apply 'error (cddr err)))))

(defun nxml-backward-single-balanced-item ()
  (condition-case err
      (goto-char (let ((end (nxml-token-before)))
		   (save-excursion
		     (while (eq xmltok-type 'space)
		       (goto-char xmltok-start)
		       (setq end (nxml-token-before)))
		     (cond ((/= (point) end)
			    (nxml-scan-backward-within end))
			   ((eq xmltok-type 'end-tag)
			    ;; can't ever return nil here
			    (nxml-scan-element-backward end)
			    xmltok-start)
			   ((eq xmltok-type 'start-tag)
			    (error "Already at start of element"))
			   (t xmltok-start)))))
    (nxml-scan-error
     (goto-char (cadr err))
     (apply 'error (cddr err)))))

(defun nxml-scan-forward-within (end)
  (setq end (- end (nxml-end-delimiter-length xmltok-type)))
  (when (<= end (point))
    (error "Already at end of %s"
	   (nxml-token-type-friendly-name xmltok-type)))
  (cond ((memq xmltok-type '(start-tag
			     empty-element
			     partial-start-tag
			     partial-empty-element))
	 (if (< (point) xmltok-name-end)
	     xmltok-name-end
	   (let ((att (nxml-find-following-attribute)))
	     (cond ((not att) end)
		   ((and (xmltok-attribute-value-start att)
			 (<= (xmltok-attribute-value-start att)
			     (point)))
		    (nxml-scan-forward-in-attribute-value att))
		   ((xmltok-attribute-value-end att)
		    (1+ (xmltok-attribute-value-end att)))
		   ((save-excursion
		      (goto-char (xmltok-attribute-name-end att))
		      (looking-at "[ \t\r\n]*="))
		    (match-end 0))
		   (t (xmltok-attribute-name-end att))))))
	(t end)))

(defun nxml-scan-backward-within (end)
  (setq xmltok-start
	(+ xmltok-start
	   (nxml-start-delimiter-length xmltok-type)))
  (when (<= (point) xmltok-start)
    (error "Already at start of %s"
	   (nxml-token-type-friendly-name xmltok-type)))
  (cond ((memq xmltok-type '(start-tag
			     empty-element
			     partial-start-tag
			     partial-empty-element))
	 (let ((att (nxml-find-preceding-attribute)))
	   (cond ((not att) xmltok-start)
		 ((and (xmltok-attribute-value-start att)
		       (<= (xmltok-attribute-value-start att)
			   (point))
		       (<= (point)
			   (xmltok-attribute-value-end att)))
		  (nxml-scan-backward-in-attribute-value att))
		 (t (xmltok-attribute-name-start att)))))
	(t xmltok-start)))

(defun nxml-scan-forward-in-attribute-value (att)
  (when (= (point) (xmltok-attribute-value-end att))
    (error "Already at end of attribute value"))
  (let ((refs (xmltok-attribute-refs att))
	ref)
    (while refs
      (setq ref (car refs))
      (if (< (point) (aref ref 2))
	  (setq refs nil)
	(setq ref nil)
	(setq refs (cdr refs))))
    (cond ((not ref)
	   (xmltok-attribute-value-end att))
	  ((< (point) (aref ref 1))
	   (aref ref 1))
	  ((= (point) (aref ref 1))
	   (aref ref 2))
	  (t
	   (let ((end (- (aref ref 2)
			 (nxml-end-delimiter-length (aref ref 0)))))
	     (if (< (point) end)
		 end
	       (error "Already at end of %s"
		      (nxml-token-type-friendly-name (aref ref 0)))))))))

(defun nxml-scan-backward-in-attribute-value (att)
  (when (= (point) (xmltok-attribute-value-start att))
    (error "Already at start of attribute value"))
  (let ((refs (reverse (xmltok-attribute-refs att)))
	ref)
    (while refs
      (setq ref (car refs))
      (if (< (aref ref 1) (point))
	  (setq refs nil)
	(setq ref nil)
	(setq refs (cdr refs))))
    (cond ((not ref)
	   (xmltok-attribute-value-start att))
	  ((< (aref ref 2) (point))
	   (aref ref 2))
	  ((= (point) (aref ref 2))
	   (aref ref 1))
	  (t
	   (let ((start (+ (aref ref 1)
			   (nxml-start-delimiter-length (aref ref 0)))))
	     (if (< start (point))
		 start
	       (error "Already at start of %s"
		      (nxml-token-type-friendly-name (aref ref 0)))))))))

(defun nxml-find-following-attribute ()
  (let ((ret nil)
	(atts (or xmltok-attributes xmltok-namespace-attributes))
	(more-atts (and xmltok-attributes xmltok-namespace-attributes)))
    (while atts
      (let* ((att (car atts))
	     (name-start (xmltok-attribute-name-start att)))
	(cond ((and (<= name-start (point))
		    (xmltok-attribute-value-end att)
		    ;; <= because end is before quote
		    (<= (point) (xmltok-attribute-value-end att)))
	       (setq atts nil)
	       (setq ret att))
	      ((and (< (point) name-start)
		    (or (not ret)
			(< name-start
			   (xmltok-attribute-name-start ret))))
	       (setq ret att))))
      (setq atts (cdr atts))
      (unless atts
	(setq atts more-atts)
	(setq more-atts nil)))
    ret))

(defun nxml-find-preceding-attribute ()
  (let ((ret nil)
	(atts (or xmltok-attributes xmltok-namespace-attributes))
	(more-atts (and xmltok-attributes xmltok-namespace-attributes)))
    (while atts
      (let* ((att (car atts))
	     (name-start (xmltok-attribute-name-start att)))
	(cond ((and (< name-start (point))
		    (xmltok-attribute-value-end att)
		    ;; <= because end is before quote
		    (<= (point) (xmltok-attribute-value-end att)))
	       (setq atts nil)
	       (setq ret att))
	      ((and (< name-start (point))
		    (or (not ret)
			(< (xmltok-attribute-name-start ret)
			   name-start)))
	       (setq ret att))))
      (setq atts (cdr atts))
      (unless atts
	(setq atts more-atts)
	(setq more-atts nil)))
    ret))

(defun nxml-up-element (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-backward-up-element (- arg))
    (condition-case err
	(while (and (> arg 0)
		    (< (point) (point-max)))
	  (let ((token-end (nxml-token-after)))
	    (goto-char (cond ((or (memq xmltok-type '(end-tag
						      partial-end-tag))
				  (and (memq xmltok-type
					     '(empty-element
					       partial-empty-element))
				       (< xmltok-start (point))))
			      token-end)
			     ((nxml-scan-element-forward
			       (if (and (eq xmltok-type 'start-tag)
					(= (point) xmltok-start))
				   xmltok-start
				 token-end)
			       t))
			     (t (error "No parent element")))))
	  (setq arg (1- arg)))
      (nxml-scan-error
       (goto-char (cadr err))
       (apply 'error (cddr err))))))

(defun nxml-backward-up-element (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-up-element (- arg))
    (condition-case err
	(while (and (> arg 0)
		    (< (point-min) (point)))
	  (let ((token-end (nxml-token-before)))
	    (goto-char (cond ((or (memq xmltok-type '(start-tag
						      partial-start-tag
						      prolog))
				  (and (memq xmltok-type
					     '(empty-element
					       partial-empty-element))
				       (< (point) token-end)))
			      xmltok-start)
			     ((nxml-scan-element-backward
			       (if (and (eq xmltok-type 'end-tag)
					(= (point) token-end))
				   token-end
				 xmltok-start)
			       t)
			      xmltok-start)
			     (t (error "No parent element")))))
	  (setq arg (1- arg)))
      (nxml-scan-error
       (goto-char (cadr err))
       (apply 'error (cddr err))))))

(defun nxml-mark-token-after ()
  (interactive)
  (push-mark (nxml-token-after) nil t)
  (goto-char xmltok-start)
  (message "Marked %s" xmltok-type))

(defun nxml-scan-element-forward (from &optional up)
  "Scan forward from FROM over a single balanced element.
Point must between tokens.  Return the position of the end of the tag
that ends the element. `xmltok-start' will contain the position of the
start of the tag. If UP is non-nil, then scan past end-tag of element
containing point.  If no element is found, return nil.  If a
well-formedness error prevents scanning, signal an nxml-scan-error.
Point is not moved."
  (let ((open-tags (and up t))
	found)
    (save-excursion
      (goto-char from)
      (while (cond ((not (nxml-tokenize-forward))
		    (when (consp open-tags)
		      (nxml-scan-error (cadr open-tags)
				       "Start-tag has no end-tag"))
		    nil)
		   ((eq xmltok-type 'start-tag)
		    (setq open-tags
			  (cons (xmltok-start-tag-qname)
				(cons xmltok-start
				      open-tags)))
		    t)
		   ((eq xmltok-type 'end-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags)) (setq found (point)) nil)
			  ((not (string= (car open-tags)
					 (xmltok-end-tag-qname)))
			   (nxml-scan-error (+ 2 xmltok-start)
					    "Mismatched end-tag; \
expected `%s'"
					    (car open-tags)))
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found (point)) nil)))
		   ((memq xmltok-type '(empty-element
					partial-empty-element))
		    (if open-tags
			t
		      (setq found (point))
		      nil))
		   ((eq xmltok-type 'partial-end-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags)) (setq found (point)) nil)
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found (point)) nil)))
		   ((eq xmltok-type 'partial-start-tag)
		    (nxml-scan-error xmltok-start
				     "Missing `>'"))
		   (t t))))
    found))

(defun nxml-scan-element-backward (from &optional up)
  "Scan backward from FROM over a single balanced element.
Point must between tokens.  Return the position of the end of the tag
that starts the element. `xmltok-start' will contain the position of
the start of the tag.  If UP is non-nil, then scan past start-tag of
element containing point.  If no element is found, return nil.  If a
well-formedness error prevents scanning, signal an nxml-scan-error.
Point is not moved."
  (let ((open-tags (and up t))
	token-end found)
    (save-excursion
      (goto-char from)
      (while (cond ((or (< (point) nxml-prolog-end)
			(not (search-backward "<" nxml-prolog-end t)))
		    (when (consp open-tags)
		      (nxml-scan-error (cadr open-tags)
				       "End-tag has no start-tag"))
		    nil)
		   ((progn
		      (nxml-move-outside-backwards)
		      (save-excursion
			(nxml-tokenize-forward)
			(setq token-end (point)))
		      (eq xmltok-type 'end-tag))
		    (setq open-tags
			  (cons (xmltok-end-tag-qname)
				(cons xmltok-start open-tags)))
		    t)
		   ((eq xmltok-type 'start-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags))
			   (setq found token-end)
			   nil)
			  ((and (car open-tags)
				(not (string= (car open-tags)
					      (xmltok-start-tag-qname))))
			   (nxml-scan-error (1+ xmltok-start)
					    "Mismatched start-tag; \
expected `%s'"
					    (car open-tags)))
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found token-end) nil)))
		   ((memq xmltok-type '(empty-element
					partial-empty-element))
		    (if open-tags
			t
		      (setq found token-end)
		      nil))
		   ((eq xmltok-type 'partial-end-tag)
		    (setq open-tags
			  (cons nil (cons xmltok-start open-tags)))
		    t)
		   ((eq xmltok-type 'partial-start-tag)
		    ;; if we have only a partial-start-tag
		    ;; then it's unlikely that there's a matching
		    ;; end-tag, so it's probably not helpful
		    ;; to treat it as a complete start-tag
		    (nxml-scan-error xmltok-start
				     "Missing `>'"))
		   (t t))))
    found))

;;; Random access parsing

(defun nxml-token-after ()
  "Return the position after the token containing the char after point.
Sets up the variables `xmltok-type', `xmltok-start',
`xmltok-name-end', `xmltok-name-colon', `xmltok-attributes',
`xmltok-namespace-attributes' in the same was as does
`xmltok-forward'.  The prolog will be treated as a single token with
type `prolog'."
  (let ((pos (point)))
    (if (< pos nxml-prolog-end)
	(progn
	  (setq xmltok-type 'prolog
		xmltok-start (point-min))
	  (min nxml-prolog-end (point-max)))
      (nxml-ensure-scan-up-to-date)
      (let ((inside-type (nxml-get-inside pos)))
	(if inside-type
	    (progn
	      (setq xmltok-type inside-type
		    xmltok-start (max (1- (nxml-inside-start pos))
				      (point-min)))
	      (nxml-inside-end pos))
	  (save-excursion
	    (unless (or (eq (char-after) ?<)
			(search-backward "<"
					 (max (point-min) nxml-prolog-end)
					 t))
	      (goto-char (if (<= (point-min) nxml-prolog-end)
			     nxml-prolog-end
			   (or (nxml-inside-end (point-min))
			       (point-min)))))
	    (while (and (nxml-tokenize-forward)
			(<= (point) pos)))
	    (point)))))))

(defun nxml-token-before ()
  (if (/= (point-min) (point))
      (save-excursion
	(goto-char (1- (point)))
	(nxml-token-after))
    (setq xmltok-start (point))
    (setq xmltok-type nil)
    (point)))

(defun nxml-tokenize-forward ()
  (let (xmltok-dependent-regions
	xmltok-errors)
    (when (and (xmltok-forward)
	       (> (point) nxml-scan-end))
      (cond ((memq xmltok-type '(comment
				 cdata-section
				 processing-instruction))
	     (nxml-set-inside (1+ xmltok-start) (point) xmltok-type))
	    (xmltok-dependent-regions
	     (nxml-mark-parse-dependent-regions)))
      (set-marker nxml-scan-end (point)))
    xmltok-type))

(defun nxml-move-outside-backwards ()
  "Move point to first character of the containing special thing.
Leave point unmoved if it is not inside anything special."
  (let ((start (nxml-inside-start (point))))
    (when start
      (goto-char (1- start))
      (when (nxml-get-inside (point))
	(error "Char before inside-start at %s had nxml-inside property %s"
	       (point)
	       (nxml-get-inside (point)))))))

(defun nxml-ensure-scan-up-to-date ()
  (let ((pos (point)))
    (when (< nxml-scan-end pos)
      (save-excursion
	(goto-char nxml-scan-end)
	(let (xmltok-errors
	      xmltok-dependent-regions)
	  (while (when (xmltok-forward-special pos)
		   (when (memq xmltok-type
			       '(comment
				 processing-instruction
				 cdata-section))
		     (nxml-set-inside (1+ xmltok-start)
				      (point)
				      xmltok-type))
		   (if (< (point) pos)
		       t
		     (setq pos (point))
		     nil)))
	  (nxml-clear-dependent-regions nxml-scan-end pos)
	  (nxml-mark-parse-dependent-regions)
	  (set-marker nxml-scan-end pos))))))

(defun nxml-scan-error (&rest args)
  (signal 'nxml-scan-error args))

(put 'nxml-scan-error
     'error-conditions
     '(error nxml-error nxml-scan-error))

(put 'nxml-scan-error
     'error-message
     "Scan over element that is not well-formed")

(defun nxml-start-delimiter-length (type)
  (or (get type 'nxml-start-delimiter-length)
      0))
			
(put 'cdata-section 'nxml-start-delimiter-length 9)
(put 'comment 'nxml-start-delimiter-length 4)
(put 'processing-instruction 'nxml-start-delimiter-length 2)
(put 'start-tag 'nxml-start-delimiter-length 1)
(put 'empty-element 'nxml-start-delimiter-length 1)
(put 'partial-empty-element 'nxml-start-delimiter-length 1)
(put 'entity-ref 'nxml-start-delimiter-length 1)
(put 'char-ref 'nxml-start-delimiter-length 2)

(defun nxml-end-delimiter-length (type)
  (or (get type 'nxml-end-delimiter-length)
      0))
			
(put 'cdata-section 'nxml-end-delimiter-length 3)
(put 'comment 'nxml-end-delimiter-length 3)
(put 'processing-instruction 'nxml-end-delimiter-length 2)
(put 'start-tag 'nxml-end-delimiter-length 1)
(put 'empty-element 'nxml-end-delimiter-length 2)
(put 'partial-empty-element 'nxml-end-delimiter-length 1)
(put 'entity-ref 'nxml-end-delimiter-length 1)
(put 'char-ref 'nxml-end-delimiter-length 1)

(defun nxml-token-type-friendly-name (type)
  (or (get type 'nxml-friendly-name)
      (symbol-name type)))

(put 'cdata-section 'nxml-friendly-name "CDATA section")
(put 'processing-instruction 'nxml-friendly-name "processing instruction")
(put 'entity-ref 'nxml-friendly-name "entity reference")
(put 'char-ref 'nxml-friendly-name "character reference")

;;; nxml-mode.el ends here
