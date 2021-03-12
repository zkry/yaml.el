;;; yaml.el --- YAML parser for Elisp -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: https://github.com/zkry/yaml.el
;; Keywords: YAML


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; YAML parser in Elisp.

;;; Code:

(defconst yaml-tracing-ignore '("s-space"
                                "s-tab"
                                "s-white"
                                "l-comment"
                                "b-break"
                                "b-line-feed"
                                "b-carriage-return"
                                "s-b-comment"
                                "b-comment"
                                "l-comment"
                                "ns-char"
                                "nb-char"
                                "b-char"
                                "c-printable"
                                "b-as-space"))

(defvar yaml-parsing-input "ABC")
(defvar yaml-parsing-position 0)
(defvar yaml-states nil)

(defvar yaml--parsing-object-type nil)
(defvar yaml--parsing-sequence-type nil)
(defvar yaml--parsing-null-object nil)
(defvar yaml--parsing-false-object nil)

(cl-defstruct (yaml-state (:constructor yaml-state-create)
                          (:copier nil))
  doc tt m name lvl beg end)

(defmacro yaml-parse (data &rest forms)
  "Parse DATA according to FORMS."
  (declare (indent defun))
  `(progn (setq yaml-parsing-input ,data)
          (setq yaml-parsing-position 0)
          (yaml--initialize-state)
          ,@forms))


(defun yaml--state-curr ()
  "Return the current state."
  (or (car yaml-states)
      (yaml-state-create
       :name nil :doc nil :lvl 0 :beg 0 :end 0 :m nil :tt nil)))

(defun yaml-state-set-m (val)
  "Set the current value of m to VAL."
  (let* ((top-state (yaml--state-curr)))
    (setcar yaml-states
            (yaml-state-create :doc (yaml-state-doc top-state)
                               :tt (yaml-state-tt top-state)
                               :m val
                               :name (yaml-state-m top-state)
                               :lvl (yaml-state-lvl top-state)
                               :beg (yaml-state-beg top-state)
                               :end (yaml-state-end top-state)))))
(defun yaml-state-set-t (val)
  "Set the current value of t to VAL."
  (let* ((top-state (yaml--state-curr)))
    (setcar yaml-states
            (yaml-state-create :doc (yaml-state-doc top-state)
                               :tt val
                               :m (yaml-state-m top-state)
                               :name (yaml-state-m top-state)
                               :lvl (yaml-state-lvl top-state)
                               :beg (yaml-state-beg top-state)
                               :end (yaml-state-end top-state)))))

(defun yaml--state-doc ()
  "Return the doc property of current state."
  (yaml-state-doc (yaml--state-curr)))

(defun yaml--state-t ()
  "Return the doc property of current state."
  (yaml-state-tt (yaml--state-curr)))

(defun yaml--state-m ()
  "Return the doc property of current state."
  (yaml-state-m (yaml--state-curr)))

(defun yaml--state-end ()
  "Return the doc property of current state."
  (yaml-state-end (yaml--state-curr)))

(defun yaml--push-state (name)
  "Add a new state frame with NAME."
  (let* ((curr-state (yaml--state-curr))
         (new-state (yaml-state-create
                    :doc (yaml--state-doc)
                    :tt (yaml--state-t)
                    :m (yaml--state-m)
                    :name name
                    :lvl (1+ (yaml-state-lvl curr-state))
                    :beg yaml-parsing-position
                    :end nil)))
    (push new-state yaml-states)))

(defun yaml--pop-state ()
  "Pop the current state."
  (let ((popped-state (car yaml-states)))
   (setq yaml-states (cdr yaml-states))
   (let ((top-state (car yaml-states)))
     (setcar yaml-states
             (yaml-state-create :doc (yaml-state-doc top-state)
                                :tt (yaml-state-tt top-state)
                                :m (yaml-state-m top-state)
                                :name (yaml-state-m top-state)
                                :lvl (yaml-state-lvl top-state)
                                :beg (yaml-state-beg popped-state)
                                :end yaml-parsing-position)))))

(defun yaml--initialize-state ()
  "Initialize the yaml state for parsing."
  (setq yaml-states
        (list (yaml-state-create :doc nil
                                 :tt nil
                                 :m nil
                                 :name nil
                                 :lvl 0
                                 :beg nil
                                 :end nil))))

(defconst yaml--grammar-resolution-rules
  '(("ns-plain" . literal))
  "Alist determining how to resolve grammar rule.")

;; Receiver Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar yaml--document-start-version nil)
(defvar yaml--document-start-explicit nil)
(defvar yaml--document-end-explicit nil)
(defvar yaml--tag-map nil)
(defvar yaml--tag-handle nil)
(defvar yaml--anchor nil)
(defvar yaml--document-end nil)

(defvar yaml--cache nil)
(defvar yaml--object-stack nil)
(defvar yaml--state-stack nil
  "The state that the YAML parser is with regards to incoming events.")
(defvar yaml--root nil)

(defun yaml--resolve-scalar-tag (scalar)
  (cond
   ;; tag:yaml.org,2002:null
   ((or (equal "null" scalar)
        (equal "Null" scalar)
        (equal "NULL" scalar)
        (equal "~" scalar))
    yaml--parsing-null-object)
   ;; tag:yaml.org,2002:bool
   ((or (equal "true" scalar)
        (equal "True" scalar)
        (equal "TRUE" scalar)) t)
   ((or (equal "false" scalar)
        (equal "False" scalar)
        (equal "FALSE" scalar))
    yaml--parsing-false-object)
   ;; tag:yaml.org,2002:int
   ((string-match "^0$\\|^-?[1-9][0-9]*$" scalar)
    (string-to-number scalar))
   ((string-match "^[-+]?[0-9]+$" scalar)
    (string-to-number scalar))
   ((string-match "^0o[0-7]+$" scalar)
    (string-to-number scalar 8))
   ((string-match "^0x[0-9a-fA-F]+$" scalar)
    (string-to-number scalar 16))
   ;; tag:yaml.org,2002:float
   ((string-match "^[-+]?\\(\\.[0-9]+\\|[0-9]+\\(\\.[0-9]*\\)?\\)\\([eE][-+]?[0-9]+\\)?$" scalar)
    (string-to-number scalar 10))
   ((string-match "^[-+]?\\(\\.inf\\|\\.Inf\\|\\.INF\\)$" scalar)
    1.0e+INF)
   ((string-match "^[-+]?\\(\\.nan\\|\\.NaN\\|\\.NAN\\)$" scalar)
    1.0e+INF)
   ((string-match "^0$\\|^-?[1-9]\\(\\.[0-9]*\\)?\\(e[-+][1-9][0-9]*\\)?$" scalar)
    (string-to-number scalar))
   (t scalar)))

(defun yaml--hash-table-to-alist (hash-table)
  "Convert HASH-TABLE to a alist."
  (let ((alist nil))
    (maphash
     (lambda (k v)
       (setq alist (cons (cons k v) alist)))
     hash-table)
    alist))

(defun yaml--hash-table-to-plist (hash-table)
  "Convert HASH-TABLE to a plist."
  (let ((plist nil))
    (maphash
     (lambda (k v)
       (setq plist (cons k (cons v plist))))
     hash-table)
    plist))

(defun yaml--format-object (hash-table)
  "Convert HASH-TABLE to alist of plist if specified."
  (cond
   ((equal yaml--parsing-object-type 'hash-table)
    hash-table)
   ((equal yaml--parsing-object-type 'alist)
    (yaml--hash-table-to-alist hash-table))
   ((equal yaml--parsing-object-type 'plist)
    (yaml--hash-table-to-plist hash-table))
   (t hash-table)))

(defun yaml--format-list (l)
  "Convert HASH-TABLE to alist of plist if specified."
  (cond
   ((equal yaml--parsing-sequence-type 'list)
    l)
   ((equal yaml--parsing-sequence-type 'array)
    (apply #'vector l))
   (t l)))

(defun yaml--add-event (e)
  "Add event E."
  (message "Adding event: %s" e))

(defun yaml--stream-start-event ()
  "Create the data for a stream-start event."
  '(:stream-start))

(defun yaml--stream-end-event ()
  "Create the data for a stream-end event."
  '(:stream-end))

(defun yaml--document-start-event (explicit)
  '(:document-start))
(defun yaml--document-end-event (explicit)
  '(:document-end))

(defun yaml--mapping-start-event (flow)
  (push :mapping yaml--state-stack)
  (push (make-hash-table :test 'equal) yaml--object-stack)
  '(:mapping-start))

(defun yaml--mapping-end-event ()
  (pop yaml--state-stack)
  (let ((obj (pop yaml--object-stack)))
    ;; TODO: Perhaps separate the logic here.
    (yaml--scalar-event nil obj))
  '(:mapping-end))

(defun yaml--sequence-start-event (flow)
  (push :sequence yaml--state-stack)
  (push nil yaml--object-stack)
  '(:sequence-start))

(defun yaml--sequence-end-event ()
  (pop yaml--state-stack)
  (let ((obj (pop yaml--object-stack)))
    (yaml--scalar-event nil obj))
  '(:sequence-end))

(defun yaml--scalar-event (style value)
  (let ((top-state (car yaml--state-stack))
        (value (cond
                ((stringp value) (yaml--resolve-scalar-tag value))
                ((listp value) (yaml--format-list value))
                ((hash-table-p value) (yaml--format-object value)))))
    (cond
     ((not top-state)
      (setq yaml--root value))
     ((equal top-state :sequence)
      (let ((l (car yaml--object-stack)))
        (setcar yaml--object-stack (append l (list value)))))
     ((equal top-state :mapping)
      (progn
        (push :mapping-value yaml--state-stack)
        (push value yaml--cache)))
     ((equal top-state :mapping-value)
      (progn
        (let ((key (pop yaml--cache))
              (table (car yaml--object-stack)))
          (puthash key value table))
        (pop yaml--state-stack)))
     ((equal top-state nil))))
  '(:scalar))

(defun yaml--alias-event (name)
  '(:alias))

(defun yaml--check-document-start () t)
(defun yaml--check-document-end () t)

(defun yaml--revers-at-list ()
  (setcar yaml--object-stack (reverse (car yaml--object-stack))))

(defconst yaml--grammar-events-in
  '(("l-yaml-stream" . (lambda ()
                         (yaml--add-event (yaml--stream-start-event))
                         (setq yaml--document-start-version nil)
                         (setq yaml--document-start-explicit nil)
                         (setq yaml--tag-map (make-hash-table))
                         (yaml--add-event (yaml--document-start-event nil))))
    ("c-flow-mapping" . (lambda ()
                          (yaml--add-event (yaml--mapping-start-event t))))
    ("c-flow-sequence" . (lambda ()
                           (yaml--add-event (yaml--sequence-start-event nil))))
    ("l+block-mapping" . (lambda ()
                           (yaml--add-event (yaml--mapping-start-event nil))))
    ("l+block-sequence" . (lambda ()
                            (yaml--add-event (yaml--sequence-start-event nil))))
    ("ns-l-compact-mapping" . (lambda ()
                                (yaml--add-event (yaml--mapping-start-event nil))))
    ("ns-l-compact-sequence" . (lambda ()
                                 (yaml--add-event (yaml--sequence-start-event nil))))
    ("ns-flow-pair" . (lambda ()
                        (yaml--add-event (yaml--mapping-start-event t))))))

(defconst yaml--grammar-events-out
  '(("l-yaml-stream" . (lambda (text)
                         (yaml--check-document-end)
                         (yaml--add-event (yaml--stream-end-event))))
    ("ns-yaml-version" . (lambda (text)
                           (when yaml--document-start-version
                             (throw 'error "Multiple %YAML directives not allowed."))
                           (setq yaml--document-start-version text)))
    ("c-tag-handle" . (lambda (text)
                        (setq yaml--tag-handle text)))
    ("ns-tag-prefix" . (lambda (text)
                         (puthash yaml--tag-handle text yaml--tag-map)))
    ("c-directives-end" . (lambda (text)
                            (yaml--check-document-end)
                            (setq yaml--document-start-explicit t)))
    ("c-document-end" . (lambda (text)
                          (when (not yaml--document-end)
                            (setq yaml--document-end-explicit t))
                          (yaml--check-document-end)))
    ("c-flow-mapping" . (lambda (text)
                          (yaml--add-event (yaml--mapping-end-event))))
    ("c-flow-sequence" . (lambda (text)
                           (yaml--add-event (yaml--sequence-end-event ))))
    ("l+block-mapping" . (lambda (text)
                           (yaml--add-event (yaml--mapping-end-event))))
    ("l+block-sequence" . (lambda (text)
                            (yaml--revers-at-list)
                            (yaml--add-event (yaml--sequence-end-event))))
    ("ns-l-compact-mapping" . (lambda (text)
                                (yaml--add-event (yaml--mapping-end-event))))
    ("ns-l-compact-sequence" . (lambda (text)
                                 (yaml--add-event (yaml--sequence-end-event))))
    ("ns-flow-pair" . (lambda (text)
                        (yaml--add-event (yaml--mapping-end-event))))
    ("ns-plain" . (lambda (text)
                    (let* ((replaced (replace-regexp-in-string
                                      "\\(?:[ \t]*\r?\n[ \t]*\\)"
                                      "\n"
                                      text))
                           (replaced (replace-regexp-in-string
                                      "\\(\n\\)\\(\n*\\)"
                                      (lambda (x)
                                        (if (> (length x) 1)
                                            (substring x 1)
                                          " "))
                                      replaced)))
                      (yaml--add-event (yaml--scalar-event "plain" replaced)))))
    ("c-single-quoted" . (lambda (text)
                           (let* ((replaced (replace-regexp-in-string
                                             "\\(?:[ \t]*\r?\n[ \t]*\\)"
                                             "\n"
                                             text))
                                  (replaced (replace-regexp-in-string
                                             "\\(\n\\)\\(\n*\\)"
                                             (lambda (x)
                                               (if (> (length x) 1)
                                                   (substring x 1)
                                                 " "))
                                             replaced))
                                  (replaced (replace-regexp-in-string
                                             "''"
                                             (lambda (x)
                                               (if (> (length x) 1)
                                                   (substring x 1)
                                                 "'"))
                                            replaced)))
                             (yaml--add-event (yaml--scalar-event "single" replaced)))))
    ("c-double-quoted" . (lambda (text)
                           (let* ((replaced (replace-regexp-in-string
                                             "\\(?:[ \t]*\r?\n[ \t]*\\)"
                                             "\n"
                                             text))
                                  (replaced (replace-regexp-in-string
                                             "\\(\n\\)\\(\n*\\)"
                                             (lambda (x)
                                               (if (> (length x) 1)
                                                   (substring x 1)
                                                 " "))
                                             replaced))
                                  (replaced (replace-regexp-in-string "\\\\\\([\"\\/]\\)" "\\1" replaced))
                                  (replaced (replace-regexp-in-string "\\\\ " " " replaced))
                                  (replaced (replace-regexp-in-string "\\\\ " " " replaced))
                                  (replaced (replace-regexp-in-string "\\\\b" "\b" replaced))
                                  (replaced (replace-regexp-in-string "\\\\t" "\t" replaced))
                                  (replaced (replace-regexp-in-string "\\\\n" "\n" replaced))
                                  (replaced (replace-regexp-in-string "\\\\r" "\r" replaced))
                                  (replaced (replace-regexp-in-string "\\\\r" "\r" replaced))
                                  (replaced (replace-regexp-in-string
                                             "\\\\x\\([0-9a-fA-F]\\{2\\}\\)"
                                             (lambda (x)
                                               (let ((char-pt (substring 2 x)))
                                                 (string (string-to-number char-pt 16))))
                                             replaced))
                                  (replaced (replace-regexp-in-string
                                             "\\\\x\\([0-9a-fA-F]\\{2\\}\\)"
                                             (lambda (x)
                                               (let ((char-pt (substring x 2)))
                                                 (string (string-to-number char-pt 16))))
                                             replaced))
                                  (replaced (replace-regexp-in-string
                                             "\\\\x\\([0-9a-fA-F]\\{4\\}\\)"
                                             (lambda (x)
                                               (let ((char-pt (substring x 2)))
                                                 (string (string-to-number char-pt 16))))
                                             replaced))
                                  (replaced (replace-regexp-in-string
                                             "\\\\x\\([0-9a-fA-F]\\{8\\}\\)"
                                             (lambda (x)
                                               (let ((char-pt (substring x 2)))
                                                 (string (string-to-number char-pt 16))))
                                             replaced))
                                  (replaced (replace-regexp-in-string
                                             "\\\\\\\\"
                                             "\\"
                                             replaced))
                                  (replaced (substring replaced 1 (1- (length replaced)))))
                             (yaml--add-event (yaml--scalar-event "double" replaced)))))
    ("c-l+literal" . (lambda (text)
                       ;; TODO
                       (yaml--add-event (yaml--scalar-event "literal" text))))
    ("c-l+folded" . (lambda (text)
                      (yaml--add-event (yaml--scalar-event "folded" text))))
    ("e-scalar" . (lambda (text)
                    (yaml--add-event (yaml--scalar-event "plain" ""))))
    ("c-ns-anchor-property" . (lambda (text)
                                (setq yaml--anchor (substring text 1))))
    ("c-ns-tag-property" . (lambda (text)
                             ;; (error "not implemented: %s" text)
                             ))
    ("c-ns-alias-node" . (lambda (text)
                           (yaml--add-event (yaml--alias-event (substring text 1)))))))

(defun yaml--walk-events (tree)
  "Event walker iterates over the parse TREE and signals events based off of the parsed rules."
  ;;(message ">>> %s" tree)
  (when (consp tree)
    (if (stringp (car tree))
        (let ((grammar-rule (car tree))
              (text (cadr tree))
              (children (caddr tree)))
          (let ((in-fn (cdr (assoc grammar-rule yaml--grammar-events-in)))
                (out-fn (cdr (assoc grammar-rule yaml--grammar-events-out))))
            (when in-fn
              (funcall in-fn))
            (yaml--walk-events children)
            (when out-fn
              (funcall out-fn text))))
      (yaml--walk-events (car tree))
      (yaml--walk-events (cdr tree)))))


(defmacro yaml--frame (name rule)
  "Add a new state frame of NAME for RULE."
  (declare (indent defun))
  (let ((res-symbol (make-symbol "res")))
    `(progn
       ;;(message "%2d: %s" (length yaml-states) ,name)
       (yaml--push-state ,name)
       (when (not (member ,name yaml-tracing-ignore))
         (message "|%s>%s %40s \"%s\""
                  (make-string (length yaml-states) ?-)
                  (make-string (- 40 (length yaml-states)) ?\s)
                  ,name
                  (replace-regexp-in-string
                   "\n"
                   "\\n"
                   (substring yaml-parsing-input yaml-parsing-position)
                   nil
                   'literal)))
       (let ((beg yaml-parsing-position)
             (,res-symbol ,rule))
         (when (and ,res-symbol (not (member ,name yaml-tracing-ignore)))
           (message "<%s|%s %40s \"%s\" = %s"
                    (make-string (length yaml-states) ?-)
                    (make-string (- 40 (length yaml-states)) ?\s)
                    ,name
                    (replace-regexp-in-string
                     "\n"
                     "\\n"
                     (substring yaml-parsing-input beg yaml-parsing-position)
                     nil
                     'literal)
                    ,res-symbol))
         (yaml--pop-state)
         (if (not ,res-symbol)
             nil
           (let ((res-type (cdr (assoc ,name yaml--grammar-resolution-rules))))
             (cond
              ((or (assoc ,name yaml--grammar-events-in)
                   (assoc ,name yaml--grammar-events-out))
               (list ,name (substring yaml-parsing-input beg yaml-parsing-position) ,res-symbol))
              ((equal res-type 'list) (list ,name ,res-symbol))
              ((equal res-type 'literal) (substring yaml-parsing-input beg yaml-parsing-position))
              (t ,res-symbol))))))))

(defun yaml--end-of-stream ()
  ""
  (>= yaml-parsing-position (length yaml-parsing-input)))

(defun yaml--char-at-pos (pos)
  "Return the character at POS."
  (aref yaml-parsing-input pos))

(defun yaml--slice (pos)
  "Return the character at POS."
  (substring yaml-parsing-input pos))

(defun yaml--at-char ()
  "Return the current character."
  (yaml--char-at-pos yaml-parsing-position))

(defun yaml--char-match (at &rest chars)
  "Return non-nil if AT match any of CHARS."
  (if (not chars)
      nil
    (or (equal at (car chars))
        (apply #'yaml--char-match (cons at (cdr chars))))))

(defun yaml--chr (c)
  "Try to match the character C."
  (if (or (yaml--end-of-stream) (not (equal (yaml--at-char) c)))
      nil
    (setq yaml-parsing-position (1+ yaml-parsing-position))
    t))

(defun yaml--chr-range (min max)
  "Return non-nil if the current character is between MIN and MAX."
  (if (or (yaml--end-of-stream) (not (<= min (yaml--at-char) max)))
      nil
    (setq yaml-parsing-position (1+ yaml-parsing-position))
    t))

(defun yaml--run-all (&rest funcs)
  "Return list of all evaluated FUNCS if all of FUNCS pass."
  (let* ((start-pos yaml-parsing-position)
         (ress '())
         (res (catch 'break
                (while funcs
                  (let ((res (funcall (car funcs))))
                    (when (not res)
                      (throw 'break nil))
                    (setq ress (append ress (list res)))
                    (setq funcs (cdr funcs))))
                ress)))
    (unless res
      (setq yaml-parsing-position start-pos))
    res))

(defmacro yaml--all (&rest forms)
  "Pass and return all forms if all of FORMS pass."
  `(yaml--run-all
    ,@(mapcar (lambda (form)
                `(lambda () ,form))
             forms)))

(defmacro yaml--any (&rest forms)
  "Pass if any of FORMS pass."
  (if (= 1 (length forms))
      (car forms)
    (let ((idx-sym (make-symbol "idx"))
          (start-pos-sym (make-symbol "start")))
      `(let ((,start-pos-sym yaml-parsing-position)
             (,idx-sym ,(car forms)))
         (or ,idx-sym
             (progn (setq yaml-parsing-position ,start-pos-sym)
                    (yaml--any ,@(cdr forms))))))))

(defmacro yaml--may (action)
  action)

(defmacro yaml--exclude (_)
  "Return non-nil."
  't)

(defmacro yaml--max (_)
  "Return non-nil."
  t)

(defun yaml--empty ()
  "Return non-nil."
  'empty)

(defun yaml--sub (a b)
  "Return A minus B."
  (- a b))

(defun yaml--match ()
  ""
  (let* ((states yaml-states)
         (res nil))
    (while (and states (not res))
      (let ((top-state (car states)))
        (if (yaml-state-end top-state)
            (let ((beg (yaml-state-beg top-state))
                  (end (yaml-state-end top-state)))
              (setq res (substring yaml-parsing-input beg end)))
          (setq states (cdr states)))))))

(defun yaml--auto-detect-indent (n)
  "Detect the indentation given N."
  (let* ((pos yaml-parsing-position)
         (in-seq (and (> pos 0)
                      (yaml--char-match (yaml--char-at-pos (1- pos)) ?\- ?\? ?\:)))
         (slice (yaml--slice pos))
         (_ (string-match
             "^\\(\\(?: *\\(?:#.*\\)?\n\\)*\\)\\( *\\)"
             slice))
         (pre (match-string 1 slice))
         (m (length (match-string 2 slice))))
    (if (and in-seq (not pre))
        (when (= n -1)
          (setq m (1+ m)))
      (setq m (- m n)))
    (when (< m 0)
      (setq m 0))
    m))

(defun yaml--the-end ()
  "Return non-nil if at the end of input (?)."
  (or (>= yaml-parsing-position (length yaml-parsing-input))
      (and (yaml--state-doc)
           (yaml--start-of-line)
           ;; TODO Test this Regex
           (string-match "\\^g(?:---|\\.\\.\\.\\)\\([[:blank:]]\\|$\\)" (substring yaml-parsing-input yaml-parsing-position)))))

(defun yaml--ord (f)
  (let ((res (funcall f)))
    (- (aref res 0) 48)))

(defun yaml--but (&rest fs)
  "Match the first FS but none of the others."
  (if (yaml--the-end)
      nil
    (let ((pos1 yaml-parsing-position))
      (if (not (funcall (car fs)))
          nil
        (let ((pos2 yaml-parsing-position))
          (setq yaml-parsing-position pos1)
          (if (equal ':error (catch 'break
                               (dolist (f (cdr fs))
                                 (if (funcall f)
                                     (progn
                                       (setq yaml-parsing-position pos1)
                                       (throw 'break ':error))))))
              nil
            (setq yaml-parsing-position pos2)
            t))))))

(defun yaml--rep (min max func)
  "Repeat FUNC between MIN and MAX times."
  (yaml--rep2 min max func))

(defun yaml--rep2 (min max func)
  "Repeat FUNC between MIN and MAX times."
  (if (and max (< max 0))
      nil
    (let* ((res-list '())
           (count 0)
           (pos yaml-parsing-position)
           (pos-start pos))
      (catch 'break
        (while (or (not max) (< count max))
          (let ((res (funcall func)))
            (when (or (not res)
                      (= yaml-parsing-position pos))
              (throw 'break nil))
            (setq res-list (cons res res-list))
            (setq count (1+ count))
            (setq pos yaml-parsing-position))))
      (if (and (>= count min)
               (or (not max) (<= count max)))
          (progn
            (setq yaml-parsing-position pos)
            (if (zerop count)
                t
              res-list))
        (setq yaml-parsing-position pos-start)
        nil))))

(defun yaml--start-of-line ()
  "Return non-nil if start of line."
  (or (= yaml-parsing-position 0)
      (>= yaml-parsing-position (length yaml-parsing-input))
      (equal (yaml--char-at-pos (1- yaml-parsing-position)) ?\n)))

(defun yaml--top ()
  "Perform top level YAML parsing rule."
  (yaml-l-yaml-stream))

(defmacro yaml--set (variable value)
  "Set the current state of VARIABLE to VALUE"
  (let ((res-sym (make-symbol "res")))
    `(let ((,res-sym ,value))
       (when ,res-sym
         (,(cond ((equal "m" (symbol-name variable)) 'yaml-state-set-m)
                 ((equal "t" (symbol-name variable)) 'yaml-state-set-t))
          ,res-sym)
         ,res-sym))))

(defmacro yaml--chk (type expr)
  (let ((start-symbol (make-symbol "start"))
        (ok-symbol (make-symbol "ok")))
    `(let ((,start-symbol yaml-parsing-position))
       (when (equal ,type "<=")
         (setq yaml-parsing-position (1- yaml-parsing-position)))
       (let ((ok (and (>= yaml-parsing-position 0) ,expr)))
         (setq yaml-parsing-position ,start-symbol)
         (if (equal ,type "!")
             (not ok)
           ok)))))

(defun yaml-parse-string (str)
  "Parse STR as YAML."
  (setq yaml--cache nil)
  (setq yaml--object-stack nil)
  (setq yaml--state-stack nil)
  (setq yaml--root nil)
  (let ((res (yaml-parse str
               (yaml--top))))
    (when (< yaml-parsing-position (length yaml-parsing-input))
      (error (format "parser finished before end of input %s/%s"
                     yaml-parsing-position
                     (length yaml-parsing-input))))
    (message "Parsed data: %s" res)
    (yaml--walk-events res)
    yaml--root))

(defun yaml-parse-string (string &rest args)
  "Parse the YAML value in STRING.  Keyword ARGS are as follows:

OBJECT-TYPE specifies the Lisp object to use for representing
key-value YAML mappings.  Possible values for OBJECT-TYPE are
the symbols hash-table, alist, and plist.

SEQUENCE-TYPE specifies the Lisp object to use for representing YAML
sequences.   Possible values for SEQUENCE-TYPE are the symbols list, and array.

NULL-OBJECT contains the object used to represent the null value.
It defaults to the symbol :null.

FALSE-OBJECT contains the object used to represent the false
value.  It defaults to the symbol :false."
  (setq yaml--cache nil)
  (setq yaml--object-stack nil)
  (setq yaml--state-stack nil)
  (setq yaml--root nil)
  (let ((object-type (plist-get args :object-type))
        (sequence-type (plist-get args :sequence-type))
        (null-object (plist-get args :null-object))
        (false-object (plist-get args :false-object)))
    (cond
     ((or (not object-type)
          (equal object-type 'hash-table))
      (setq yaml--parsing-object-type 'hash-table))
     ((equal 'alist object-type)
      (setq yaml--parsing-object-type 'alist))
     ((equal 'plist object-type)
      (setq yaml--parsing-object-type 'plist))
     (t (error "Invalid object-type.  object-type must be hash-table, alist, or plist")))
    (cond
     ((or (not sequence-type)
          (equal sequence-type 'array))
      (setq yaml--parsing-sequence-type 'array))
     ((equal 'list sequence-type)
      (setq yaml--parsing-sequence-type 'list))
     (t (error "Invalid sequence-type.  sequence-type must be list or array")))
    (setq yaml--parsing-null-object (or null-object :null))
    (setq yaml--parsing-false-object (or false-object :false))
    (let ((res (yaml-parse string
                 (yaml--top))))
      (when (< yaml-parsing-position (length yaml-parsing-input))
        (error (format "parser finished before end of input %s/%s"
                       yaml-parsing-position
                       (length yaml-parsing-input))))
      (message "Parsed data: %s" res)
      (yaml--walk-events res)
      yaml--root)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun yaml-c-flow-sequence (n c)
  "Documentation string."
  (yaml--frame "c-flow-sequence"
               (yaml--all (yaml--chr ?\[)
                          (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
                          (yaml--rep 0 1 (lambda () (yaml-ns-s-flow-seq-entries n (yaml-in-flow c))))
                          (yaml--chr ?\]))))

(defun yaml-c-indentation-indicator (m)
  "Documentation string."
  (yaml--frame "c-indentation-indicator"
    (yaml--any
     (yaml--set m (yaml--ord (lambda () (yaml--match))))
     (yaml--set m (yaml-auto-detect)))))

(defun yaml-ns-reserved-directive ()
  "Documentation string."
  (yaml--frame "ns-reserved-directive"
               (yaml--all (yaml-ns-directive-name)
                          (yaml--rep2 0 nil (lambda () (yaml--all (yaml-s-separate-in-line) (yaml-ns-directive-parameter)))))))

(defun yaml-ns-flow-map-implicit-entry (n c)
  "Documentation string."
  (yaml--frame "ns-flow-map-implicit-entry"
               (yaml--any (yaml-ns-flow-map-yaml-key-entry n c)
                          (yaml-c-ns-flow-map-empty-key-entry n c)
                          (yaml-c-ns-flow-map-json-key-entry n c))))

(defun yaml-ns-esc-double-quote ()
  "Documentation string."
  (yaml--frame "ns-esc-double-quote"
               (yaml--chr ?\")))

(defun yaml-c-mapping-start ()
  "Documentation string."
  (yaml--frame "c-mapping-start"
               (yaml--chr ?\{)))

(defun yaml-ns-flow-seq-entry (n c)
  "Documentation string."
  (yaml--frame "ns-flow-seq-entry"
               (yaml--any (yaml-ns-flow-pair n c)
                          (yaml-ns-flow-node n c))))

(defun yaml-l-empty (n c)
  "Documentation string."
  (yaml--frame "l-empty"
               (yaml--all (yaml--any (yaml-s-line-prefix n c)
                                     (yaml-s-indent-lt n))
                          (yaml-b-as-line-feed))))

(defun yaml-c-primary-tag-handle ()
  "Documentation string."
  (yaml--frame "c-primary-tag-handle"
               (yaml--chr ?\!)))

(defun yaml-ns-plain-safe-out ()
  "Documentation string."
  (yaml--frame "ns-plain-safe-out"
               (yaml-ns-char)))

(defun yaml-c-ns-shorthand-tag ()
  "Documentation string."
  (yaml--frame "c-ns-shorthand-tag"
               (yaml--all (yaml-c-tag-handle)
                          (yaml--rep 1 nil (lambda () (yaml-ns-tag-char))))))

(defun yaml-nb-ns-single-in-line ()
  "Documentation string."
  (yaml--frame "nb-ns-single-in-line"
               (yaml--rep2 0 nil (lambda () (yaml--all (yaml--rep2 0 nil (lambda () (yaml-s-white))) (yaml-ns-single-char))))))

(defun yaml-l-strip-empty (n)
  "Documentation string."
  (yaml--frame "l-strip-empty"
               (yaml--all (yaml--rep2 0 nil (lambda () (yaml--all (yaml-s-indent-le n)
                                                                  (yaml-b-non-content))))
                          (yaml--rep 0 1 (lambda () (yaml-l-trail-comments n))))))

(defun yaml-c-indicator ()
  "Documentation string."
  (yaml--frame "c-indicator"
               (yaml--any (yaml--chr ?\-)
                          (yaml--chr ?\?)
                          (yaml--chr ?\:)
                          (yaml--chr ?\,)
                          (yaml--chr ?\[)
                          (yaml--chr ?\])
                          (yaml--chr ?\{)
                          (yaml--chr ?\})
                          (yaml--chr ?\#)
                          (yaml--chr ?\&)
                          (yaml--chr ?\*)
                          (yaml--chr ?\!)
                          (yaml--chr ?\|)
                          (yaml--chr ?\>)
                          (yaml--chr ?\')
                          (yaml--chr ?\")
                          (yaml--chr ?\%)
                          (yaml--chr ?\@)
                          (yaml--chr ?\`))))

(defun yaml-c-l+literal (n)
  "Documentation string."
  (yaml--frame "c-l+literal" (yaml--all (yaml--chr ?\|)
                                        (yaml-c-b-block-header (yaml--state-m) (yaml--state-t))
                                        (yaml-l-literal-content (+ n (yaml--state-m)) (yaml--state-t)))))

(defun yaml-c-single-quoted (n c)
  "Documentation string."
  (yaml--frame "c-single-quoted" (yaml--all (yaml--chr ?\')
                                            (yaml-nb-single-text n c)
                                            (yaml--chr ?\'))))

(defun yaml-c-forbidden ()
  "Documentation string."
  (yaml--frame "c-forbidden" (yaml--all (yaml--start-of-line)
                                        (yaml--any (yaml-c-directives-end)
                                                   (yaml-c-document-end))
                                        (yaml--any (yaml-b-char)
                                                   (yaml-s-white)
                                                   (yaml--end-of-stream)))))

(defun yaml-c-ns-alias-node ()
  "Documentation string."
  (yaml--frame "c-ns-alias-node"
               (yaml--all (yaml--chr ?\*)
                          (yaml-ns-anchor-name))))

(defun yaml-c-secondary-tag-handle ()
  "Documentation string."
  (yaml--frame "c-secondary-tag-handle"
               (yaml--all (yaml--chr ?\!)
                          (yaml--chr ?\!))))

(defun yaml-ns-esc-next-line ()
  "Documentation string."
  (yaml--frame "ns-esc-next-line"
               (yaml--chr ?N)))

(defun yaml-l-nb-same-lines (n)
  "Documentation string."
  (yaml--frame "l-nb-same-lines"
               (yaml--all (yaml--rep2 0 nil (lambda () (yaml-l-empty n "block-in")))
                          (yaml--any (yaml-l-nb-folded-lines n)
                                     (yaml-l-nb-spaced-lines n)))))

(defun yaml-c-alias ()
  "Documentation string."
  (yaml--frame "c-alias" (yaml--chr ?\*)))

(defun yaml-ns-single-char ()
  "Documentation string."
  (yaml--frame "ns-single-char"
               (yaml--but (lambda () (yaml-nb-single-char))
                          (lambda () (yaml-s-white)))))

(defun yaml-c-l-block-map-implicit-value (n)
  "Documentation string."
  (yaml--frame "c-l-block-map-implicit-value"
               (yaml--all (yaml--chr ?\:)
                          (yaml--any (yaml-s-l+block-node n "block-out")
                                     (yaml--all (yaml-e-node) (yaml-s-l-comments))))))

(defun yaml-ns-uri-char ()
  "Documentation string."
  (yaml--frame "ns-uri-char"
               (yaml--any (yaml--all (yaml--chr ?\%) (yaml-ns-hex-digit) (yaml-ns-hex-digit))
                          (yaml-ns-word-char)
                          (yaml--chr ?\#)
                          (yaml--chr ?\;)
                          (yaml--chr ?\/)
                          (yaml--chr ?\?)
                          (yaml--chr ?\:)
                          (yaml--chr ?\@)
                          (yaml--chr ?\&)
                          (yaml--chr ?\=)
                          (yaml--chr ?\+)
                          (yaml--chr ?\$)
                          (yaml--chr ?\,)
                          (yaml--chr ?\_)
                          (yaml--chr ?\.)
                          (yaml--chr ?\!)
                          (yaml--chr ?\~)
                          (yaml--chr ?\*)
                          (yaml--chr ?\')
                          (yaml--chr ?\()
                          (yaml--chr ?\))
                          (yaml--chr ?\[)
                          (yaml--chr ?\]))))

(defun yaml-ns-esc-16-bit ()
  "Documentation string."
  (yaml--frame "ns-esc-16-bit"
    (yaml--all (yaml--chr ?u)
               (yaml--rep 4 4 #'yaml-ns-hex-digit))))

(defun yaml-l-nb-spaced-lines (n)
  "Documentation string."
  (yaml--frame "l-nb-spaced-lines"
               (yaml--all (yaml-s-nb-spaced-text n)
                          (yaml--rep2 0 nil (lambda () (yaml--all (yaml-b-l-spaced n)
                                                                  (yaml-s-nb-spaced-text n)))))))

(defun yaml-ns-plain (n c)
  "Documentation string."
  (yaml--frame "ns-plain"
               (cond ((equal c "block-key") (yaml-ns-plain-one-line c))
                     ((equal c "flow-in") (yaml-ns-plain-multi-line n c))
                     ((equal c "flow-key") (yaml-ns-plain-one-line c))
                     ((equal c "flow-out") (yaml-ns-plain-multi-line n c)))))

(defun yaml-c-printable ()
  "Documentation string."
  (yaml--frame "c-printable"
               (yaml--any (yaml--chr ?\x09)
                          (yaml--chr ?\x0A)
                          (yaml--chr ?\x0D)
                          (yaml--chr-range ?\x20 ?\x7E)
                          (yaml--chr ?\x85)
                          (yaml--chr-range ?\xA0 ?\xD7FF)
                          (yaml--chr-range ?\xE000 ?\xFFFD)
                          (yaml--chr-range ?\x010000 ?\x10FFFF))))

(defun yaml-c-mapping-value ()
  "Documentation string."
  (yaml--frame "c-mapping-value" (yaml--chr ?\:)))

(defun yaml-l-nb-literal-text (n)
  "Documentation string."
  (yaml--frame "l-nb-literal-text"
               (yaml--all (yaml--rep2 0 nil (lambda () (yaml-l-empty n "block-in")))
                          (yaml-s-indent n)
                          (yaml--rep 1 nil (lambda () (yaml-nb-char))))))

(defun yaml-ns-plain-char (c)
  "Documentation string."
  (yaml--frame "ns-plain-char"
    (yaml--any (yaml--but (lambda () (yaml-ns-plain-safe c))
                          (lambda () (yaml--chr ?\:))
                          (lambda () (yaml--chr ?\#)))
               (yaml--all (yaml--chk "<=" (yaml-ns-char))
                          (yaml--chr ?\#))
               (yaml--all (yaml--chr ?\:)
                          (yaml--chk "=" (yaml-ns-plain-safe c))))))

(defun yaml-ns-anchor-char ()
  "Documentation string."
  (yaml--frame "ns-anchor-char"
               (yaml--but (lambda () (yaml-ns-char))
                          (lambda () (yaml-c-flow-indicator)))))

(defun yaml-s-l+block-scalar (n c)
  "Documentation string."
  (yaml--frame "s-l+block-scalar"
               (yaml--all (yaml-s-separate (+ n 1) c)
                          (yaml--rep 0 1 (lambda () (yaml--all (yaml-c-ns-properties (+ n 1) c)
                                                               (yaml-s-separate (+ n 1) c))))
                          (yaml--any (yaml-c-l+literal n)
                                     (yaml-c-l+folded n)))))

(defun yaml-ns-plain-safe-in ()
  "Documentation string."
  (yaml--frame "ns-plain-safe-in"
               (yaml--but (lambda () (yaml-ns-char))
                          (lambda () (yaml-c-flow-indicator)))))

(defun yaml-nb-single-text (n c)
  "Documentation string."
  (yaml--frame "nb-single-text"
               (cond ((equal c "block-key") (yaml-nb-single-one-line))
                     ((equal c "flow-in") (yaml-nb-single-multi-line n))
                     ((equal c "flow-key") (yaml-nb-single-one-line))
                     ((equal c "flow-out") (yaml-nb-single-multi-line n)))))

(defun yaml-s-indent-le (n)
  "Documentation string."
  (yaml--frame "s-indent-le"
               (yaml--may (yaml--all (yaml--rep2 0 nil (lambda () (yaml-s-space)))
                                     (<= (length (yaml--match)) n)))))

(defun yaml-ns-esc-carriage-return ()
  "Documentation string."
  (yaml--frame "ns-esc-carriage-return" (yaml--chr ?\r)))

(defun yaml-l-chomped-empty (n t)
  "Documentation string."
  (yaml--frame "l-chomped-empty"
               (cond ((equal t "clip") (yaml-l-strip-empty n))
                     ((equal t "keep") (yaml-l-keep-empty n))
                     ((equal t "strip") (yaml-l-strip-empty n)))))

(defun yaml-c-s-implicit-json-key (c)
  "Documentation string."
  (yaml--frame "c-s-implicit-json-key"
               (yaml--all (yaml--max 1024)
                          (yaml-c-flow-json-node nil c)
                          (yaml--rep 0 1 (lambda () (yaml-s-separate-in-line))))))

(defun yaml-b-as-space ()
  "Documentation string."
  (yaml--frame "b-as-space" (yaml-b-break)))

(defun yaml-ns-s-flow-seq-entries (n c)
  "Documentation string."
  (yaml--frame "ns-s-flow-seq-entries"
               (yaml--all (yaml-ns-flow-seq-entry n c)
                          (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
                          (yaml--rep 0 1 (lambda () (yaml--all (yaml--chr ?\,)
                                                               (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
                                                               (yaml--rep 0 1 (lambda () (yaml-ns-s-flow-seq-entries n c)))))))))

(defun yaml-l-block-map-explicit-value (n)
  "Documentation string."
  (yaml--frame "l-block-map-explicit-value"
               (yaml--all (yaml-s-indent n)
                          (yaml--chr ?\:)
                          (yaml-s-l+block-indented n "block-out"))))

(defun yaml-c-ns-flow-map-json-key-entry (n c)
  "Documentation string."
  (yaml--frame "c-ns-flow-map-json-key-entry"
               (yaml--all (yaml-c-flow-json-node n c)
                          (yaml--any (yaml--all (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
                                                (yaml-c-ns-flow-map-adjacent-value n c))
                                     (yaml-e-node)))))

(defun yaml-c-sequence-entry ()
  "Documentation string."
  (yaml--frame "c-sequence-entry"
               (yaml--chr ?\-)))

(defun yaml-l-bare-document ()
  "Documentation string."
  (yaml--frame "l-bare-document"
               (yaml--all (yaml--exclude "c-forbidden")
                          (yaml-s-l+block-node -1 "block-in"))))

(defun yaml-b-chomped-last (t)
  "Documentation string."
  (yaml--frame "b-chomped-last"
    (cond ((equal t "clip") (yaml--any (yaml-b-as-line-feed) (yaml--end-of-stream)))
          ((equal t "keep") (yaml--any (yaml-b-as-line-feed) (yaml--end-of-stream)))
          ((equal t "strip") (yaml--any (yaml-b-non-content) (yaml--end-of-stream))))))

(defun yaml-b-chomped-last (t)
  "Documentation string."
  (yaml--frame "b-chomped-last"
    (cond ((equal t "clip") (yaml--any (yaml-b-as-line-feed) (yaml--end-of-stream)))
          ((equal t "keep") (yaml--any (yaml-b-as-line-feed) (yaml--end-of-stream)))
          ((equal t "strip") (yaml--any (yaml-b-non-content) (yaml--end-of-stream))))))

(defun yaml-l-trail-comments (n)
  "Documentation string."
  (yaml--frame "l-trail-comments"
               (yaml--all (yaml-s-indent-lt n)
                          (yaml-c-nb-comment-text)
                          (yaml-b-comment)
                          (yaml--rep2 0 nil (lambda () (yaml-l-comment))))))

(defun yaml-ns-flow-map-yaml-key-entry (n c)
  "Documentation string."
  (yaml--frame "ns-flow-map-yaml-key-entry"
               (yaml--all (yaml-ns-flow-yaml-node n c)
                          (yaml--any (yaml--all (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
                                                (yaml-c-ns-flow-map-separate-value n c))
                                     (yaml-e-node)))))

(defun yaml-s-indent (n)
  "Documentation string."
  (yaml--frame "s-indent" (yaml--rep n n #'yaml-s-space)))

(defun yaml-ns-esc-line-separator ()
  "Documentation string."
  (yaml--frame "ns-esc-line-separator" (yaml--chr ?\L)))

(defun yaml-ns-flow-yaml-content (n c)
  "Documentation string."
  (yaml--frame "ns-flow-yaml-content"
    (yaml-ns-plain n c)))

(defun yaml-ns-flow-yaml-node (n c)
  "Documentation string."
  (yaml--frame "ns-flow-yaml-node"
               (yaml--any (yaml-c-ns-alias-node)
                          (yaml-ns-flow-yaml-content n c)
                          (yaml--all (yaml-c-ns-properties n c)
                                     (yaml--any (yaml--all (yaml-s-separate n c) (yaml-ns-flow-yaml-content n c)) (yaml-e-scalar))))))

(defun yaml-ns-yaml-version ()
  "Documentation string."
  (yaml--frame "ns-yaml-version"
               (yaml--all (yaml--rep 1 nil (lambda () (yaml-ns-dec-digit)))
                          (yaml--chr ?\.)
                          (yaml--rep 1 nil (lambda () (yaml-ns-dec-digit))))))

(defun yaml-c-folded ()
  "Documentation string."
  (yaml--frame "c-folded" (yaml--chr ?\>)))

(defun yaml-c-directives-end ()
  "Documentation string."
  (yaml--frame "c-directives-end"
    (yaml--all (yaml--chr ?\-)
               (yaml--chr ?\-)
               (yaml--chr ?\-))))

(defun yaml-s-double-break (n)
  "Documentation string."
  (yaml--frame "s-double-break"
               (yaml--any (yaml-s-double-escaped n)
                          (yaml-s-flow-folded n))))

(defun yaml-s-nb-spaced-text (n)
  "Documentation string."
  (yaml--frame "s-nb-spaced-text"
               (yaml--all (yaml-s-indent n)
                          (yaml-s-white)
                          (yaml--rep2 0 nil (lambda () (yaml-nb-char))))))

(defun yaml-l-folded-content (n t)
  "Documentation string."
  (yaml--frame "l-folded-content"
               (yaml--all (yaml--rep 0 1 (lambda () (yaml--all (yaml-l-nb-diff-lines n)
                                                               (yaml-b-chomped-last (yaml--state-t)))))
                          (yaml-l-chomped-empty n (yaml--state-t)))))

(defun yaml-nb-ns-plain-in-line (c)
  "Documentation string."
  (yaml--frame "nb-ns-plain-in-line"
    (yaml--rep2 0 nil (lambda ()
                        (yaml--all (yaml--rep2 0 nil (lambda () (yaml-s-white)))
                                   (yaml-ns-plain-char c))))))

(defun yaml-nb-single-multi-line (n)
  "Documentation string."
  (yaml--frame "nb-single-multi-line"
               (yaml--all (yaml-nb-ns-single-in-line)
                          (yaml--any (yaml-s-single-next-line n)
                                     (yaml--rep2 0 nil (lambda () (yaml-s-white)))))))

(defun yaml-l-document-suffix ()
  "Documentation string."
  (yaml--frame "l-document-suffix"
               (yaml--all (yaml-c-document-end)
                          (yaml-s-l-comments))))

(defun yaml-c-sequence-start ()
  "Documentation string."
  (yaml--frame "c-sequence-start"
               (yaml--chr ?\[)))

(defun yaml-ns-l-block-map-entry (n)
  "Documentation string."
  (yaml--frame "ns-l-block-map-entry"
               (yaml--any (yaml-c-l-block-map-explicit-entry n)
                          (yaml-ns-l-block-map-implicit-entry n))))

(defun yaml-ns-l-compact-mapping (n)
  "Documentation string."
  (yaml--frame "ns-l-compact-mapping"
               (yaml--all (yaml-ns-l-block-map-entry n)
                          (yaml--rep2 0 nil (lambda () (yaml--all (yaml-s-indent n)
                                                                  (yaml-ns-l-block-map-entry n)))))))

(defun yaml-ns-esc-space ()
  "Documentation string."
  (yaml--frame "ns-esc-space"
               (yaml--chr ?\x20)))

(defun yaml-ns-esc-vertical-tab ()
  "Documentation string."
  (yaml--frame "ns-esc-vertical-tab"
               (yaml--chr ?v)))

(defun yaml-ns-s-implicit-yaml-key (c)
  "Documentation string."
  (yaml--frame "ns-s-implicit-yaml-key"
               (yaml--all (yaml--max 1024)
                          (yaml-ns-flow-yaml-node nil c)
                          (yaml--rep 0 1 (lambda () (yaml-s-separate-in-line))))))

(defun yaml-b-l-folded (n c)
  "Documentation string."
  (yaml--frame "b-l-folded"
               (yaml--any (yaml-b-l-trimmed n c)
                          (yaml-b-as-space))))

(defun yaml-s-l+block-collection (n c)
  "Documentation string."
  (yaml--frame "s-l+block-collection"
    (yaml--all (yaml--rep 0 1 (lambda () (yaml--all (yaml-s-separate (+ n 1) c)
                                                    (yaml-c-ns-properties (+ n 1) c))))
               (yaml-s-l-comments)
               (yaml--any (yaml-l+block-sequence (yaml-seq-spaces n c))
                          (yaml-l+block-mapping n)))))

(defun yaml-c-quoted-quote ()
  "Documentation string."
  (yaml--frame "c-quoted-quote"
               (yaml--all (yaml--chr ?\')
                          (yaml--chr ?\'))))

(defun yaml-l+block-sequence (n)
  "Documentation string."
  (yaml--frame "l+block-sequence"
               (yaml--all (yaml--set m (yaml--auto-detect-indent n))
                          (yaml--rep 1 nil (lambda () (yaml--all (yaml-s-indent (+ n (yaml--state-m)))
                                                                 (yaml-c-l-block-seq-entry (+ n (yaml--state-m)))))))))

(defun yaml-c-double-quote ()
  "Documentation string."
  (yaml--frame "c-double-quote" (yaml--chr ?\")))

(defun yaml-ns-esc-backspace ()
  "Documentation string."
  (yaml--frame "ns-esc-backspace" (yaml--chr ?b)))

(defun yaml-c-flow-json-content (n c)
  "Documentation string."
  (yaml--frame "c-flow-json-content"
               (yaml--any (yaml-c-flow-sequence n c)
                          (yaml-c-flow-mapping n c)
                          (yaml-c-single-quoted n c)
                          (yaml-c-double-quoted n c))))

(defun yaml-c-mapping-end ()
  "Documentation string."
  (yaml--frame "c-mapping-end" (yaml--chr ?\})))

(defun yaml-nb-single-char ()
  "Documentation string."
  (yaml--frame "nb-single-char"
               (yaml--any (yaml-c-quoted-quote)
                          (yaml--but (lambda () (yaml-nb-json)) (lambda () (yaml--chr ?\'))))))

(defun yaml-ns-flow-node (n c)
  "Documentation string."
  (yaml--frame "ns-flow-node"
               (yaml--any (yaml-c-ns-alias-node)
                          (yaml-ns-flow-content n c)
                          (yaml--all (yaml-c-ns-properties n c)
                                     (yaml--any (yaml--all (yaml-s-separate n c) (yaml-ns-flow-content n c))
                                                (yaml-e-scalar))))))

(defun yaml-c-non-specific-tag ()
  "Documentation string."
  (yaml--frame "c-non-specific-tag"
               (yaml--chr ?\!)))

(defun yaml-l-directive-document ()
  "Documentation string."
  (yaml--frame "l-directive-document"
               (yaml--all (yaml--rep 1 nil (lambda () (yaml-l-directive)))
                          (yaml-l-explicit-document))))

(defun yaml-c-l-block-map-explicit-entry (n)
  "Documentation string."
  (yaml--frame "c-l-block-map-explicit-entry"
               (yaml--all (yaml-c-l-block-map-explicit-key n)
                          (yaml--any (yaml-l-block-map-explicit-value n)
                                     (yaml-e-node)))))

(defun yaml-e-node ()
  "Documentation string."
  (yaml--frame "e-node" (yaml-e-scalar)))

(defun yaml-seq-spaces (n c)
  "Documentation string."
  (yaml--frame "seq-spaces"
               (cond ((equal c "block-in") n)
                     ((equal c "block-out") (yaml--sub n 1)))))


(defun yaml-l-yaml-stream ()
  "Documentation string."
  (yaml--frame "l-yaml-stream"
    (yaml--all (yaml--rep2 0 nil (lambda () (yaml-l-document-prefix)))
               (yaml--rep 0 1 (lambda () (yaml-l-any-document)))
               (yaml--rep2 0 nil (lambda ()
                                   (yaml--any (yaml--all (yaml--rep 1 nil (lambda () (yaml-l-document-suffix)))
                                                         (yaml--rep2 0 nil (lambda () (yaml-l-document-prefix)))
                                                         (yaml--rep 0 1 (lambda () (yaml-l-any-document))))
                                              (yaml--all (yaml--rep2 0 nil (lambda () (yaml-l-document-prefix)))
                                                         (yaml--rep 0 1 (lambda () (yaml-l-explicit-document))))))))))

(defun yaml-nb-double-one-line ()
  "Documentation string."
  (yaml--frame "nb-double-one-line"
               (yaml--rep2 0 nil (lambda () (yaml-nb-double-char)))))

(defun yaml-s-l-comments ()
  "Documentation string."
  (yaml--frame "s-l-comments" (yaml--all (yaml--any (yaml-s-b-comment)
                                                    (yaml--start-of-line))
                                         (yaml--rep2 0 nil (lambda () (yaml-l-comment))))))

(defun yaml-nb-char ()
  "Documentation string."
  (yaml--frame "nb-char"
               (yaml--but (lambda () (yaml-c-printable))
                          (lambda () (yaml-b-char))
                          (lambda () (yaml-c-byte-order-mark)))))

(defun yaml-ns-plain-first (c)
  "Documentation string."
  (yaml--frame "ns-plain-first"
               (yaml--any (yaml--but (lambda () (yaml-ns-char))
                                     (lambda () (yaml-c-indicator)))
                          (yaml--all (yaml--any (yaml--chr ?\?)
                                                (yaml--chr ?\:)
                                                (yaml--chr ?\-))
                                     (yaml--chk "=" (yaml-ns-plain-safe c))))))

(defun yaml-c-ns-esc-char ()
  "Documentation string."
  (yaml--frame "c-ns-esc-char"
    (yaml--all (yaml--chr ?\\)
               (yaml--any (yaml-ns-esc-null)
                          (yaml-ns-esc-bell)
                          (yaml-ns-esc-backspace)
                          (yaml-ns-esc-horizontal-tab)
                          (yaml-ns-esc-line-feed)
                          (yaml-ns-esc-vertical-tab)
                          (yaml-ns-esc-form-feed)
                          (yaml-ns-esc-carriage-return)
                          (yaml-ns-esc-escape)
                          (yaml-ns-esc-space)
                          (yaml-ns-esc-double-quote)
                          (yaml-ns-esc-slash)
                          (yaml-ns-esc-backslash)
                          (yaml-ns-esc-next-line)
                          (yaml-ns-esc-non-breaking-space)
                          (yaml-ns-esc-line-separator)
                          (yaml-ns-esc-paragraph-separator)
                          (yaml-ns-esc-8-bit)
                          (yaml-ns-esc-16-bit)
                          (yaml-ns-esc-32-bit)))))

(defun yaml-ns-flow-map-entry (n c)
  "Documentation string."
  (yaml--frame "ns-flow-map-entry"
    (yaml--any (yaml--all (yaml--chr ?\?)
                          (yaml-s-separate n c)
                          (yaml-ns-flow-map-explicit-entry n c))
               (yaml-ns-flow-map-implicit-entry n c))))

(defun yaml-l-explicit-document ()
  "Documentation string."
  (yaml--frame "l-explicit-document"
    (yaml--all (yaml-c-directives-end)
               (yaml--any (yaml-l-bare-document)
                          (yaml--all (yaml-e-node)
                                     (yaml-s-l-comments))))))

(defun yaml-s-white ()
  "Documentation string."
  (yaml--frame "s-white"
    (yaml--any (yaml-s-space)
               (yaml-s-tab))))

(defun yaml-l-keep-empty (n)
  "Documentation string."
  (yaml--frame "l-keep-empty"
    (yaml--all (yaml--rep2 0 nil (lambda () (yaml-l-empty n "block-in")))
               (yaml--rep 0 1 (lambda () (yaml-l-trail-comments n))))))

(defun yaml-ns-tag-prefix ()
  "Documentation string."
  (yaml--frame "ns-tag-prefix"
    (yaml--any (yaml-c-ns-local-tag-prefix)
               (yaml-ns-global-tag-prefix))))

(defun yaml-c-l+folded (n)
  "Documentation string."
  (yaml--frame "c-l+folded"
    (yaml--all (yaml--chr ?\>) (yaml-c-b-block-header (yaml--state-m) (yaml--state-t))
               (yaml-l-folded-content (+ n (yaml--state-m)) (yaml--state-t)))))

(defun yaml-ns-directive-name ()
  "Documentation string."
  (yaml--frame "ns-directive-name"
    (yaml--rep 1 nil (lambda () (yaml-ns-char)))))

(defun yaml-b-char ()
  "Documentation string."
  (yaml--frame "b-char"
    (yaml--any (yaml-b-line-feed)
               (yaml-b-carriage-return))))

(defun yaml-ns-plain-multi-line (n c)
  "Documentation string."
  (yaml--frame "ns-plain-multi-line"
    (yaml--all (yaml-ns-plain-one-line c)
               (yaml--rep2 0 nil (lambda () (yaml-s-ns-plain-next-line n c))))))

(defun yaml-ns-char ()
  "Documentation string."
  (yaml--frame "ns-char"
    (yaml--but (lambda () (yaml-nb-char))
               (lambda () (yaml-s-white)))))

(defun yaml-s-space ()
  "Documentation string."
  (yaml--frame "s-space" (yaml--chr ?\x20)))

(defun yaml-c-l-block-seq-entry (n)
  "Documentation string."
  (yaml--frame "c-l-block-seq-entry"
    (yaml--all (yaml--chr ?\-)
               (yaml--chk "!" (yaml-ns-char))
               (yaml-s-l+block-indented n "block-in"))))

(defun yaml-c-ns-properties (n c)
  "Documentation string."
  (yaml--frame "c-ns-properties"
    (yaml--any (yaml--all (yaml-c-ns-tag-property)
                          (yaml--rep 0 1 (lambda () (yaml--all (yaml-s-separate n c)
                                                               (yaml-c-ns-anchor-property)))))
               (yaml--all (yaml-c-ns-anchor-property)
                          (yaml--rep 0 1 (lambda () (yaml--all (yaml-s-separate n c)
                                                               (yaml-c-ns-tag-property))))))))

(defun yaml-ns-directive-parameter ()
  "Documentation string."
  (yaml--frame "ns-directive-parameter"
    (yaml--rep 1 nil (lambda () (yaml-ns-char)))))

(defun yaml-c-chomping-indicator (t)
  "Documentation string."
  (yaml--frame "c-chomping-indicator"
    (yaml--any (yaml--set t (yaml-strip))
               (yaml--set t (yaml-keep))
               (yaml--set t (yaml-clip)))))

(defun yaml-ns-global-tag-prefix ()
  "Documentation string."
  (yaml--frame "ns-global-tag-prefix"
    (yaml--all (yaml-ns-tag-char)
               (yaml--rep2 0 nil (lambda () (yaml-ns-uri-char))))))

(defun yaml-c-ns-flow-pair-json-key-entry (n c)
  "Documentation string."
  (yaml--frame "c-ns-flow-pair-json-key-entry"
    (yaml--all (yaml-c-s-implicit-json-key "flow-key")
               (yaml-c-ns-flow-map-adjacent-value n c))))

(defun yaml-l-literal-content (n t)
  "Documentation string."
  (yaml--frame "l-literal-content"
    (yaml--all (yaml--rep 0 1 (lambda () (yaml--all (yaml-l-nb-literal-text n) (yaml--rep2 0 nil (lambda () (yaml-b-nb-literal-next n))) (yaml-b-chomped-last (yaml--state-t)))))
               (yaml-l-chomped-empty n (yaml--state-t)))))

(defun yaml-c-document-end ()
  "Documentation string."
  (yaml--frame "c-document-end"
    (yaml--all (yaml--chr ?\.) (yaml--chr ?\.) (yaml--chr ?\.))))

(defun yaml-nb-double-text (n c)
  "Documentation string."
  (yaml--frame "nb-double-text"
    (cond ((equal c "block-key") (yaml-nb-double-one-line))
          ((equal c "flow-in") (yaml-nb-double-multi-line n))
          ((equal c "flow-key") (yaml-nb-double-one-line))
          ((equal c "flow-out") (yaml-nb-double-multi-line n)))))

(defun yaml-s-b-comment ()
  "Documentation string."
  (yaml--frame "s-b-comment"
    (yaml--all (yaml--rep 0 1 (lambda () (yaml--all (yaml-s-separate-in-line)
                                                    (yaml--rep 0 1 (lambda () (yaml-c-nb-comment-text))))))
               (yaml-b-comment))))

(defun yaml-s-block-line-prefix (n)
  "Documentation string."
  (yaml--frame "s-block-line-prefix"
    (yaml-s-indent n)))

(defun yaml-c-tag-handle ()
  "Documentation string."
  (yaml--frame "c-tag-handle"
    (yaml--any (yaml-c-named-tag-handle)
               (yaml-c-secondary-tag-handle)
               (yaml-c-primary-tag-handle))))

(defun yaml-ns-plain-one-line (c)
  "Documentation string."
  (yaml--frame "ns-plain-one-line"
    (yaml--all (yaml-ns-plain-first c)
               (yaml-nb-ns-plain-in-line c))))

(defun yaml-nb-json ()
  "Documentation string."
  (yaml--frame "nb-json"
    (yaml--any (yaml--chr ?\x09) (yaml--chr-range ?\x20 ?\x10FFFF))))

(defun yaml-s-ns-plain-next-line (n c)
  "Documentation string."
  (yaml--frame "s-ns-plain-next-line"
    (yaml--all (yaml-s-flow-folded n)
               (yaml-ns-plain-char c)
               (yaml-nb-ns-plain-in-line c))))

(defun yaml-c-reserved ()
  "Documentation string."
  (yaml--frame "c-reserved"
    (yaml--any (yaml--chr ?\@) (yaml--chr ?\`))))

(defun yaml-b-l-trimmed (n c)
  "Documentation string."
  (yaml--frame "b-l-trimmed"
    (yaml--all (yaml-b-non-content)
               (yaml--rep 1 nil (lambda () (yaml-l-empty n c))))))

(defun yaml-l-document-prefix ()
  "Documentation string."
  (yaml--frame "l-document-prefix"
    (yaml--all (yaml--rep 0 1 (lambda () (yaml-c-byte-order-mark)))
               (yaml--rep2 0 nil (lambda () (yaml-l-comment))))))

(defun yaml-c-byte-order-mark ()
  "Documentation string."
  (yaml--frame "c-byte-order-mark"
    (yaml--chr ?\xFEFF)))

(defun yaml-c-anchor ()
  "Documentation string."
  (yaml--frame "c-anchor"
    (yaml--chr ?\&)))

(defun yaml-s-double-escaped (n)
  "Documentation string."
  (yaml--frame "s-double-escaped"
    (yaml--all (yaml--rep2 0 nil (lambda () (yaml-s-white)))
               (yaml--chr ?\\)
               (yaml-b-non-content)
               (yaml--rep2 0 nil (lambda () (yaml-l-empty n "flow-in")))
               (yaml-s-flow-line-prefix n))))

(defun yaml-ns-esc-32-bit ()
  "Documentation string."
  (yaml--frame "ns-esc-32-bit"
    (yaml--all
     (yaml--chr ?U)
     (yaml--rep 8 8 #'yaml-ns-hex-digit))))

(defun yaml-b-non-content ()
  "Documentation string."
  (yaml--frame "b-non-content" (yaml-b-break)))

(defun yaml-ns-tag-char ()
  "Documentation string."
  (yaml--frame "ns-tag-char"
    (yaml--but (lambda () (yaml-ns-uri-char))
               (lambda () (yaml--chr ?\!))
               (lambda () (yaml-c-flow-indicator)))))

(defun yaml-b-carriage-return ()
  "Documentation string."
  (yaml--frame "b-carriage-return"
    (yaml--chr ?\x0D)))

(defun yaml-s-double-next-line (n)
  "Documentation string."
  (yaml--frame "s-double-next-line"
    (yaml--all (yaml-s-double-break n)
               (yaml--rep 0 1 (lambda () (yaml--all (yaml-ns-double-char)
                                                    (yaml-nb-ns-double-in-line)
                                                    (yaml--any (yaml-s-double-next-line n)
                                                               (yaml--rep2 0 nil (lambda () (yaml-s-white))))))))))

(defun yaml-ns-esc-non-breaking-space ()
  "Documentation string."
  (yaml--frame "ns-esc-non-breaking-space"
    (yaml--chr ?\_)))

(defun yaml-l-nb-diff-lines (n)
  "Documentation string."
  (yaml--frame "l-nb-diff-lines"
    (yaml--all (yaml-l-nb-same-lines )
               (yaml--rep2 0 nil (lambda () (yaml--all (yaml-b-as-line-feed) (yaml-l-nb-same-lines n)))))))

(defun yaml-s-flow-folded (n)
  "Documentation string."
  (yaml--frame "s-flow-folded"
    (yaml--all (yaml--rep 0 1 (lambda () (yaml-s-separate-in-line)))
               (yaml-b-l-folded n "flow-in")
               (yaml-s-flow-line-prefix n))))

(defun yaml-ns-flow-map-explicit-entry (n c)
  "Documentation string."
  (yaml--frame "ns-flow-map-explicit-entry"
    (yaml--any (yaml-ns-flow-map-implicit-entry n c)
               (yaml--all (yaml-e-node) (yaml-e-node)))))

(defun yaml-ns-l-block-map-implicit-entry (n)
  "Documentation string."
  (yaml--frame "ns-l-block-map-implicit-entry"
    (yaml--all (yaml--any (yaml-ns-s-block-map-implicit-key)
                          (yaml-e-node))
               (yaml-c-l-block-map-implicit-value n))))

(defun yaml-l-nb-folded-lines (n)
  "Documentation string."
  (yaml--frame "l-nb-folded-lines"
    (yaml--all (yaml-s-nb-folded-text n)
               (yaml--rep2 0 nil (lambda () (yaml--all (yaml-b-l-folded n "block-in")
                                                       (yaml-s-nb-folded-text n)))))))

(defun yaml-c-l-block-map-explicit-key (n)
  "Documentation string."
  (yaml--frame "c-l-block-map-explicit-key"
    (yaml--all (yaml--chr ?\?)
               (yaml-s-l+block-indented n "block-out"))))

(defun yaml-s-separate (n c)
  "Documentation string."
  (yaml--frame "s-separate"
    (cond ((equal c "block-in") (yaml-s-separate-lines n))
          ((equal c "block-key") (yaml-s-separate-in-line))
          ((equal c "block-out") (yaml-s-separate-lines n))
          ((equal c "flow-in") (yaml-s-separate-lines n))
          ((equal c "flow-key") (yaml-s-separate-in-line))
          ((equal c "flow-out") (yaml-s-separate-lines n)))))

(defun yaml-ns-flow-pair-entry (n c)
  "Documentation string."
  (yaml--frame "ns-flow-pair-entry"
    (yaml--any (yaml-ns-flow-pair-yaml-key-entry n c)
               (yaml-c-ns-flow-map-empty-key-entry n c)
               (yaml-c-ns-flow-pair-json-key-entry n c))))

(defun yaml-c-flow-indicator ()
  "Documentation string."
  (yaml--frame "c-flow-indicator"
    (yaml--any (yaml--chr ?\,)
               (yaml--chr ?\[)
               (yaml--chr ?\])
               (yaml--chr ?\{)
               (yaml--chr ?\}))))

(defun yaml-ns-flow-pair-yaml-key-entry (n c)
  "Documentation string."
  (yaml--frame "ns-flow-pair-yaml-key-entry"
    (yaml--all (yaml-ns-s-implicit-yaml-key "flow-key")
               (yaml-c-ns-flow-map-separate-value n c))))

(defun yaml-e-scalar ()
  "Documentation string."
  (yaml--frame "e-scalar"
    (yaml--empty)))

(defun yaml-s-indent-lt (n)
  "Documentation string."
  (yaml--frame "s-indent-lt"
    (yaml--may (yaml--all (yaml--rep2 0 nil (lambda () (yaml-s-space)))
                          (< (length (yaml--match)) n)))))

(defun yaml-nb-single-one-line ()
  "Documentation string."
  (yaml--frame "nb-single-one-line"
    (yaml--rep2 0 nil (lambda () (yaml-nb-single-char)))))

(defun yaml-c-collect-entry ()
  "Documentation string."
  (yaml--frame "c-collect-entry" (yaml--chr ?\,)))

(defun yaml-ns-l-compact-sequence (n)
  "Documentation string."
  (yaml--frame "ns-l-compact-sequence"
    (yaml--all (yaml-c-l-block-seq-entry n)
               (yaml--rep2 0 nil (lambda () (yaml--all (yaml-s-indent n)
                                                       (yaml-c-l-block-seq-entry n)))))))

(defun yaml-c-comment ()
  "Documentation string."
  (yaml--frame "c-comment" (yaml--chr ?\#)))

(defun yaml-s-line-prefix (n c)
  "Documentation string."
  (yaml--frame "s-line-prefix"
    (cond ((equal c "block-in") (yaml-s-block-line-prefix n))
          ((equal c "block-out") (yaml-s-block-line-prefix n))
          ((equal c "flow-in") (yaml-s-flow-line-prefix n))
          ((equal c "flow-out") (yaml-s-flow-line-prefix n)))))

(defun yaml-s-tab ()
  "Documentation string."
  (yaml--frame "s-tab" (yaml--chr ?\x09)))

(defun yaml-c-directive ()
  "Documentation string."
  (yaml--frame "c-directive" (yaml--chr ?\%)))

(defun yaml-ns-flow-pair (n c)
  "Documentation string."
  (yaml--frame "ns-flow-pair"
    (yaml--any (yaml--all (yaml--chr ?\?)
                          (yaml-s-separate n c)
                          (yaml-ns-flow-map-explicit-entry n c))
               (yaml-ns-flow-pair-entry n c))))

(defun yaml-s-l+block-indented (n c)
  "Documentation string."
  (yaml--frame "s-l+block-indented"
    (yaml--any (yaml--all (yaml-s-indent (yaml--state-m))
                          (yaml--any (yaml-ns-l-compact-sequence (+ n (+ 1 (yaml--state-m))))
                                     (yaml-ns-l-compact-mapping (+ n (+ 1 (yaml--state-m))))))
               (yaml-s-l+block-node n c)
               (yaml--all (yaml-e-node)
                          (yaml-s-l-comments)))))

(defun yaml-c-single-quote ()
  "Documentation string."
  (yaml--frame "c-single-quote"
    (yaml--chr ?\')))

(defun yaml-s-flow-line-prefix (n)
  "Documentation string."
  (yaml--frame "s-flow-line-prefix"
    (yaml--all (yaml-s-indent n)
               (yaml--rep 0 1 (lambda () (yaml-s-separate-in-line))))))

(defun yaml-nb-double-char ()
  "Documentation string."
  (yaml--frame "nb-double-char"
    (yaml--any (yaml-c-ns-esc-char)
               (yaml--but (lambda () (yaml-nb-json))
                          (lambda () (yaml--chr ?\\))
                          (lambda () (yaml--chr ?\"))))))

(defun yaml-l-comment ()
  "Documentation string."
  (yaml--frame "l-comment"
    (yaml--all (yaml-s-separate-in-line)
               (yaml--rep 0 1 (lambda () (yaml-c-nb-comment-text)))
               (yaml-b-comment))))

(defun yaml-ns-hex-digit ()
  "Documentation string."
  (yaml--frame "ns-hex-digit"
    (yaml--any (yaml-ns-dec-digit)
               (yaml--chr-range ?\x41 ?\x46)
               (yaml--chr-range ?\x61 ?\x66))))

(defun yaml-s-l+flow-in-block (n)
  "Documentation string."
  (yaml--frame "s-l+flow-in-block"
    (yaml--all (yaml-s-separate (+ n 1) "flow-out")
               (yaml-ns-flow-node (+ n 1) "flow-out")
               (yaml-s-l-comments))))

(defun yaml-s-l+flow-in-block (n)
  "Documentation string."
  (yaml--frame "s-l+flow-in-block"
    (yaml--all (yaml-s-separate (+ n 1) "flow-out")
               (yaml-ns-flow-node (+ n 1) "flow-out")
               (yaml-s-l-comments))))

(defun yaml-c-flow-json-node (n c)
  "Documentation string."
  (yaml--frame "c-flow-json-node"
    (yaml--all (yaml--rep 0 1 (lambda () (yaml--all (yaml-c-ns-properties n c) (yaml-s-separate n c))))
               (yaml-c-flow-json-content n c))))

(defun yaml-c-b-block-header (m t)
  "Documentation string."
  (yaml--frame "c-b-block-header"
    (yaml--all (yaml--any (yaml--all (yaml-c-indentation-indicator (yaml--state-m))
                                     (yaml-c-chomping-indicator (yaml--state-t)))
                          (yaml--all (yaml-c-chomping-indicator (yaml--state-t))
                                     (yaml-c-indentation-indicator (yaml--state-m))))
               (yaml-s-b-comment))))

(defun yaml-ns-esc-8-bit ()
  "Documentation string."
  (yaml--frame "ns-esc-8-bit"
    (yaml--all (yaml--chr ?x)
               (yaml--rep 2 2 #'yaml-ns-hex-digit))))

(defun yaml-ns-anchor-name ()
  "Documentation string."
  (yaml--frame "ns-anchor-name"
    (yaml--rep 1 nil (lambda () (yaml-ns-anchor-char)))))

(defun yaml-ns-esc-slash ()
  "Documentation string."
  (yaml--frame "ns-esc-slash" (yaml--chr ?\/)))

(defun yaml-s-nb-folded-text (n)
  "Documentation string."
  (yaml--frame "s-nb-folded-text"
    (yaml--all (yaml-s-indent n)
               (yaml-ns-char)
               (yaml--rep2 0 nil (lambda () (yaml-nb-char))))))

(defun yaml-ns-word-char ()
  "Documentation string."
  (yaml--frame "ns-word-char"
    (yaml--any (yaml-ns-dec-digit)
               (yaml-ns-ascii-letter)
               (yaml--chr ?\-))))

(defun yaml-ns-esc-form-feed ()
  "Documentation string."
  (yaml--frame "ns-esc-form-feed" (yaml--chr ?\f)))

(defun yaml-ns-s-block-map-implicit-key ()
  "Documentation string."
  (yaml--frame "ns-s-block-map-implicit-key"
    (yaml--any (yaml-c-s-implicit-json-key "block-key")
               (yaml-ns-s-implicit-yaml-key "block-key"))))

(defun yaml-ns-esc-null ()
  "Documentation string."
  (yaml--frame "ns-esc-null" (yaml--chr ?0)))

(defun yaml-c-ns-tag-property ()
  "Documentation string."
  (yaml--frame "c-ns-tag-property"
    (yaml--any (yaml-c-verbatim-tag)
               (yaml-c-ns-shorthand-tag)
               (yaml-c-non-specific-tag))))

(defun yaml-c-ns-local-tag-prefix ()
  "Documentation string."
  (yaml--frame "c-ns-local-tag-prefix"
    (yaml--all
     (yaml--chr ?\!)
     (yaml--rep2 0 nil (lambda () (yaml-ns-uri-char))))))

(defun yaml-ns-tag-directive ()
  "Documentation string."
  (yaml--frame "ns-tag-directive"
    (yaml--all (yaml--chr ?T)
               (yaml--chr ?A)
               (yaml--chr ?G)
               (yaml-s-separate-in-line)
               (yaml-c-tag-handle)
               (yaml-s-separate-in-line)
               (yaml-ns-tag-prefix))))

(defun yaml-c-flow-mapping (n c)
  "Documentation string."
  (yaml--frame "c-flow-mapping"
    (yaml--all (yaml--chr ?\{)
               (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
               (yaml--rep 0 1 (lambda () (yaml-ns-s-flow-map-entries n (yaml-in-flow c))))
               (yaml--chr ?\}))))

(defun yaml-ns-double-char ()
  "Documentation string."
  (yaml--frame "ns-double-char"
    (yaml--but (lambda () (yaml-nb-double-char))
               (lambda () (yaml-s-white)))))

(defun yaml-ns-ascii-letter ()
  "Documentation string."
  (yaml--frame "ns-ascii-letter"
    (yaml--any (yaml--chr-range ?\x41 ?\x5A)
               (yaml--chr-range ?\x61 ?\x7A))))

(defun yaml-b-break ()
  "Documentation string."
  (yaml--frame "b-break"
    (yaml--any
     (yaml--all (yaml-b-carriage-return)
                (yaml-b-line-feed))
     (yaml-b-carriage-return)
     (yaml-b-line-feed))))

(defun yaml-nb-ns-double-in-line ()
  "Documentation string."
  (yaml--frame "nb-ns-double-in-line"
    (yaml--rep2 0 nil (lambda () (yaml--all (yaml--rep2 0 nil (lambda () (yaml-s-white)))
                                            (yaml-ns-double-char))))))

(defun yaml-s-l+block-node (n c)
  "Documentation string."
  (yaml--frame "s-l+block-node"
    (yaml--any (yaml-s-l+block-in-block n c)
               (yaml-s-l+flow-in-block n))))

(defun yaml-ns-esc-bell ()
  "Documentation string."
  (yaml--frame "ns-esc-bell" (yaml--chr ?a)))

(defun yaml-c-named-tag-handle ()
  "Documentation string."
  (yaml--frame "c-named-tag-handle"
    (yaml--all (yaml--chr ?\!)
               (yaml--rep 1 nil (lambda () (yaml-ns-word-char)))
               (yaml--chr ?\!))))

(defun yaml-s-separate-lines (n)
  "Documentation string."
  (yaml--frame "s-separate-lines"
    (yaml--any (yaml--all (yaml-s-l-comments)
                          (yaml-s-flow-line-prefix n))
               (yaml-s-separate-in-line))))

(defun yaml-l-directive ()
  "Documentation string."
  (yaml--frame "l-directive"
    (yaml--all (yaml--chr ?\%)
               (yaml--any (yaml-ns-yaml-directive)
                          (yaml-ns-tag-directive)
                          (yaml-ns-reserved-directive))
               (yaml-s-l-comments))))

(defun yaml-ns-esc-escape ()
  "Documentation string."
  (yaml--frame "ns-esc-escape"
    (yaml--chr ?e)))

(defun yaml-b-nb-literal-next (n)
  "Documentation string."
  (yaml--frame "b-nb-literal-next"
    (yaml--all (yaml-b-as-line-feed)
               (yaml-l-nb-literal-text n))))

(defun yaml-ns-s-flow-map-entries (n c)
  "Documentation string."
  (yaml--frame "ns-s-flow-map-entries"
    (yaml--all (yaml-ns-flow-map-entry n c)
               (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
               (yaml--rep 0 1 (lambda () (yaml--all (yaml--chr ?\,)
                                                    (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
                                                    (yaml--rep 0 1 (lambda () (yaml-ns-s-flow-map-entries n c)))))))))

(defun yaml-c-nb-comment-text ()
  "Documentation string."
  (yaml--frame "c-nb-comment-text"
    (yaml--all (yaml--chr ?\#)
               (yaml--rep2 0 nil (lambda () (yaml-nb-char))))))

(defun yaml-ns-dec-digit ()
  "Documentation string."
  (yaml--frame "ns-dec-digit"
    (yaml--chr-range ?\x30 ?\x39)))

(defun yaml-ns-yaml-directive ()
  "Documentation string."
  (yaml--frame "ns-yaml-directive"
    (yaml--all (yaml--chr ?Y)
               (yaml--chr ?A)
               (yaml--chr ?M)
               (yaml--chr ?L)
               (yaml-s-separate-in-line)
               (yaml-ns-yaml-version))))

(defun yaml-c-mapping-key ()
  "Documentation string."
  (yaml--frame "c-mapping-key"
    (yaml--chr ?\?)))

(defun yaml-b-as-line-feed ()
  "Documentation string."
  (yaml--frame "b-as-line-feed"
    (yaml-b-break)))

(defun yaml-s-l+block-in-block (n c)
  "Documentation string."
  (yaml--frame "s-l+block-in-block"
    (yaml--any (yaml-s-l+block-scalar n c)
               (yaml-s-l+block-collection n c))))

(defun yaml-ns-esc-paragraph-separator ()
  "Documentation string."
  (yaml--frame "ns-esc-paragraph-separator"
    (yaml--chr ?\P)))

(defun yaml-c-double-quoted (n c)
  "Documentation string."
  (yaml--frame "c-double-quoted"
    (yaml--all (yaml--chr ?\")
               (yaml-nb-double-text n c)
               (yaml--chr ?\"))))

(defun yaml-b-line-feed ()
  "Documentation string."
  (yaml--frame "b-line-feed"
    (yaml--chr ?\x0A)))

(defun yaml-ns-esc-horizontal-tab ()
  "Documentation string."
  (yaml--frame "ns-esc-horizontal-tab"
    (yaml--any (yaml--chr ?t)
               (yaml--chr ?\x09))))

(defun yaml-c-ns-flow-map-empty-key-entry (n c)
  "Documentation string."
  (yaml--frame "c-ns-flow-map-empty-key-entry"
    (yaml--all (yaml-e-node)
               (yaml-c-ns-flow-map-separate-value n c))))

(defun yaml-l-any-document ()
  "Documentation string."
  (yaml--frame "l-any-document"
    (yaml--any (yaml-l-directive-document)
               (yaml-l-explicit-document)
               (yaml-l-bare-document))))

(defun yaml-c-tag ()
  "Documentation string."
  (yaml--frame "c-tag"
    (yaml--chr ?\!)))

(defun yaml-c-escape ()
  "Documentation string."
  (yaml--frame "c-escape"
    (yaml--chr ?\\)))

(defun yaml-c-sequence-end ()
  "Documentation string."
  (yaml--frame "c-sequence-end"
    (yaml--chr ?\])))

(defun yaml-l+block-mapping (n)
  "Documentation string."
  (yaml--frame  "l+block-mapping"
    (yaml--all (yaml--set m (yaml--auto-detect-indent n))
               (yaml--rep 1 nil (lambda () (yaml--all (yaml-s-indent (+ n (yaml--state-m)))
                                                      (yaml-ns-l-block-map-entry (+ n (yaml--state-m)))))))))

(defun yaml-c-ns-flow-map-adjacent-value (n c)
  "Documentation string."
  (yaml--frame "c-ns-flow-map-adjacent-value"
    (yaml--all (yaml--chr ?\:)
               (yaml--any (yaml--all (yaml--rep 0 1 (lambda () (yaml-s-separate n c)))
                                     (yaml-ns-flow-node n c))
                          (yaml-e-node)))))

(defun yaml-s-single-next-line (n)
  "Documentation string."
  (yaml--frame "s-single-next-line"
    (yaml--all (yaml-s-flow-folded n)
               (yaml--rep 0 1 (lambda () (yaml--all (yaml-ns-single-char)
                                                    (yaml-nb-ns-single-in-line)
                                                    (yaml--any (yaml-s-single-next-line n)
                                                               (yaml--rep2 0 nil (lambda () (yaml-s-white))))))))))

(defun yaml-s-separate-in-line ()
  "Documentation string."
  (yaml--frame "s-separate-in-line"
    (yaml--any (yaml--rep 1 nil (lambda () (yaml-s-white)))
               (yaml--start-of-line))))

(defun yaml-b-comment ()
  "Documentation string."
  (yaml--frame "b-comment"
    (yaml--any (yaml-b-non-content)
               (yaml--end-of-stream))))

(defun yaml-ns-esc-backslash ()
  "Documentation string."
  (yaml--frame "ns-esc-backslash"
    (yaml--chr ?\\)))

(defun yaml-c-ns-anchor-property ()
  "Documentation string."
  (yaml--frame "c-ns-anchor-property"
    (yaml--all (yaml--chr ?\&)
               (yaml-ns-anchor-name))))

(defun yaml-ns-plain-safe (c)
  "Documentation string."
  (yaml--frame "ns-plain-safe"
    (cond ((equal c "block-key") (yaml-ns-plain-safe-out))
          ((equal c "flow-in") (yaml-ns-plain-safe-in))
          ((equal c "flow-key") (yaml-ns-plain-safe-in))
          ((equal c "flow-out") (yaml-ns-plain-safe-out)))))

(defun yaml-ns-flow-content (n c)
  "Documentation string."
  (yaml--frame "ns-flow-content"
    (yaml--any (yaml-ns-flow-yaml-content n c)
               (yaml-c-flow-json-content n c))))


(defun yaml-c-ns-flow-map-separate-value (n c)
  "Documentation string."
  (yaml--frame "c-ns-flow-map-separate-value"
    (yaml--all (yaml--chr ?\:)
               (yaml--chk "!" ((yaml-ns-plain-safe) c))
               (yaml--any (yaml--all (yaml-s-separate n c) (yaml-ns-flow-node n c)) (yaml-e-node)))))

(defun yaml-c-ns-flow-map-separate-value
    (n c)
  "Documentation string."
  (yaml--frame "c-ns-flow-map-separate-value"
    (yaml--all (yaml--chr ?\:)
               (yaml--chk "!" (yaml-ns-plain-safe c))
               (yaml--any (yaml--all (yaml-s-separate n c)
                                     (yaml-ns-flow-node n c))
                          (yaml-e-node)))))

(defun yaml-in-flow (c)
  "Documentation string."
  (yaml--frame "in-flow"
    (cond ((equal c "block-key") "flow-key")
          ((equal c "flow-in") "flow-in")
          ((equal c "flow-key") "flow-key")
          ((equal c "flow-out") "flow-in"))))

(defun yaml-c-verbatim-tag ()
  "Documentation string."
  (yaml--frame "c-verbatim-tag"
    (yaml--all (yaml--chr ?\!)
               (yaml--chr ?\<)
               (yaml--rep 1 nil (lambda () (yaml-ns-uri-char)))
               (yaml--chr ?\>))))

(defun yaml-c-literal ()
  "Documentation string."
  (yaml--frame "c-literal" (yaml--chr ?\|)))

(defun yaml-ns-esc-line-feed ()
  "Documentation string."
  (yaml--frame "ns-esc-line-feed"
    (yaml--chr ?n)))

(defun yaml-nb-double-multi-line (n)
  "Documentation string."
  (yaml--frame "nb-double-multi-line"
    (yaml--all (yaml-nb-ns-double-in-line)
               (yaml--any (yaml-s-double-next-line n)
                          (yaml--rep2 0 nil (lambda () (yaml-s-white)))))))

(defun yaml-b-l-spaced (n)
  "Documentation string."
  (yaml--frame "b-l-spaced"
    (yaml--all (yaml-b-as-line-feed)
               (yaml--rep2 0 nil (lambda () (yaml-l-empty n "block-in"))))))

(provide 'yaml)

;;; yaml.el ends here
