;;; yaml-tests.el --- Tests for yaml.el -*- lexical-binding: t -*-

;;; Code:

(require 'yaml)
(require 'ert)

(ert-deftest yaml-parsing-functions-characters ()
  "Tests the auto-generated parsing "
  ;; 5.1. Character Set
  (should (yaml-parse "\u0009"
            (yaml-c-printable)))
  (should (yaml-parse "\u000A"
            (yaml-c-printable)))
  (should (yaml-parse "\u000A"
            (yaml-c-printable)))
  (should (not (yaml-parse "\u000B"
                 (yaml-c-printable))))
  (should (yaml-parse "\u0020"
            (yaml-c-printable)))
  (should (yaml-parse "\u0085"
            (yaml-c-printable)))
  (should (yaml-parse "\uE000"
            (yaml-c-printable)))
  (should (yaml-parse "\u10FFFF"
            (yaml-c-printable)))
  (should (yaml-parse "\u0009"
            (yaml-nb-json)))
  (should (yaml-parse "\u0009"
            (yaml-nb-json)))
  (should (yaml-parse "\u0020"
            (yaml-nb-json)))
  (should (yaml-parse "\uAAAAAA"
            (yaml-nb-json)))
  ;; 5.2. Character Encodings
  (should (yaml-parse "\uFEFF"
            (yaml-c-byte-order-mark)))
  ;; 5.3. Indicator Characters
  (should (yaml-parse "-"
            (yaml-c-sequence-entry)))
  (should (yaml-parse "?"
            (yaml-c-mapping-key)))
  (should (yaml-parse ":"
            (yaml-c-mapping-value)))
  (should (yaml-parse ","
            (yaml-c-collect-entry)))
  (should (yaml-parse "["
            (yaml-c-sequence-start)))
  (should (yaml-parse "]"
            (yaml-c-sequence-end)))
  (should (yaml-parse "{"
            (yaml-c-mapping-start)))
  (should (yaml-parse "}"
            (yaml-c-mapping-end)))
  (should (yaml-parse "#"
            (yaml-c-comment)))
  (should (yaml-parse "&"
            (yaml-c-anchor)))
  (should (yaml-parse "*"
            (yaml-c-alias)))
  (should (yaml-parse "!"
            (yaml-c-tag)))
  (should (yaml-parse "|"
            (yaml-c-literal)))
  (should (yaml-parse ">"
            (yaml-c-folded)))
  (should (yaml-parse "'"
            (yaml-c-single-quote)))
  (should (yaml-parse "\""
            (yaml-c-double-quote)))
  (should (yaml-parse "%"
            (yaml-c-directive)))
  (should (yaml-parse "@"
            (yaml-c-reserved)))
  (should (yaml-parse "`'"
            (yaml-c-reserved)))
  (should (yaml-parse "-"
            (yaml-c-indicator)))
  (should (yaml-parse "?"
            (yaml-c-indicator)))
  (should (yaml-parse ":"
            (yaml-c-indicator)))
  (should (yaml-parse ","
            (yaml-c-indicator)))
  (should (yaml-parse "|"
            (yaml-c-indicator)))
  (should (yaml-parse "%"
            (yaml-c-indicator)))
  (should (yaml-parse ","
            (yaml-c-flow-indicator)))
  (should (yaml-parse "["
            (yaml-c-flow-indicator)))
  (should (yaml-parse "]"
            (yaml-c-flow-indicator)))
  (should (yaml-parse "}"
            (yaml-c-flow-indicator)))

  ;; 5.4. Line Break Characters
  (should (yaml-parse "\u000A"
            (yaml-b-line-feed)))
  (should (yaml-parse "\u000D"
            (yaml-b-carriage-return)))
  (should (yaml-parse "\u000A"
            (yaml-b-char)))
  (should (yaml-parse "a"
            (yaml-nb-char)))
  (should (not (yaml-parse "\u000A"
                 (yaml-nb-char))))
  (should (not (yaml-parse "\u000D"
                 (yaml-nb-char))))

  (should (yaml-parse "\u000A"
            (yaml-b-break)))
  (should (yaml-parse "\u000D\u000A"
            (yaml-b-break)))
  (should (yaml-parse "\u000D"
            (yaml-b-break)))
  (should (yaml-parse "\u000D"
            (yaml-b-as-line-feed)))
  (should (yaml-parse "\u000A"
            (yaml-b-non-content)))

  ;; 5.5. White Space Characters
  (should (yaml-parse " "
            (yaml-s-space)))
  (should (yaml-parse "\t"
            (yaml-s-tab)))
  (should (yaml-parse " "
            (yaml-s-white)))
  (should (yaml-parse "\t"
            (yaml-s-white)))
  (should (yaml-parse "x"
            (yaml-ns-char)))
  (should (not (yaml-parse ""
                 (yaml-ns-char))))

  ;; 5.6. Miscellaneous Characters
  (should (yaml-parse "0"
            (yaml-ns-dec-digit)))
  (should (yaml-parse "1"
            (yaml-ns-dec-digit)))
  (should (yaml-parse "4"
            (yaml-ns-dec-digit)))
  (should (yaml-parse "9"
            (yaml-ns-dec-digit)))
  (should (yaml-parse "A"
            (yaml-ns-hex-digit)))
  (should (yaml-parse "F"
            (yaml-ns-hex-digit)))
  (should (not (yaml-parse "G"
                 (yaml-ns-hex-digit))))
  (should (yaml-parse "a"
            (yaml-ns-hex-digit)))
  (should (yaml-parse "f"
            (yaml-ns-hex-digit)))
  (should (yaml-parse "a"
            (yaml-ns-ascii-letter)))
  (should (yaml-parse "z"
            (yaml-ns-ascii-letter)))
  (should (yaml-parse "-"
            (yaml-ns-word-char)))
  (should (yaml-parse "#"
            (yaml-ns-uri-char)))
  (should (yaml-parse "A"
            (yaml-ns-uri-char)))
  (should (yaml-parse "_"
            (yaml-ns-uri-char)))
  (should (yaml-parse ";"
            (yaml-ns-uri-char)))
  (should (yaml-parse "a"
            (yaml-ns-tag-char)))
  ;; 5.7. Escaped Characters
  (should (yaml-parse "\\"
            (yaml-c-escape)))

  (should (yaml-parse "n"
            (yaml-ns-esc-line-feed)
            ))
  (should (yaml-parse "\\n"
            (yaml-c-ns-esc-char))))

(ert-deftest yaml-parsing-functions-basic-structure ()
  "Tests the auto-generated parsing"
  (should (yaml-parse "    "
            (yaml-s-indent 4)))
  (should (yaml-parse " "
            (yaml-s-indent 1)))
  ;; (should (yaml-parse " " (yaml-s-indent-lt 1)))
  ;; (should (yaml-parse " " (yaml-s-indent-le 1)))
  (should (yaml-parse "    "
            (yaml-s-separate-in-line)))
  (should (yaml-parse "  "
            (yaml-s-block-line-prefix 2)))
  (should (yaml-parse "  "
            (yaml-s-block-line-prefix 2)))
  (should (yaml-parse "  "
            (yaml-s-flow-line-prefix 2)))
  (should (yaml-parse "  \n"
            (yaml-l-empty 2 "block-in")))
  (should (yaml-parse "\n  \n \n\n"
                      (yaml-b-l-trimmed 2 "block-in")))
  (should (yaml-parse "\n"
                      (yaml-b-as-space)))
  (should (yaml-parse "\n  "
                      (yaml-s-flow-folded 1)))

  ;; 6.6. Comments
  (should (yaml-parse "# Comment"
                      (yaml-c-nb-comment-text)))
  (should (yaml-parse "\n"
                      (yaml-b-comment)))
  (should (yaml-parse "    # Comment\n"
                      (yaml-s-b-comment)))
  (should (yaml-parse "  # Comment\n   \n\n"
                      (yaml-l-comment)))
  (should (yaml-parse "    # Comment\n          # Lines\n"
                      (yaml-s-l-comments)))

  ;; 6.7. Separation Lines
  (should (yaml-parse "\n# Statistics:\n  "
                      (yaml-s-separate-lines 2)))

  ;; 6.8. Directives
  (should (yaml-parse "FOO  bar baz"
                      (yaml-ns-reserved-directive)))
  (should (yaml-parse "%FOO  bar baz # Should be ignored"
                      (yaml-l-directive)))

  ;; 6.8.1. YAML Directives
  (should (yaml-parse "1.3"
                      (yaml-ns-yaml-version)))
  (should (yaml-parse "YAML 1.3"
                      (yaml-ns-yaml-directive)))

  ;; 6.8.3. TAG Directives
  (should (yaml-parse "TAG !yaml! tag:yaml.org,2002:"
                      (yaml-ns-tag-directive)))

  (should (yaml-parse "!e!"
                      (yaml-c-named-tag-handle)))

  ;;6.9. Node Properties
  (should (yaml-parse "&a2"
                      (yaml-c-ns-anchor-property)))

  (should (yaml-parse "!<tag:yaml.org,2002:str>"
                      (yaml-c-verbatim-tag)))
  (should (yaml-parse "!local"
                      (yaml-c-ns-shorthand-tag)))

  (should (yaml-parse "&anchor"
            (yaml-c-ns-anchor-property))))


(defun yaml--hash-table-equal (h1 h2)
  (when (or (not (hash-table-p h1))
            (not (hash-table-p h2)))
    (error "arguments to yaml--hash-table-equal must be a hash-table."))
  (if (not (= (hash-table-size h1)
              (hash-table-size h2)))
      nil
    (let ((res t))
      (maphash (lambda (key value)
                 (when (not (equal (gethash key h2)
                                   value))
                   (setq res nil)))
               h1)
      res)))

(ert-deftest yaml-parsing-scalar ()
  "Test conversion of !!str to JSON scalar"
  (should (equal :null (yaml--resolve-scalar-tag "null")))
  (should (equal :false (yaml--resolve-scalar-tag "false")))
  (should (equal t (yaml--resolve-scalar-tag "true")))
  (should (equal "xtruex" (yaml--resolve-scalar-tag "xtruex")))
  (should (equal 0 (yaml--resolve-scalar-tag "0")))
  (should (equal 10 (yaml--resolve-scalar-tag "10")))
  (should (equal "x10" (yaml--resolve-scalar-tag "x10")))
  (should (equal 10.52 (yaml--resolve-scalar-tag "10.52")))
  (should (equal 52.0 (yaml--resolve-scalar-tag ".52e2")))
  (should (equal 1.0e+INF (yaml--resolve-scalar-tag ".Inf")))
  (should (equal "hello world" (yaml--resolve-scalar-tag "hello world"))))

(ert-deftest yaml-process-literal-text-test ()
  "Test processing literal strings"
  (should (equal (yaml--process-literal-text "\n  abc\n  def")
                 "abc\ndef\n"))
  (should (equal (yaml--process-literal-text "\n  abc\n\n  def")
                 "abc\n\ndef\n"))
  (should (equal (yaml--process-literal-text "1\n  abc\n  def")
                 " abc\n def"))
  (should (equal (yaml--process-literal-text "2\n   abc\n   def")
                 " abc\n def\n"))
  (should (equal (yaml--process-literal-text "-\n   abc\n   def\n   \n   \n   \n")
                 "abc\ndef"))
  (should (equal (yaml--process-literal-text "\n   abc\n   def\n   \n   \n   \n")
                 "abc\ndef\n"))
  (should (equal (yaml--process-literal-text "+\n   abc\n   def\n   \n   \n   \n")
                 "abc\ndef\n\n\n\n")))

(ert-deftest yaml-process-folded-text-test ()
  "Test processing literal strings"
  (should (equal (yaml--process-folded-text "\n  abc\n  def")
                 "abc def\n"))
  (should (equal (yaml--process-folded-text "\n  abc\n\n  def")
                 "abc\ndef\n"))
  (should (equal (yaml--process-folded-text "1\n  abc\n  def")
                 " abc\n def\n"))
  (should (equal (yaml--process-folded-text "2\n   abc\n   def")
                 " abc\n def\n"))
  (should (equal (yaml--process-folded-text "-\n   abc\n   def\n   \n   \n   \n")
                 "abc def"))
  (should (equal (yaml--process-folded-text "\n   abc\n   def\n   \n   \n   \n")
                 "abc def\n"))
  (should (equal (yaml--process-folded-text "+\n   abc\n   def\n   \n   \n   \n")
                 "abc def\n\n\n\n")))

(ert-deftest yaml-parsing-yaml ()
  "Tests YAML parsing."
  (should (yaml--hash-table-equal
           (yaml-parse-string "key: value")
           (let ((ht (make-hash-table :test 'equal)))
             (prog1 ht
               (puthash "key" "value" ht)))))
  (should (equal (yaml-parse-string "value")
                 "value"))
  (should (equal (yaml-parse-string "[abc, def, ghi]")
                 '("abc" "def" "ghi")))
  (should (equal (yaml-parse-string "- abc\n- def\n- ghi")
                 '("abc" "def" "ghi")))
  (should (equal (yaml-parse-string "- abc\n- def\n- ghi")
                 '("abc" "def" "ghi")))
  (should (equal (yaml-parse-string "- [abc, def, ghi]\n- [jkl, mno, pqr]\n- [stu, vwx, yz]")
                 '(("abc" "def" "ghi") ("jkl" "mno" "pqr") ("stu" "vwx" "yz"))))
  )


;; (yaml-parse-string
;;  "one: two
;; three: four")

;; (yaml-parse-string
;;  "apiVersion: apps/v1
;; kind: Deployment
;; metadata:
;;   name: nginx-deployment
;; spec:
;;   selector:
;;     matchLabels:
;;       app: nginx
;;   replicas: 2 # tells deployment to run 2 pods matching the template
;;   template:
;;     metadata:
;;       labels:
;;         app: nginx
;;     spec:
;;       containers:
;;       - name: nginx
;;         image: nginx:1.14.2
;;         ports:
;;         - containerPort: 80")

;; (yaml-parse-string
;;  "schema: 'packages/api/src/schema.graphql'
;; documents: 'packages/app/src/components/**/*.graphql'
;; extensions:
;;   customExtension:
;;     foo: true")

(yaml-parse-string
 "schema: './schema/*.graphql'
extensions:
  codegen:
    generates:
      ./src/types.ts:
        plugins:
          - typescript
          - typescript-resolvers")

;; (yaml-parse-string "apiVersion: v1
;; description: A Helm chart for bidder canary
;; home: https://github.com/travelaudience/bidder-bidder
;; maintainers:
;; - name: realtime-team
;; name: rtb-canary
;; version: 1.26.3
;; ")

(provide 'yaml-tests)
