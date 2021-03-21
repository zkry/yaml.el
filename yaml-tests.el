;;; yaml-tests.el --- Tests for yaml.el -*- lexical-binding: t -*-

;;; Code:

(require 'yaml)
(require 'ert)

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
                 ["abc" "def" "ghi"]))
  (should (equal (yaml-parse-string "- abc\n- def\n- ghi")
                 ["abc" "def" "ghi"]))
  (should (equal (yaml-parse-string "- abc\n- def\n- ghi")
                 ["abc" "def" "ghi"]))
  (should (equal (yaml-parse-string "- [abc, def, ghi]\n- [jkl, mno, pqr]\n- [stu, vwx, yz]")
                 [["abc" "def" "ghi"] ["jkl" "mno" "pqr"] ["stu" "vwx" "yz"]]))
  (should (equal (yaml-parse-string "a")
                 "a"))
  (should (equal (yaml-parse-string "\"a\"")
                 "a"))
  (should (equal (yaml-parse-string "'a'")
                 "a"))

  ;; will be fixed when tags are implemented.
  ;;   (should (equal (yaml-parse-string "- !!str \"a\"
  ;; - 'b'
  ;; - &anchor \"c\"
  ;; - *anchor
  ;; - !!str")
  ;;                  ["a" "b" "c" "c" ""]))

  ;; example 8.1
  (should (equal (yaml-parse-string "- | # Empty header
 literal
- >1 # Indentation indicator
  folded
- |+ # Chomping indicator
 keep

- >1- # Both indicators↓
  strip")
                 ["literal\n" " folded\n" "keep\n\n" " strip"]))

  (should (equal (yaml-parse-string ">

 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line

# Comment")

                 "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"))
  (should (equal (yaml-parse-string "- # Empty
- abc")
                 [:null "abc"]))
  (should (equal (yaml-parse-string "block sequence:
  - one
  - two : three
" :object-type 'alist)
                 '(("block sequence" . ["one" (("two" . "three"))]))))

  ;; Example 8.15. Block Sequence Entry Types
  (should (equal (yaml-parse-string "- # Empty
- |
 block node
- - one # Compact
  - two # sequence
- one: two # Compact mapping" :object-type 'alist)
                 [:null "block node\n" ["one" "two"] (("one" . "two"))]))

  ;; Example 8.16. Block mapping
  (should (equal (yaml-parse-string "block mapping:
 key: value
" :object-type 'alist)
                 '(("block mapping" . (("key" . "value"))))))


  ;; Example 8.17. Explicit Block Mapping Entries
  (should (equal (yaml-parse-string "? explicit key # Empty value
? |
  block key
: - one # Explicit compact
  - two # block value
" :object-type 'alist)
                 '(("explicit key" . :null)
                   ("block key\n" . ["one" "two"]))))

  ;; Example 8.18. Implicit Block Mapping Entries
  (should (equal (yaml-parse-string "plain key: in-line value
: # Both empty
\"quoted key\":
- entry
" :object-type 'alist)
                 '(("plain key" . "in-line value")
                   (:null . :null)
                   ("quoted key" . ["entry"]))))


  ;; Example 8.19. Compact Block Mappings
  (should (equal (yaml-parse-string "- sun: yellow
- ? earth: blue
  : moon: white
" :object-type 'alist)
                 [(("sun" . "yellow")) (((("earth" . "blue")) . (("moon" . "white"))))])))

(ert-deftest yaml-parsing-completes ()
  "Tests that the yaml parses."
  (should (yaml-parse-string "one: two"))
  (should (yaml-parse-string "!!map { ? !!str one : !!str one}"))
  (should (yaml-parse-string "\"one\" : [
  \"key\" : value,
 ]"))
  (should (yaml-parse-string "{ ? !!str : \"two\"}"))
  (should (yaml-parse-string "{ ? !!str : !!str }"))
  (should (yaml-parse-string "{ ? : }"))
  (should (yaml-parse-string "{ ? !!str \"one\" : \"two\"}"))
  (should (let ((max-lisp-eval-depth 1000))
            (yaml-parse-string
             "apiVersion: apps/v1
kind: Deployment
metadata:
  name: nginx-deployment
spec:
  selector:
    matchLabels:
      app: nginx
  replicas: 2 # tells deployment to run 2 pods matching the template
  template:
    metadata:
      labels:
        app: nginx
    spec:
      containers:
      - name: nginx
        image: nginx:1.14.2
        ports:
        - containerPort: 80")))

  ;; example 7.17
  ;; TODO: The empty strings of "http://foo.com" and "omitted value" should be tagged as !!null.
  (should (yaml-parse-string
           "{
unquoted : \"separate\",
http://foo.com,
omitted value:,
: omitted key,
}"))

  ;; example 7.18
  (should (yaml-parse-string
           "{
\"adjacent\":value,
\"readable\":·value,
\"empty\":
}"))

  ;; example 7.19
  (should (yaml-parse-string
           "[
foo: bar
]"))

  ;; example 7.20
  (should (yaml-parse-string
           "[
? foo
 bar : baz
]"))

  ;; example 7.21
  (should (yaml-parse-string
           "- [ YAML : separate ]
- [ : empty key entry ]
- [ {JSON: like}:adjacent ]"))

  ;; example 7.22
  (should (not (condition-case n
                   (yaml-parse-string "[ foo
 bar: invalid,
 \"foo...>1K characters...bar\": invalid ]")
                 (error nil))))

  ;; example 7.23
  (should (yaml-parse-string "- [ a, b ]
- { a: b }
- \"a\"
- 'b'
- c"))

  ;; example 8.1
;;   (should (yaml-parse-string "- |
;; \sdetected
;; - >
;; \s
;; \s\s
;; \s\s# detected
;; - |1
;; \s\sexplicit
;; - >
;; \s\t
  ;; \sdetected"))


  ;; example 8.2
  (should (yaml-parse-string "strip: |-
  text
clip: |
  text
keep: |+
  text
"))
  (should (yaml-parse-string "block sequence:
  - one
  - two : three"))
  (should (yaml-parse-string "- # Empty
- abc")))

(provide 'yaml-tests)
