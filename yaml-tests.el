;;; yaml-tests.el --- Tests for yaml.el -*- lexical-binding: t -*-

;; Copyright © 2021-2024  Free Software Foundation, Inc.

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

;; This file contains the unit tests for yaml.el

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
  (let* ((yaml--parsing-null-object :null)
         (yaml--parsing-false-object :false))
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
    (should (equal "hello world" (yaml--resolve-scalar-tag "hello world")))))

(ert-deftest yaml-process-literal-text-test ()
  "Test processing literal strings"
  (should (equal (yaml--process-literal-text "\n  abc\n  def")
                 "abc\ndef\n"))
  (should (equal (yaml--process-literal-text "\n  abc\n\n  def")
                 "abc\n\ndef\n"))
  (should (equal (yaml--process-literal-text "1\n  abc\n  def")
                 " abc\n def\n"))
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
               (puthash 'key "value" ht)))))
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

  ;; Example 8.5
  (should (equal (yaml-parse-string " # Strip
  # Comments:
strip: |-
   # text
\s\s
 # Clip
  # comments:" :object-type 'alist)
                 '((strip . "# text"))))

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
                 `((,(intern "block sequence") . ["one" ((two . "three"))]))))

  ;; Example 8.15. Block Sequence Entry Types
  (should (equal (yaml-parse-string "- # Empty
- |
 block node
- - one # Compact
  - two # sequence
- one: two # Compact mapping" :object-type 'alist)
                 [:null "block node\n" ["one" "two"] ((one . "two"))]))

  ;; Example 8.16. Block mapping
  (should (equal (yaml-parse-string "block mapping:
 key: value
" :object-type 'alist)
                 `((,(intern "block mapping") . ((key . "value"))))))


  ;; Example 8.17. Explicit Block Mapping Entries
  (should (equal (yaml-parse-string "? explicit key # Empty value
? |
  block key
: - one # Explicit compact
  - two # block value
" :object-type 'alist)
                 `((,(intern "explicit key") . :null)
                   (,(intern "block key\n") . ["one" "two"]))))

  ;; Example 8.18. Implicit Block Mapping Entries
  (should (equal (yaml-parse-string "plain key: in-line value
: # Both empty
\"quoted key\":
- entry
" :object-type 'alist)
                 `((,(intern "plain key") . "in-line value")
                   (:null . :null)
                   (,(intern "quoted key") . ["entry"]))))


  ;; Example 8.19. Compact Block Mappings
  (should (equal (yaml-parse-string "- sun: yellow
- ? earth: blue
  : moon: white
" :object-type 'alist)
                 [((sun . "yellow")) ((((earth . "blue")) . ((moon . "white"))))]))

  ;; Example 8.20. Block Node Types
  (should (equal (yaml-parse-string "-
  \"flow in block\"
- >
 Block scalar
- !!map # Block collection
  foo : bar
" :object-type 'alist)
                 ["flow in block" "Block scalar\n" ((foo . "bar"))]))

  ;; Example 8.21. Block Scalar Nodes
  ;; TODO: The document has no new line after the literal and folded
  ;; but since it is using default chomp, I think there should be one new line.
  ;; Which one is right?
  (should (equal (yaml-parse-string "literal: |2
  value
folded:
   !foo
  >1
 value" :object-type 'alist)
                 '((literal . "value\n") (folded . "value\n"))))

  (should (equal (yaml-parse-string "sequence: !!seq
- entry
- !!seq
 - nested
mapping: !!map
 foo: bar" :object-type 'alist)
                 '((sequence . ["entry" ["nested"]])
                   (mapping . ((foo . "bar"))))))

  ;; Example 9.2. Document Markers
  (should (equal (yaml-parse-string "%YAML 1.2
---
Document
...")
                 "Document"))

  ;; Example 9.3 Bare Documents
  ;; TODO: Allow first character of bare document to be %
  ;;   (should (equal (yaml-parse-string "%!PS-Adobe-2.0 # Not the first line\n")))

  (should (equal (yaml-parse-string "---
{ matches
% : 20 }
...
---
# Empty
...")
                 "" ;; TODO: Should this be :null instead?
                 ))

  ;; Example 9.4. Explicit Documents
  (should (equal (yaml-parse-string "---
{ matches
% : 20 }
..." :object-type 'alist)
                 `((,(intern "matches %") . 20))))

  (should (equal (yaml-parse-string "strip: |-
   beep
  # text" :object-type 'alist)
                 '((strip . "beep"))))

  (should (equal (yaml-parse-string "
- one: |
   hey
- two" :object-type 'alist)
                 [((one . "hey\n"))
                  "two"]))

  (should (equal (yaml-parse-string "
one:
     note: |-
      this is a note
     two: three" :object-type 'alist)
                 '((one . ((note . "this is a note")
                           (two . "three"))))))

  (should (equal (yaml-parse-string "key: |-
   # not
   # a
   # comment
  # these
 # are
# all
# comments" :object-type 'alist)
                 '((key . "# not\n# a\n# comment"))))
  (should (equal (yaml-parse-string "
abc:
  key-1: |-
   # not
   # a
   # comment
  key-2: [1, 2, \"three\"]
  key-3:
      - deeply
      - |
         nested
  # these
 # are
# all
# comments" :object-type 'alist)
                 '((abc (key-1 . "# not
# a
# comment") (key-2 . [1 2 "three"]) (key-3 . ["deeply" "nested
"])))))

  (should (equal (yaml-parse-string "
key-1: |-2
    ---
    ---
key-2: |2-
    ---
    ---"
                                    :object-type 'alist)
                 '((key-1 . "  ---\n  ---")
                   (key-2 . "  ---\n  ---"))))
  (should (equal (yaml-parse-string "
key-1: |-2
    ---
    ---
key-2: |2-
    ---
    ---"
                                    :object-key-type 'string
                                    :object-type 'alist)
                 '(("key-1" . "  ---\n  ---")
                   ("key-2" . "  ---\n  ---"))))
  (should (equal (yaml-parse-string "''") ""))
  (should (equal (yaml-parse-string "foo: ''" :object-type 'alist)
                 '((foo . ""))))
  ;; anchor should produce same parse as without anchor
  (should (equal (yaml-parse-string "bill-to:  &id001
    city:   East Centerville
    state:  KS
" :object-type 'alist
  :object-key-type 'string
  :string-values t)
                 (yaml-parse-string "bill-to:
    city:   East Centerville
    state:  KS
" :object-type 'alist
  :object-key-type 'string
  :string-values t)))
  ;; anchor should expand to be identical to definition
  (should (equal (yaml-parse-string "bill-to:  &id001
    street: |
            123 Tornado Alley
            Suite 16
    city:   East Cenrville
    state:  KS

ship-to:  *id001
" :object-type 'alist
  :object-key-type 'string
  :string-values t)
                 (yaml-parse-string "bill-to:
    street: |
            123 Tornado Alley
            Suite 16
    city:   East Cenrville
    state:  KS

ship-to:
    street: |
            123 Tornado Alley
            Suite 16
    city:   East Cenrville
    state:  KS

" :object-type 'alist
  :object-key-type 'string
  :string-values t)))
  (should (equal (progn
                   (yaml-parse-string-with-pos "- # Empty\n- abc")
                   (yaml-parse-string "- # Empty\n- abc"))
                 [:null "abc"]))
  (should (equal (yaml-parse-string "deeply:\n  nested:\n    value: >-1\n      test string"
                                    :object-type 'alist)
                 '((deeply (nested (value . " test string"))))))
  (should (equal (yaml-parse-string "deeply:\n  nested:\n    value: >-\n      test string"
                                    :object-type 'alist)
                 '((deeply (nested (value . "test string"))))))
  (should (equal (yaml-parse-string "deeply:\n  nested:\n    value: >-2\n      test string"
                                    :object-type 'alist)
                 '((deeply (nested (value . "test string"))))))
  (should (equal (yaml-parse-string "deeply:\n  nested:\n    value: >-7\n           test string"
                                    :object-type 'alist)
                 '((deeply (nested (value . "test string"))))))
  (should (equal (yaml-parse-string "deeply:\n  nested:\n    value: |-6\n           test string"
                                    :object-type 'alist)
                 '((deeply (nested (value . " test string"))))))
  (should (equal (yaml-parse-string "deeply:\n  nested:\n    value: |-\n           test string"
                                    :object-type 'alist)
                 '((deeply (nested (value . "test string"))))))
  (should (equal (yaml-parse-string ">-\n this is text"
                                    :object-type 'alist)
                 "this is text"))
  (should (equal (yaml-parse-string ">-1\n this is text"
                                    :object-type 'alist)
                 "this is text"))
  (should (equal (yaml-parse-string "top: |1\n  this is text"
                                    :object-type 'alist)
                 '((top . " this is text\n"))))
  (should (equal (yaml-parse-string "top: 0x10" :object-type 'alist)
                 '((top . 16))))
  (should (equal (yaml-parse-string "top: 0o10" :object-type 'alist)
                 '((top . 8)))))

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
  (should (let ((max-lisp-eval-depth 1050))
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
- abc"))

  ;; The following tests make sure long strings don't blow up the stack when trying to traverse the tree.
  (should (yaml-parse-string "oarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoar
# beep" :object-type 'alist))
  (should (yaml-parse-string "'oarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoar'
# beep" :object-type 'alist))
  (should (yaml-parse-string "\"oarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoar\"
# beep" :object-type 'alist))
  (should (yaml-parse-string "|\n oarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoaroarsietnaorsetnaorstnearsotnarsoitneiaosretnaoirsetnaorsentaorsentoar
# beep" :object-type 'alist))
  (should (yaml-parse-string "another:\n  value:\n    deeply: >\n      nested" :object-type 'alist)))


(defun yaml-test-round-trip (o)
  "Test (equal (decode (encode o)) o)"
  (let* ((encoded (yaml-encode o))
         (parsed (yaml-parse-string encoded
                                    :object-type 'alist
                                    :sequence-type 'list))
         (encoded-2 (yaml-encode o)))
    (equal encoded encoded-2)))

(ert-deftest yaml-encode-tests ()
  (should (yaml-test-round-trip 1))
  (should (yaml-test-round-trip "one"))
  (should (yaml-test-round-trip nil))
  (should (yaml-test-round-trip '(1 2 3)))
  (should (yaml-test-round-trip '((1 . 2) (3 . 4) (5 . 6))))
  (should (yaml-test-round-trip
           '(("key" . "value")
             ("nested-map" . ((1 . 2) (3 . 4) (5 . 6))))))
  (should (yaml-test-round-trip
           '("one"
             (("key" . "value")
              ("nested-map" . ((1 . 2) (3 . 4) (5 . 6))))
             "three")))
  (should (yaml-test-round-trip
           '("one"
             (("key" . "value")
              ("nested-map" . ((1 . 2) (3 . 4) (5 . 6))))
             "three")))
  (should (yaml-test-round-trip
           '("one"
             (("key" . "value")
              ("nested-map" . ((1 . 2) (3 . 4) (5 . 6))))
             ("nested" "list" 1 2 3))))
  (should (yaml-test-round-trip
           '("one"
             (("key" . "value")
              ("nested-map" . ((1 . 2) (3 . 4) (5 . 6)))
              ("nested-list" . (1 2 3 4 5)))
             ("nested" "list" 1 2 3))))
  (should (yaml-test-round-trip
           '("one"
             (("key" . "value")
              ("nested-map" . ((1 . 2) (3 . 4) (5 . 6)))
              ("nested-list" . (1 2 3 4 5)))
             ("nested" "list" 1 2 3))))
  (should (yaml-test-round-trip
           '("one" t nil :false)))
  (should (yaml-test-round-trip
           '(t nil)))
  (should (yaml-test-round-trip
           [1 2 3 4]))
  (should (yaml-encode
           '((("aaaa" "bbbb" "cccc") ("dddd" "eeee" "ffff") ("gggg" "hhhh" "iiii"))
             ("jjjj" "kkkk" "llll") ("mmmm" "nnnn" "oooo") ("pppp" "qqqq" "rrrr"))))
  (should (yaml-encode
           '(("aaaa" "bbbb" "cccc" ("dddd" "eeee" "ffff") ("gggg" "hhhh" "iiii"))
             ("jjjj" "kkkk" "llll") ("mmmm" "nnnn" "oooo") ("pppp" "qqqq" "rrrr"))))

  (should (equal
           (yaml-encode :null)
           "null"))
  (should (equal
           (yaml-encode :false)
           "false"))
  (should (equal
           (yaml-encode nil)
           "null"))
  (should (equal
           (yaml-encode [1 2 3])
           "[1, 2, 3]"))
  (should (equal
           (yaml-encode `[((foo . bar) (baz . bax))])
           "- foo: bar\n  baz: bax"))
  (should (equal
           (yaml-encode `((deeper . [((foo . bar) (baz . bax))])))
           "deeper: \n- foo: bar\n  baz: bax"))
  (should (equal (yaml-parse-string
                  (yaml-encode [1 [2 [3] 2] 1])
                  :object-type 'alist
                  :sequence-type 'array)
                 [1 [2 [3] 2] 1]))
  (should (yaml-test-round-trip `[1 [2 [[4 4 4] 3 ((a . 1) (b . 2) (c . 3)) 3] 2] 1]))
  (should (yaml-test-round-trip
           '((build_the_package (script . "if [ -z \"${CUSTOM}\" ]; then
  ./mvnw package
fi") (stage . "build")))))
  (should (yaml-test-round-trip
           `((deeper . [((foo . bar) (baz . bax))
                        ((foo . bar) (baz . bax) (bee . bop))])
             (lower . [((foo . bar) (baz . bax))
                       ((foo . [((foo . bar) (baz . bax))
                                ((foo . bar) (baz . bax) (bee . bop))])
                        (baz . bax)
                        (bee . bop))])))))

(provide 'yaml-tests)

;; yaml-tests.el ends here
