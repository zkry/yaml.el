#!/usr/bin/env bb

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

;; This file is the Babashka script used to initially generate the YAML grammar Elisp code.
;; The program expects to find a yaml-spec-1.2.json file in the same directory and will
;; generate the elisp code that should go in the main grammar parsing cond.  Due to certain
;; hand optimizations made in the Elisp code, the output of this program has fell out of sync
;; with what is currently in yaml.el (though I don't find this a big problem as the YAML spec
;; should never change).

;;; Code:


(def package-prefix "yaml")
(def fn-name "yaml--parse-from-grammar")

(defn prefix-package-symbol [name]
  (symbol (str "'" name)))

(defn prefix-package-symbol-quoted [name]
  (list 'lambda (list)
        (list (symbol fn-name) (symbol (str "'" name)))))

(defn extract-params
  "Extract the parameters of a rule and return them as a list of symbols."
  [spec]
  (if (map? spec)
    (->> (get spec "(...)")
         list
         (filter identity)
         (flatten)
         (map symbol))
    (list)))

(defn gen-elisp-lambda [f]
  (list 'lambda (list) f))

(defmulti gen-elisp-fn-arg #(if (nil? %)
                              "nil"
                              (.getName (class %))))

(defmethod gen-elisp-fn-arg "nil" [_]
  'nil)
(defmethod gen-elisp-fn-arg "java.lang.Integer" [n]
  n)
(defmethod gen-elisp-fn-arg "java.lang.String" [var-name]
  (cond (= "<auto-detect-indent>" var-name)
        (list 'yaml--auto-detect-indent 'n)

        (= "<end-of-stream>" var-name)
        (list 'yaml--end-of-stream)

        (= "(match)" var-name)
        (list 'yaml--match)

        (#{"in-flow" "flow-out" "flow-in" "block-key" "block-in" "block-out"
           "clip" "keep" "strip"} var-name)
        var-name

        (> (count var-name) 2)
        (list (symbol fn-name) (prefix-package-symbol var-name))

        (= "m" var-name)
        (list 'yaml--state-m)

        (= "t" var-name)
        (list 'yaml--state-t)

        :else (symbol var-name)))

(defmethod gen-elisp-fn-arg "clojure.lang.MapEntry" [var-name]
  (list (symbol (first var-name)) (symbol (second var-name))))

(defmethod gen-elisp-fn-arg "clojure.lang.PersistentArrayMap" [m]
  (cond
    (get m "(+)")
    (let [[var val] (get m "(+)")]
      (list '+ (gen-elisp-fn-arg var) (gen-elisp-fn-arg val)))
    (get m "(-)")
    (let [[var val] (get m "(-)")]
      (list 'yaml--sub (symbol var) val))
    (get m "(len)")
    (list 'length (gen-elisp-fn-arg (get m "(len)")))
    (get m "(ord)")
    (list 'yaml--ord (gen-elisp-lambda (list 'yaml--match))) ;; hack
    (get m "(any)")
    (concat (list 'yaml--any) (map gen-elisp-fn-arg (get m "(any)")))

    :else
    (let [[f args] (first m)]
      (concat (list (symbol fn-name) (prefix-package-symbol f))
              (map gen-elisp-fn-arg (flatten (list args)))))))

(defmulti gen-elisp-parse-expr #(.getName (class %)))

(defn gen-elsip-char-symbol [c]
  (cond
    (re-matches #"[a-zA-Z]" c)
    (symbol (str "?" c))
    :else
    (symbol (str "?\\" (if (= c "N") "n" c)))))

(defmethod gen-elisp-parse-expr "java.lang.String" [chr]
  (cond (= "<start-of-line>" chr)
        (list 'yaml--start-of-line)
        (#{"in-flow" "block-key" "flow-out" "flow-in" "block-in" "block-out"} chr)
        chr
        (= "<end-of-stream>" chr)
        (list 'yaml--end-of-stream)
        (= "<empty>" chr)
        (list 'yaml--empty)
        (or (= (count chr) 1) (= (first chr) \x))
        (list 'yaml--chr (gen-elsip-char-symbol chr))
        (= "N" chr)
        (list (prefix-package-symbol "\n"))
        :else
        (list (symbol fn-name) (prefix-package-symbol chr))))

(defmethod gen-elisp-parse-expr "clojure.lang.PersistentVector" [[min max]]
  (list 'yaml--chr-range
        (gen-elsip-char-symbol min)
        (gen-elsip-char-symbol max)))

(defmethod gen-elisp-parse-expr "clojure.lang.PersistentArrayMap" [m]
  (cond
    (get m "({n})")
    (list 'yaml--rep 'n 'n (prefix-package-symbol-quoted (get m "({n})")))
    (get m "({2})")
    (list 'yaml--rep 2 2 (prefix-package-symbol-quoted (get m "({2})")))
    (get m "({4})")
    (list 'yaml--rep 4 4 (prefix-package-symbol-quoted (get m "({4})")))
    (get m "({8})")
    (list 'yaml--rep 8 8 (prefix-package-symbol-quoted (get m "({8})")))

    (get m "(all)")
    (concat (list 'yaml--all) (map gen-elisp-parse-expr (get m "(all)")))

    (get m "(any)")
    (concat (list 'yaml--any) (map gen-elisp-parse-expr (get m "(any)")))

    (get m "(exclude)")
    (concat (list 'yaml--exclude (get m "(exclude)")))

    (get m "(flip)")
    (let [flip-args (get m "(flip)")
          var (symbol (get flip-args "var"))
          flip-args (dissoc flip-args "var")]
      (concat (list 'cond)
              (map (fn [[from to]]
                     (list (list 'equal var from)
                           (gen-elisp-fn-arg  to)))
                   flip-args)))

    (get m "(<<<)")
    (list 'yaml--may (gen-elisp-parse-expr (get m "(<<<)")))

    (get m "(???)")
    (list 'yaml--rep 0 1 (gen-elisp-lambda (gen-elisp-parse-expr (get m "(???)"))))
    (get m "(***)")
    (list 'yaml--rep2 0 'nil (gen-elisp-lambda (gen-elisp-parse-expr (get m "(***)"))))

    (get m "(+++)")
    (list 'yaml--rep 1 'nil (gen-elisp-lambda (gen-elisp-parse-expr (get m "(+++)"))))

    (get m "(---)")
    (concat (list 'yaml--but) (map gen-elisp-lambda (map gen-elisp-parse-expr (get m "(---)"))))



    (get m "(+)")
    (let [[var val] (get m "(+)")]
      (list 'yaml--add val (symbol var)))

    (get m "(<)")
    (let [[a b] (get m "(<)")]
      (list '< (gen-elisp-fn-arg a) (gen-elisp-fn-arg b)))

    (get m "(<=)")
    (let [[a b] (get m "(<=)")]
      (list '<= (gen-elisp-fn-arg a) (gen-elisp-fn-arg b)))

    (get m "(<==)")
    (let [expr (get m "(<==)")]
      (list 'yaml--chk "<=" (gen-elisp-fn-arg expr)))

    (get m "(===)")
    (let [x (get m "(===)")]
      (list 'yaml--chk "=" (gen-elisp-fn-arg x)))

    (get m "(!==)")
    (let [x (get m "(!==)")]
      (list 'yaml--chk "!" (gen-elisp-fn-arg x)))

    (get m "(max)")
    (list 'yaml--max (get m "(max)"))

    (and (get m "(if)") (get m "(set)"))
    (let [rule (get m "(if)")
          [var-name val] (get m "(set)")]
      (list 'when (gen-elisp-parse-expr rule)
            (list 'yaml--set (symbol var-name) (gen-elisp-fn-arg val))
            't))

    (get m "(case)")
    (let [case-params (get m "(case)")
          var-name (get case-params "var")
          case-params (dissoc case-params "var")]
      (concat
       (list 'cond)
       (map (fn [[case-name function]]
              (list (list 'equal (symbol var-name) case-name)
                    (gen-elisp-fn-arg function)))
            case-params)))


    ;; else funcall with args
    :else
    (let [[f args] (first m)]
      ;;(println "[debug-2]" (pr-str f) (pr-str args))
      (concat (list (symbol fn-name) (prefix-package-symbol f))
              (map gen-elisp-fn-arg (flatten (list args)))))))

(defn gen-elisp-arg-let [params body]
  (list 'let (map-indexed (fn [idx param]
                            (list param (list 'nth idx 'args)))
                          params)
        body))

(defn gen-elisp-defun [[name rule]]
  (let [params (extract-params rule)
        rule (if (map? rule) (dissoc rule "(...)") rule)]
    (list (list 'eq 'state (symbol (str "'" name)))
          (gen-elisp-arg-let params
                             (list 'yaml--frame name (gen-elisp-parse-expr rule))))))

(def json-grammar (into {} (filter (fn [[k _]] (not (= ":" (subs k 0 1)))) (json/parse-string (slurp "./yaml-spec-1.2.json")))))

;; (println (pr-str (get json-grammar "c-mapping-key")))
;; (println (pr-str (gen-elisp-defun ["c-mapping-key" (get json-grammar "c-mapping-key")])))

;; (println (pr-str (gen-elisp-defun ["ns-dec-digit" (get json-grammar "ns-dec-digit")])))

;; (println (pr-str (gen-elisp-defun ["c-printable" (get json-grammar "c-printable")])))

;; (println (pr-str (gen-elisp-defun ["s-indent" (get json-grammar "s-indent")])))
;; (println (pr-str (gen-elisp-defun ["ns-esc-32-bit" (get json-grammar "ns-esc-32-bit")])))

;;(println (pr-str (gen-elisp-defun ["s-indent-lt" (get json-grammar "s-indent-lt")])))
;; (println (pr-str (gen-elisp-defun ["s-l+block-collection" (get json-grammar "s-l+block-collection")])))

;;

;; (println (pr-str (gen-elisp-defun ["ns-plain-safe" (get json-grammar "ns-plain-safe")])))
(println (pr-str *command-line-args*))

(if-let [fn-name (first *command-line-args*)]
  (println (pr-str (gen-elisp-defun [fn-name (get json-grammar fn-name)])))
  (doall (map println (map pr-str (map gen-elisp-defun json-grammar)))))
;;
