#!/usr/bin/env bb

(def package-prefix "yaml")

(defn prefix-package-symbol [name]
  (symbol (str package-prefix "-" name)))

(defn prefix-package-symbol-quoted [name]
  (symbol (str "#'" package-prefix "-" name)))

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

(defmulti gen-elisp-fn-arg #(.getName (class %)))
(defmethod gen-elisp-fn-arg "java.lang.String" [var-name]
  (symbol var-name))
(defmethod gen-elisp-fn-arg "clojure.lang.PersistentArrayMap" [m]
  (cond
    (get m "(+)")
    (let [[var val] (get m "(+)")]
      (list 'yaml--add (symbol var) val))
    (get m "(-)")
    (let [[var val] (get m "(-)")]
      (list 'yaml--sub (symbol var) val))
    :else
    (let [[f args] (first m)]
      (concat (list (prefix-package-symbol f))
              (map gen-elisp-fn-arg (flatten (list args)))))))

(defmulti gen-elisp-parse-expr #(.getName (class %)))

(defmethod gen-elisp-parse-expr "java.lang.String" [chr]
  (list 'yaml--chr chr))

(defmethod gen-elisp-parse-expr "clojure.lang.PersistentVector" [[min max]]
  (list 'yaml--chr-range
        (symbol (str "?\\" min))
        (symbol (str "?\\" max))))

(defmethod gen-elisp-parse-expr "clojure.lang.PersistentArrayMap" [m]
  (cond
    (get m "({n})")
    (list 'yaml--repeat-n 'n (prefix-package-symbol-quoted (get m "({n})")))
    (get m "({2})")
    (list 'yaml--repeat-n 2 (prefix-package-symbol-quoted (get m "({2})")))
    (get m "({4})")
    (list 'yaml--repeat-n 4 (prefix-package-symbol-quoted (get m "({4})")))
    (get m "({8})")
    (list 'yaml--repeat-n 4 (prefix-package-symbol-quoted (get m "({8})")))

    (get m "(all)")
    (concat (list 'yaml--all) (map gen-elisp-parse-expr (get m "(all)")))

    (get m "(any)")
    (concat (list 'yaml--any) (map gen-elisp-parse-expr (get m "(any)")))

    (get m "(flip)")
    (let [flip-args (get m "(flip)")
          var (gen-elisp-fn-arg (get flip-args "var"))
          block-in (or (and (get flip-args "block-in") (gen-elisp-fn-arg (get flip-args "block-in"))) 'f)
          block-out (or (and (get flip-args "block-out") (gen-elisp-fn-arg (get flip-args "block-out"))) 'f)
          block-key (or (and (get flip-args "block-key") (gen-elisp-fn-arg (get flip-args "block-key"))) 'f)
          flow-in (or (and (get flip-args "flow-in") (gen-elisp-fn-arg (get flip-args "flow-in"))) 'f)
          flow-out (or (and (get flip-args "flow-out") (gen-elisp-fn-arg (get flip-args "flow-out"))) 'f)
          flow-key (or (and (get flip-args "flow-key") (gen-elisp-fn-arg (get flip-args "flow-key"))) 'f)]
      (concat (list 'yaml--flip var block-in block-out block-key flow-in flow-out flow-key)))

    (get m "(<<<)")
    (list 'yaml--may (gen-elisp-parse-expr (get m "(<<<)")))

    (get m "(???)")
    (list 'yaml--rep 0 1 (gen-elisp-parse-expr (get m "(???)")))
    (get m "(***)")
    (list 'yaml--rep2 0 'f (gen-elisp-parse-expr (get m "(***)")))

    (get m "(+++)")
    (list 'yaml--rep 1 'f (gen-elisp-parse-expr (get m "(+++)")))

    (get m "(+)")
    (let [[var val] (get m "(+)")]
      (list 'yaml--add val (symbol var)))

    ;; else funcall with args
    :else
    (let [[f args] (first m)]
      ;;(println "[debug-2]" (pr-str f) (pr-str args))
      (concat (list (prefix-package-symbol f))
              (map gen-elisp-fn-arg (flatten (list args)))))))

(defn gen-elisp-defun [[name rule]]
  (let [params (extract-params rule)]
    (println "[debug]"  (pr-str name) (pr-str rule))
    (list 'defun (prefix-package-symbol name) params
          "Documentation string."
          (list 'yaml-debug-symbol name)
          (gen-elisp-parse-expr rule))))

(def json-grammar (into {} (filter (fn [[k _]] (not (= ":" (subs k 0 1)))) (json/parse-string (slurp "./yaml-spec-1.2.json")))))

;; (println (pr-str (get json-grammar "c-mapping-key")))
;; (println (pr-str (gen-elisp-defun ["c-mapping-key" (get json-grammar "c-mapping-key")])))

;; (println (pr-str (gen-elisp-defun ["ns-dec-digit" (get json-grammar "ns-dec-digit")])))

;; (println (pr-str (gen-elisp-defun ["c-printable" (get json-grammar "c-printable")])))

;; (println (pr-str (gen-elisp-defun ["s-indent" (get json-grammar "s-indent")])))
;; (println (pr-str (gen-elisp-defun ["ns-esc-32-bit" (get json-grammar "ns-esc-32-bit")])))

;; (println (pr-str (gen-elisp-defun ["s-indent-lt" (get json-grammar "s-indent-lt")])))
;; (println (pr-str (gen-elisp-defun ["s-l+block-collection" (get json-grammar "s-l+block-collection")])))
;; (println (pr-str (gen-elisp-defun ["in-flow" (get json-grammar "in-flow")])))

(println (first json-grammar))
(gen-elisp-defun (first json-grammar))
