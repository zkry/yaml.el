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

        (= "(match)" var-name)
        (list 'yaml--match)

        (#{"flow-out" "flow-in" "block-in" "block-out"} var-name)
        var-name

        (> (count var-name) 2)
        (list (prefix-package-symbol var-name))

        (= "m" var-name)
        (list 'yaml--state-m)

        (= "t" var-name)
        (list 'yaml--state-t)

        :else (symbol var-name)))

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

    :else
    (let [[f args] (first m)]
      (concat (list (prefix-package-symbol f))
              (map gen-elisp-fn-arg (flatten (list args)))))))

(defmulti gen-elisp-parse-expr #(.getName (class %)))

(defn gen-elsip-char-symbol [c]
  (symbol (str "?\\"
               (if (= c "N")
                 "n"
                 c))))

(defmethod gen-elisp-parse-expr "java.lang.String" [chr]
  (cond (= "<start-of-line>" chr)
        (list 'yaml--start-of-line)
        (= "<end-of-stream>" chr)
        (list 'yaml--end-of-stream)
        (= "<empty>" chr)
        (list 'yaml--empty)
        (or (= (count chr) 1) (= (first chr) \x))
        (list 'yaml--chr (gen-elsip-char-symbol chr))
        (= "N" chr)
        (list (prefix-package-symbol "\n"))
        :else
        (list (prefix-package-symbol chr))))

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
    (let [[a b] (first (get m "(===)"))]
      (list 'yaml--chk "=" (list (first (gen-elisp-fn-arg a)) (gen-elisp-fn-arg b))))

    (get m "(!==)")
    (let [[a b] (first (get m "(!==)"))]
      (list 'yaml--chk "!" (list (first (gen-elisp-fn-arg a)) (gen-elisp-fn-arg b))))

    (get m "(max)")
    (list 'yaml--max (get m "(max)"))

    (get m "(set)")
    (let [[var-name val] (get m "(set)")]
      (list 'yaml--set (symbol var-name) (gen-elisp-fn-arg val)))

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
      (concat (list (prefix-package-symbol f))
              (map gen-elisp-fn-arg (flatten (list args)))))))

(defn gen-elisp-defun [[name rule]]
  (let [params (extract-params rule)
        rule (if (map? rule) (dissoc rule "(...)") rule)]
    (list 'defun (prefix-package-symbol name) params
          "Documentation string."
          (list 'yaml--frame name (gen-elisp-parse-expr rule)))))

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
