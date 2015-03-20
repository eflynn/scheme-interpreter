(ns ^{:doc "Scheme interpreter in Clojure"
      :author "Evan Flynn"}
  scheme.core
  (:refer-clojure :exclude [true? false?]))

(defprotocol Expression
  "Things to be evaluated."
  (value-of [exp env]))

(defrecord ProcExp [vars body]
  Expression
  (value-of [this env]
    (fn [& args]
      (value-of body (extend-env* vars args env)))))

(defn report-no-binding-found [search-var]
  (throw (Exception. (str "Unbound variable: " search-var))))

(defn empty-env []
  {})

(defn apply-env [env search-var]
  (if-let [result (env search-var)]
    (if (instance? ProcExp result)
      (value-of result env)
      result)
    (report-no-binding-found search-var)))

(defn extend-env* [syms vals old-env]
  (loop [env (transient old-env)
         syms syms
         vals vals]
    (if (and syms vals)
      (recur (assoc! env (first syms) (first vals))
             (next syms)
             (next vals))
      (persistent! env))))

(def primitive-procedures
  {'car first
   'cdr rest
   'cons cons
   '+ +
   '- -
   '* *
   '/ /
   '> >
   '< <
   '= =})

(defn init-env []
  primitive-procedures)

(defn true? [x]
  (not (= x false)))

(defn false? [x]
  (= x false))


(extend-type clojure.lang.Symbol Expression
  (value-of [this env] (apply-env env this)))

(extend-type java.lang.Number Expression
  (value-of [this env] this))


(defrecord CallExp [operator operands]
  Expression
  (value-of [this env]
    (apply (value-of operator env)
           (map #(value-of % env) operands))))

(defrecord IfExp [predicate consequent alternative]
  Expression
  (value-of [this env]
    (value-of (if (true? (value-of predicate env)) consequent alternative) env)))

(defrecord LetExp [var exp body]
  Expression
  (value-of [this env]
    (value-of body (extend-env* (list var)
                                (list (value-of exp env))
                                env))))

(defrecord LetRecExp [var exp body]
  Expression
  (value-of [this env]
    (value-of body (extend-env* (list var) (list exp) env))))

(defn parse-expression [datum]
  (if-not (list? datum)
    datum
    (let [head (first datum)
          tail (rest datum)]
      (case head
        let    (let [[[[var exp1]] body] tail]
                 (LetExp. var (parse-expression exp1) (parse-expression body)))
        letrec (let [[[[var exp1]] body] tail]
                 (LetRecExp. var (parse-expression exp1) (parse-expression body)))
        quote   datum
        if      (apply ->IfExp (map parse-expression tail))
        lambda  (let [[vars body] tail]
                  (ProcExp. vars (parse-expression body)))
        (CallExp. (parse-expression head)
                  (map parse-expression tail))))))

(parse-expression '(letrec ((factorial (lambda (x)
                      (if (< x 1) 1
                          (* (factorial (- x 1)) x)))))
  (factorial 10)))

(value-of (parse-expression '(letrec ((factorial (lambda (x)
                      (if (< x 1) 1
                          (* (factorial (- x 1)) x)))))
  (factorial 10))) (init-env))
