(ns yana.i18n.po
  "gettext PO reader/writer.

  PO is represented as a map where keys are contexts, and values are maps
  between original strings and their translations."
  (:refer-clojure :exclude [read-string])
  (:require [blancas.kern.core :as kern]
            [blancas.kern.lexer :as lex]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(s/def ::ctxt (s/nilable string?))
(s/def ::id string?)
(s/def ::str string?)

(s/def ::po
  (s/map-of ::ctxt (s/map-of ::id ::str)))

(def ^:private po-style
  (assoc lex/basic-def :comment-line "#"))

(def ^:private po
  (lex/make-parsers po-style))

(def ^:private po-parser
  (let [{:keys [string-lit token trim]} po]
    (kern/bind [_ trim
                c (kern/optional
                   (kern/>>
                    (token "msgctxt")
                    (kern/many string-lit)))
                i (kern/>>
                   (token "msgid")
                   (kern/many string-lit))
                s (kern/>>
                   (token "msgstr")
                   (kern/many string-lit))]
               (kern/return (map #(if % (str/join %)) [c i s])))))

(defn read-string [s]
  (reduce (fn [m [c i s]]
            (assoc-in m [c i] s))
          {}
          (kern/value (kern/many po-parser) s)))

(s/fdef read-string
  :args (s/cat :s string?)
  :ret ::po)

(def ^:private po-format-with-ctxt
  "msgctxt \"%s\"
msgid \"%s\"
msgstr \"%s\"

")

(def ^:private po-format
  "msgid \"%s\"
msgstr \"%s\"

")

(defn write-string [m]
  (str/join
     (for [[c is] m
           [i s] is]
       (if c
         (format po-format-with-ctxt c i s)
         (format po-format i s)))))

(s/fdef write-string
  :args (s/cat :m ::po)
  :ret string?)
