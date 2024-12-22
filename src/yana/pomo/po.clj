(ns yana.pomo.po
  "gettext PO reader."
  (:refer-clojure :exclude [read-string])
  (:require [blancas.kern.core :as kern]
            [blancas.kern.lexer :as lex]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [yana.pomo :as pomo]))

(set! *warn-on-reflection* true)

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

(defn read-string
  "Reads a PO from a string and returns a catalog."
  [s]
  (reduce (fn [m [c i s]]
            (assoc-in m [c i] s))
          {}
          (kern/value (kern/many po-parser) s)))

(s/fdef read-string
  :args (s/cat :s string?)
  :ret ::pomo/catalog)
