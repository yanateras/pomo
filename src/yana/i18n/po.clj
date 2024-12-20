(ns yana.i18n.po
  "gettext PO reader/writer.

  PO is represented as a map where keys are contexts, and values are maps
  between original strings and their translations."
  (:refer-clojure :exclude [read-string])
  (:require [blancas.kern.core :as kern]
            [blancas.kern.lexer :as lex]
            [clojure.string :as str]))

(def ^:private po-style
  (assoc lex/basic-def :comment-line "#"))

(def ^:private po
  (lex/make-parsers po-style))

(def ^:private po-parser
  (let [{:keys [string-lit token trim]} po]
    (kern/bind [_ trim
                _ (token "msgid")
                k string-lit
                _ (token "msgstr")
                v (kern/many string-lit)]
      (kern/return [k (str/join v)]))))

(defn read-string [s]
  {nil (apply array-map
              (sequence cat (kern/value (kern/many po-parser) s)))})

(def ^:private po-format
  "msgid \"%s\"
msgstr \"%s\"

")

(defn write-string [m]
  (str/join (map (partial apply format po-format) (m nil))))
