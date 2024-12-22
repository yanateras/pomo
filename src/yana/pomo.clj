(ns yana.pomo
  "gettext PO/MO.

  A catalog is represented as a map where keys are contexts, and values are
  maps between original strings and their translations."
  (:require [clojure.spec.alpha :as s]))

(s/def ::msgctxt (s/nilable string?))
(s/def ::msgid string?)
(s/def ::msgstr string?)

(s/def ::catalog
  (s/map-of ::msgctxt (s/map-of ::msgid ::msgstr)))
