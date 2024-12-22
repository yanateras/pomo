(ns yana.pomo.mo
  "gettext MO reader."
  (:refer-clojure :exclude [read read-string])
  (:require [clojure.spec.alpha :as s]
            [yana.pomo :as pomo])
  (:import (java.io ByteArrayInputStream DataInputStream InputStream)
           (java.nio ByteOrder)))

(set! *warn-on-reflection* true)

(defn- read-unsigned-int
  ([^DataInputStream in]
   (read-unsigned-int in ByteOrder/BIG_ENDIAN))
  ([^DataInputStream in ^ByteOrder endian]
   (let [n (.readInt in)]
     (Integer/toUnsignedLong
      (condp = endian
        ByteOrder/BIG_ENDIAN n
        ByteOrder/LITTLE_ENDIAN (Integer/reverseBytes n))))))

(def ^:const ^:private magic-be (long 0x950412de))
(def ^:const ^:private magic-le (long 0xde120495))

(defn- read-header [^DataInputStream in]
  (let [magic (read-unsigned-int in)
        endian (condp = magic
                 magic-be ByteOrder/BIG_ENDIAN
                 magic-le ByteOrder/LITTLE_ENDIAN
                 (throw
                  (ex-info "Invalid MO magic number"
                           {:actual magic
                            :expected #{magic-be magic-le}})))]
    {:endian endian
     :revision (read-unsigned-int in endian)
     :entry-count (read-unsigned-int in endian)
     :id-table-offset (read-unsigned-int in endian)
     :str-table-offset (read-unsigned-int in endian)}))

(def ^:const ^:private header-size 20)

(defrecord ^:private Entry [length offset])

(defn- read-entry [^DataInputStream in ^ByteOrder endian]
  (Entry. (read-unsigned-int in endian)
          (read-unsigned-int in endian)))

(def ^:const ^:private entry-size (* 2 Integer/BYTES))

(defn- table-size [n]
  (* n entry-size))

(defn- string-size [entry]
  (inc (:length entry)))

(defn- string-offset [entry]
  (+ (:offset entry)
     (string-size entry)))

(defn- read-string [^InputStream in entry]
  (let [bs (byte-array (string-size entry))]
    (.read in bs)
    (String. bs "UTF-8")))

(defn read
  "Reads a MO from an input stream and returns a catalog."
  [^InputStream in]
  (let [in (DataInputStream. in)
        {:keys [endian
                entry-count
                id-table-offset
                str-table-offset]} (read-header in)
        _ (.skip in (- id-table-offset
                       header-size))
        id-entries (for [_ (range entry-count)]
                     (read-entry in endian))
        _ (.skip in (- str-table-offset
                       id-table-offset
                       (table-size entry-count)))
        str-entries (for [_ (range entry-count)]
                      (read-entry in endian))
        _ (.skip in (- (:offset (first id-entries))
                       str-table-offset
                       (table-size entry-count)))
        ids (for [id-entry id-entries]
              (read-string in id-entry))
        _ (.skip in (- (:offset (first str-entries))
                       (string-offset (last id-entries))))
        strs (for [str-entry str-entries]
               (read-string in str-entry))]
    {nil (zipmap ids strs)}))

(def ^:private input-stream?
  (partial instance? InputStream))

(s/fdef read
  :args (s/cat :in input-stream?)
  :ret ::pomo/catalog)

(defn read-bytes
  "Reads a MO from a byte array and returns a catalog."
  [^bytes bs]
  (read (ByteArrayInputStream. bs)))

(s/fdef read-bytes
  :args (s/cat :bs bytes?)
  :ret ::pomo/catalog)
