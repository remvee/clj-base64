;; Copyright (c) Remco van 't Veer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution.  By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.  You must not remove this notice, or any other, from
;; this software.

(ns
    #^{:author "Remco van 't Veer"
       :doc "Functions to encode and decode base64 strings."}
  remvee.base64
  (:require [clojure.string :as string]))

(def alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defn encode
  "Encode sequence of bytes to a sequence of base64 encoded
  characters."
  [bytes]
  (when (seq bytes)
    (let [t (->> bytes (take 3) (map #(bit-and (int %) 0xff)))
          v (int (reduce (fn [a b] (+ (bit-shift-left (int a) 8) (int b))) t))
          f #(nth alphabet (bit-and (if (pos? %)
                                      (bit-shift-right v %)
                                      (bit-shift-left v (* % -1)))
                                    0x3f))
          r (condp = (count t)
              1 (concat (map f [2 -4])    [\= \=])
              2 (concat (map f [10 4 -2]) [\=])
              3         (map f [18 12 6 0]))]
      (concat r (lazy-seq (encode (drop 3 bytes)))))))

(defn encode-str
  "Encode a string to a base64 encoded string."
  ([string]
   (string/join (encode (.getBytes string))))
  ([string charset]
   (string/join (encode (.getBytes string charset)))))

(defn decode
  "Decode sequence of base64 encoded characters to a sequence of
  bytes."
  [string]
  (when (seq string)
    (let [t (take 4 (filter #(not= \= %) string))
          v (int (reduce #(+ (bit-shift-left (int %1) 6) (int %2))
                         (map #(.indexOf alphabet (str %)) t)))
          r (map #(int (bit-and (bit-shift-right v %) 0xff))
                 ({2 [4]
                   3 [10 2]
                   4 [16 8 0]} (count t)))]
      (concat r (lazy-seq (decode (drop 4 string)))))))

(defn decode-str
  "Decode a base64 encoded string."
  ([string]
   (String. (bytes (byte-array (decode string)))))
  ([string charset]
   (String. (bytes (byte-array (decode string))) charset)))
