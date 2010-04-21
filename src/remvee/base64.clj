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
  [:use clojure.test])

(def alphabet
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(def #^{:private true}
     test-data
     '(("t" "dA==")
       ("te" "dGU=")
       ("tes" "dGVz")
       ("test" "dGVzdA==")
       ("t\377\377t" "dP//dA==")
       ("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=")))

(defn encode
  "Encode sequence of characters to a sequence of base64 encoded
  characters."
  {:test #(doseq [[plain encoded] test-data]
            (is (= (encode (seq plain)) (seq encoded))))}
  [string]
  (if (empty? string)
    nil
    (let [t (take 3 string)
          v (int (reduce (fn [a b] (+ (bit-shift-left (int a) 8) (int b))) t))
          f #(nth alphabet (bit-and (bit-shift-right v %) 0x3f))
          r (condp = (count t)
              1 (concat (map f '(2 -4))    '(\= \=))
              2 (concat (map f '(10 4 -2)) '(\=))
              3         (map f '(18 12 6 0)))]
      (concat r (lazy-seq (encode (drop 3 string)))))))

(defn encode-str [string]
  "Encode a string to a base64 encoded string."
  (apply str (encode string)))

(defn decode
  "Decode sequence of base64 encoded characters to a sequence of
  characters."
  {:test #(doseq [[plain encoded] test-data]
            (is (= (decode (seq encoded)) (seq plain))))}
  [string]
  (if (empty? string)
    nil
    (let [t (take 4 (filter #(not (= \= %)) string))
          v (int (reduce #(+ (bit-shift-left (int %1) 6) (int %2))
                         (map #(. alphabet (indexOf (str %))) t)))
          r (map #(char (bit-and (bit-shift-right v %) 0xff))
                 ({2 '(4) 3 '(10 2) 4 '(16 8 0)} (count t)))]
      (concat r (lazy-seq (decode (drop 4 string)))))))

(defn decode-str [string]
  "Decode a base64 encoded string."
  (apply str (decode string)))
