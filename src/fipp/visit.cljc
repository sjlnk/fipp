(ns fipp.visit
  "Convert to and visit edn structures."
  #?(:clj  (:refer-clojure :exclude [boolean?])
     :cljs (:refer-clojure :exclude [boolean? char?])))

;;;TODO Stablize public interface

(defprotocol IVisitor

  (visit-unknown [this x])

  (visit-nil [this])
  (visit-boolean [this x])
  (visit-string [this x])
  (visit-character [this x])
  (visit-symbol [this x])
  (visit-keyword [this x])
  (visit-number [this x])
  (visit-seq [this x])
  (visit-vector [this x])
  (visit-map [this x])
  (visit-set [this x])
  (visit-tagged [this x])

  ;; Not strictly Edn...
  (visit-meta [this meta x])
  (visit-var [this x])
  (visit-pattern [this x])
  (visit-record [this x])
  (visit-boolean-array [this x])
  (visit-byte-array [this x])
  (visit-char-array [this x])
  (visit-short-array [this x])
  (visit-int-array [this x])
  (visit-long-array [this x])
  (visit-float-array [this x])
  (visit-double-array [this x])
  (visit-array [this x])
  )

;;TODO: CLJ-1719 and CLJS-1241
(defn boolean? [x]
  (or (true? x) (false? x)))

#?(:cljs (defn char? [x]
           false)
   ;;TODO: CLJ-1720 and CLJS-1242
   :clj (defn regexp? [x]
          (instance? java.util.regex.Pattern x)))

(let [t (type (boolean-array []))]
  (defn boolean-array? [x] (instance? t x)))

(let [t (type (byte-array []))]
  (defn byte-array? [x] (instance? t x)))

(let [t (type (char-array []))]
  (defn char-array? [x] (instance? t x)))

(let [t (type (short-array []))]
  (defn short-array? [x] (instance? t x)))

(let [t (type (int-array []))]
  (defn int-array? [x] (instance? t x)))

(let [t (type (long-array []))]
  (defn long-array? [x] (instance? t x)))

(let [t (type (float-array []))]
  (defn float-array? [x] (instance? t x)))

(let [t (type (double-array []))]
  (defn double-array? [x] (instance? t x)))

(defn array? [x] (.isArray (type x)))

(defn visit*
  "Visits objects, ignoring metadata."
  [visitor x]
  (cond
    (nil? x) (visit-nil visitor)
    (boolean? x) (visit-boolean visitor x)
    (string? x) (visit-string visitor x)
    (char? x) (visit-character visitor x)
    (symbol? x) (visit-symbol visitor x)
    (keyword? x) (visit-keyword visitor x)
    (number? x) (visit-number visitor x)
    (seq? x) (visit-seq visitor x)
    (vector? x) (visit-vector visitor x)
    (record? x) (visit-record visitor x)
    (map? x) (visit-map visitor x)
    (set? x) (visit-set visitor x)
    (tagged-literal? x) (visit-tagged visitor x)
    (var? x) (visit-var visitor x)
    (regexp? x) (visit-pattern visitor x)
    (boolean-array? x) (visit-boolean-array visitor x)
    (byte-array? x) (visit-byte-array visitor x)
    (char-array? x) (visit-char-array visitor x)
    (short-array? x) (visit-short-array visitor x)
    (int-array? x) (visit-int-array visitor x)
    (long-array? x) (visit-long-array visitor x)
    (float-array? x) (visit-float-array visitor x)
    (double-array? x) (visit-double-array visitor x)
    (array? x) (visit-array visitor x)
    :else (visit-unknown visitor x)))

(defn visit [visitor x]
  (if-let [m (meta x)]
    (visit-meta visitor m x)
    (visit* visitor x)))

