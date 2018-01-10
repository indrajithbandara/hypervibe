(ns hypervibe.core
  (:require [clojure.java.shell :as shell]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [com.rpl.specter :as s])
  (:import (java.net URL)
           (java.util Random)
           (java.lang ProcessBuilder)
           (java.io FileNotFoundException File IOException)
           (clojure.lang PersistentVector LazySeq PersistentHashMap Keyword)
           (java.nio.file StandardCopyOption Files)))

(def dirs {:targ "target/"})
(def files {:hyper "hypervibe" :hyper-pack "hypervibe-packaged"})
(def exten {:edn ".edn" :json ".json"})

(defn- ^Boolean edn-pref?
  [^File edn-file]
  (.endsWith (.getAbsolutePath edn-file)
    ".edn"))

(defn- ^Boolean file-exist?
  [^File file]
  (.exists (.getAbsoluteFile file)))

(defn- ^Boolean edn-file-exist?
  [^File edn-file]
  (and (file-exist? edn-file)
    (edn-pref? edn-file)))

(defn- ^Boolean json-pref?
  [^File json-file]
  (.endsWith (.getAbsolutePath json-file)
    ".json"))

(defn- ^PersistentVector json-file-exist?
  [^File json-file]
  (and (file-exist? json-file)
    (json-pref? json-file)))

(defn- ^Boolean edn-file-exist?
  [^File edn-file]
  (and (file-exist? edn-file)
    (edn-pref? edn-file)))

(defn- ^PersistentHashMap slurp-edn
  [^File edn-file]
  (if (edn-file-exist? edn-file)
    (try (edn/read-string
           (slurp (.getAbsolutePath edn-file)))
         (catch Exception
                _))))

(defn- ^String rand-16-char
  []
  (string/upper-case (Long/toHexString (Double/doubleToLongBits (.nextLong (Random.))))))

(defn- ^String edn->json
  [^File edn-file]
  (if-let [edn (slurp-edn edn-file)]
    (try (cheshire/generate-string
           edn
           {:pretty true})
         (catch Exception
                _))))

(defn- ^File spit-json
  []
  (try (spit
         (File. (str
                  (dirs :targ)
                  (files :hyper)
                  (exten :json)))
         (edn->json
           (File. (str
                    (files :hyper)
                    (exten :edn)))))
       (catch IOException
              _)))

(defn- str-eq-kv
  [^PersistentHashMap params]
  (map (fn [[k v]]
         (str
           (name k)
           "="
           v))
    params))

(defn- exec
  [^PersistentVector args]
  (.waitFor (.start (.inheritIO (ProcessBuilder. ^PersistentVector (remove nil? args))))))

(defn- ^LazySeq cons-param-over
  [^PersistentHashMap params]
  (if params
    (cons "--parameter-overrides"
      (str-eq-kv params))))

(defn- stack-name-rand-16-char
  [^String stack-name]
  (if stack-name
    (str
      stack-name
      "-"
      (rand-16-char))
    "hypervibe"))

(defn- cond-capab
  [^Keyword capab]
  (cond (= capab
          :CAPABILITY_IAM)
        (name capab)
        (= capab
          :CAPABILITY_NAMED_IAM)
        (name capab)))

(defn- ^PersistentVector pack-comm
  [^String s3-buck
   ^Boolean force-upl?
   ^String kms-key-id]
  (do
    (spit-json)
    ["aws"
     "cloudformation"
     "package"
     "--template-file"
     (str
       (dirs :targ)
       (files :hyper)
       (exten :json))
     "--s3-bucket" s3-buck
     "--s3-prefix" "jars"
     "--output-template-file"
     (str
       (dirs :targ)
       (files :hyper-pack)
       (exten :json))
     "--kms-key-id" kms-key-id
     "--use-json"
     (if (true? force-upl?)
       "--force-upload")]))

(defn- ^PersistentVector dep-comm
  [^String stack-name
   ^Keyword capab
   ^Boolean no-exec-chan?
   ^PersistentHashMap param-over]
  (into ["aws"
         "cloudformation"
         "deploy"
         "--template-file"
         (str
           (dirs :targ)
           (files :hyper-pack)
           (exten :json))
         "--stack-name"
         (stack-name-rand-16-char stack-name)
         "--capabilities"
         (cond-capab capab)
         (if (true? no-exec-chan?)
           "--no-execute-changeset")]
    (cons-param-over param-over)))

(defn package
  [& {:keys
      [s3-buck
       force-upl?
       kms-key-id]}]
  (if ((every-pred json-file-exist?)
        (str
          (dirs :targ)
          (files :hyper)
          (exten :json))
        (str
          (dirs :targ)
          (files :hyper-pack)
          (exten :json)))
    (exec (apply pack-comm
            [s3-buck
             force-upl?
             kms-key-id]))
    (throw
      (FileNotFoundException.
        "JSON template was unavailable
         when attempting to package
         artifact."))))

(defn deploy
  [& {:keys
      [stack-name
       capab
       no-exec-chan?
       param-over]}]
  (exec (apply dep-comm
          [stack-name
           capab
           no-exec-chan?
           param-over])))


