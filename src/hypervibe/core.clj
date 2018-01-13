(ns hypervibe.core
  (:require [clojure.java.shell :as shell]
            [cheshire.core :as cc]
            [clojure.java.io :as io]
            [clojure.string :as cs]
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

(defn ^Boolean edn-pref?
  [edn-file]
  (.endsWith (.getAbsolutePath edn-file)
    ".edn"))

(defn- ^Boolean file-exist?
  [file]
  (.exists (.getAbsoluteFile file)))

(defn- ^Boolean edn-file-exist?
  [edn-file]
  (and (file-exist? edn-file)
    (edn-pref? edn-file)))

(defn- ^Boolean json-pref?
  [json-file]
  (.endsWith (.getAbsolutePath json-file)
    ".json"))

(defn- ^PersistentVector json-file-exist?
  [json-file]
  (and (file-exist? json-file)
    (json-pref? json-file)))

(defn- ^Boolean edn-file-exist?
  [edn-file]
  (and (file-exist? edn-file)
    (edn-pref? edn-file)))

(defn- ^PersistentHashMap slurp-edn
  [edn-file]
  (if (edn-file-exist? edn-file)
    (try (edn/read-string
           (slurp (.getAbsolutePath edn-file)))
         (catch Exception
                _))))

(defn- ^String rand-16-char
  []
  (cs/upper-case (Long/toHexString
                   (Double/doubleToLongBits
                     (.nextLong (Random.))))))

(defn- ^String edn->json
  [edn-file]
  (if-let [edn
           (slurp-edn edn-file)]
    (try (cc/generate-string edn
           {:pretty true})
         (catch Exception
                _))))

(defn- ^File spit-json
  []
  (let [hyper-json
        (str (dirs :targ)
          (files :hyper)
          (exten :json))]
    (try (do
           (spit (File. hyper-json)
             (edn->json (File. (str (files :hyper)
                                 (exten :edn)))))
           (.getAbsoluteFile (File. hyper-json)))
         (catch IOException
                _))))

(defn- str-eq-kv
  [^PersistentHashMap params]
  (map (fn [[k v]]
         (str (name k)
           "="
           v))
    params))

(defn- exec
  [args]
  (..
    (ProcessBuilder. (remove nil? args))
    inheritIO
    start
    waitFor))

(defn- ^LazySeq cons-param-over
  [params]
  (if params
    (cons "--parameter-overrides"
      (str-eq-kv params))))

(defn- ^String stack-name-rand-16-char
  [stack-name]
  (if stack-name
    (str stack-name
      "-"
      (rand-16-char))
    "hypervibe"))

(defn- cond-capab
  [capab]
  (cond (= capab
          :CAPABILITY_IAM)
        (name capab)
        (= capab
          :CAPABILITY_NAMED_IAM)
        (name capab)))

(defn- ^PersistentVector pack-comm
  [s3-buck force-upl? kms-key-id]
  ["aws" "cloudformation" "package"
   "--template-file"
   (str (dirs :targ)
     (files :hyper)
     (exten :json))
   "--s3-bucket" s3-buck
   "--s3-prefix"
   "hypervibe"
   "--output-template-file"
   (str (dirs :targ)
     (files :hyper-pack)
     (exten :json))
   "--kms-key-id" kms-key-id
   "--use-json"
   (if (true? force-upl?)
     "--force-upload")])

(defn- ^PersistentVector dep-comm
  [stack-name capab no-exec-chan?
   param-over]
  (into ["aws" "cloudformation" "deploy"
         "--template-file"
         (str (dirs :targ)
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
  [& {:keys [s3-buck force-upl? kms-key-id]}]
  (if (json-file-exist? (spit-json))
    {:status (exec (pack-comm s3-buck force-upl?
                     kms-key-id))}
    (throw (FileNotFoundException.
             (str "file: "
               (str (dirs :targ)
                 (files :hyper)
                 (exten :json))
               " cannot be found")))))

(defn deploy
  [& {:keys [stack-name capab no-exec-chan?
             param-over]}]
  (let [hyper-json-pack
        (str (dirs :targ)
          (files :hyper-pack)
          (exten :json))]                                   ;;Todo check if jar exists
    (if (json-file-exist? hyper-json-pack)
      (exec (dep-comm stack-name capab
              no-exec-chan?
              param-over))
      (throw (FileNotFoundException.
               (str "file:"
                 hyper-json-pack
                 " cannot be found"))))))


