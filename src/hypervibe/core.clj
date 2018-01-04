(ns hypervibe.core
  (:require [clojure.java.shell :as shell]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [com.rpl.specter :as s])
  (:import (java.io FileNotFoundException File IOException)
           (java.net URL)
           (clojure.lang PersistentVector LazySeq PersistentHashMap Keyword)
           (java.util Random)
           (java.nio.file StandardCopyOption Files)))

;TODO Parse AWS response response from package and deploy

(def ^:const ^String targ-dir "target")
(def ^:const ^String hyper-edn "hypervibe.edn")
(def ^:const ^String hyper-json "hypervibe.json")
(def ^:const ^String hyper-pack-json "hypervibe-packaged.json")
(def ^:const ^String hyper-targ-json (str targ-dir "/" hyper-json))
(def ^:const ^String hyper-targ-pack-json (str targ-dir "/" hyper-pack-json))

(defn ^Boolean edn-pref?
  [^File file]
  (.endsWith (.getAbsolutePath file) ".edn"))

(defn ^Boolean file-exist?
  [^File file]
  (.exists (.getAbsoluteFile file)))

(defn ^Boolean edn-file-exist?
  [^File file]
  (and (file-exist? file)
    (edn-pref? file)))

(defn ^Boolean json-pref?
  [^File file]
  (.endsWith (.getAbsolutePath file) ".json"))

(defn ^Boolean json-file-exist?
  [^File file]
  (and (file-exist? file)
    (json-pref? file)))

(defn ^Boolean edn-file-exist?
  [^File file]
  (and (file-exist? file)
    (edn-pref? file)))

(defn ^PersistentHashMap slurp-edn
  [^File file]
  (if (edn-file-exist? file)
    (try (edn/read-string
           (slurp (.getAbsolutePath file)))
         (catch Exception _))))

(defn- ^String rand-16-char
  []
  (string/upper-case (Long/toHexString (Double/doubleToLongBits (.nextLong (Random.))))))

(defn ^String edn->json
  [^File file]
  (if-let [edn (slurp-edn file)]
    (try (cheshire/generate-string edn
           {:pretty true})
         (catch Exception _))))

(defn ^File spit-json
  []
  (let [hyper-json-file (File. hyper-targ-json)]
    (try (spit hyper-json-file (edn->json (File. hyper-edn)))
         (catch IOException _))))

(defn- str-eq-kv
  [^PersistentHashMap pers-hash-map]
  (map (fn [[k v]]
         (str (name k) "=" v)) pers-hash-map))

(defn- apply-sh
  [^PersistentVector pers-vec]
  (apply shell/sh (remove nil? pers-vec)))

(defn- ^LazySeq param-over [^PersistentHashMap pers-hash-map]
  (if pers-hash-map (cons "--parameter-overrides" (str-eq-kv pers-hash-map))))

(defn- stack-name-rand-16-char
  [^String string]
  (if string (str string "-" (rand-16-char)) "hypervibe"))

(defn- capab
  [^Keyword keyword]
  (condp = keyword
    :CAPABILITY_IAM (name keyword)
    :CAPABILITY_NAMED_IAM (name keyword)))

(defn- ^PersistentVector pack-comm
  [^String s3-bucket ^Boolean force-upload? ^String kms-key-id]
  (do (spit-json)
      ["aws"
       "cloudformation"
       "package"
       "--template-file" hyper-targ-json
       "--s3-bucket" s3-bucket
       "--s3-prefix" "jars"
       "--output-template-file" hyper-targ-pack-json
       "--kms-key-id" kms-key-id
       "--use-json"
       (if (true? force-upload?) "--force-upload")]))

(defn- ^PersistentVector dep-comm
  [^String stack-name ^Keyword capabilities ^Boolean no-execute-changeset?
   ^PersistentHashMap parameter-overrides]
  (into ["aws"
         "cloudformation"
         "deploy"
         "--template-file" hyper-targ-pack-json
         "--stack-name" (stack-name-rand-16-char stack-name)
         "--capabilities" (capab capabilities)
         (if (true? no-execute-changeset?) "--no-execute-changeset")]
    (param-over parameter-overrides)))

(defn ^PersistentHashMap package
  [& {:keys [s3-bucket force-upload? kms-key-id]}]
  (apply-sh (pack-comm s3-bucket force-upload? kms-key-id)))

(defn ^PersistentHashMap deploy
  [& {:keys [stack-name capabilities no-execute-changeset?
             parameter-overrides]}]
  (apply-sh (dep-comm stack-name capabilities no-execute-changeset?
              parameter-overrides)))
