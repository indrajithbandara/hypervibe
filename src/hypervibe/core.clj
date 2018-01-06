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

;TODO Parse AWS response response from package and deploy

(def ^:const ^String targ-dir "target")
(def ^:const ^String hyper-edn "hypervibe.edn")
(def ^:const ^String hyper-json "hypervibe.json")
(def ^:const ^String hyper-pack-json "hypervibe-packaged.json")
(def ^:const ^String hyper-targ-json (str targ-dir "/" hyper-json))
(def ^:const ^String hyper-targ-pack-json (str targ-dir "/" hyper-pack-json))

(defn ^Boolean edn-pref?
  [^File edn-file]
  (.endsWith (.getAbsolutePath edn-file) ".edn"))

(defn ^Boolean file-exist?
  [^File file]
  (.exists (.getAbsoluteFile file)))

(defn ^Boolean edn-file-exist?
  [^File edn-file]
  (and (file-exist? edn-file)
    (edn-pref? edn-file)))

(defn ^Boolean json-pref?
  [^File json-file]
  (.endsWith (.getAbsolutePath json-file) ".json"))

(defn ^Boolean json-file-exist?
  [^File json-file]
  (and (file-exist? json-file)
    (json-pref? json-file)))

(defn ^Boolean edn-file-exist?
  [^File edn-file]
  (and (file-exist? edn-file)
    (edn-pref? edn-file)))

(defn ^PersistentHashMap slurp-edn
  [^File edn-file]
  (if (edn-file-exist? edn-file)
    (try (edn/read-string
           (slurp (.getAbsolutePath edn-file)))
         (catch Exception _))))

(defn- ^String rand-16-char
  []
  (string/upper-case (Long/toHexString (Double/doubleToLongBits (.nextLong (Random.))))))

(defn ^String edn->json
  [^File edn-file]
  (if-let [edn (slurp-edn edn-file)]
    (try (cheshire/generate-string edn
           {:pretty true})
         (catch Exception _))))

(defn ^File spit-json
  []
  (let [hyper-targ-json-file (File. hyper-targ-json)]
    (try (do
           (spit hyper-targ-json-file
             (edn->json (File. hyper-edn)))
           (.getAbsoluteFile hyper-targ-json-file))
         (catch IOException _))))

(defn- str-eq-kv
  [^PersistentHashMap params]
  (map (fn [[k v]]
         (str (name k) "=" v)) params))

(defn- exec
  [^PersistentVector args]
  (.waitFor (.start (.inheritIO (ProcessBuilder. ^PersistentVector (remove nil? args))))))

(defn- apply-sh
  [^PersistentVector aws-cli-args]
  (apply shell/sh (remove nil? aws-cli-args)))

(defn- ^LazySeq cons-param-over [^PersistentHashMap params]
  (if params (cons "--parameter-overrides" (str-eq-kv params))))

(defn- stack-name-rand-16-char
  [^String stack-name]
  (if stack-name (str stack-name "-" (rand-16-char)) "hypervibe"))

(defn- cond-capab
  [^Keyword capab]
  (cond (= capab :CAPABILITY_IAM) (name capab)
        (= capab :CAPABILITY_NAMED_IAM) (name capab)))

(defn- split-out-info
  [^PersistentHashMap out-info]
  (update out-info :out #(string/split % #"\r")))

(defn- packaged?
  [^PersistentHashMap out-info]
  (if (zero? (:out out-info)) true false))

(defn- ^PersistentVector pack-comm
  [^String s3-buck ^Boolean force-uplo? ^String kms-key-id]
  (if-let [targ-abs-path ^File (spit-json)]
    ["aws"
     "cloudformation"
     "package"
     "--template-file" (.getAbsolutePath targ-abs-path)
     "--s3-bucket" s3-buck
     "--s3-prefix" "jars"
     "--output-template-file" hyper-targ-pack-json
     "--kms-key-id" kms-key-id
     "--use-json"
     (if (true? force-uplo?) "--force-upload")]
    (throw (IOException. "Error creating template JSON template file"))))

(defn- ^PersistentVector dep-comm
  [^String stack-name ^Keyword capab ^Boolean no-exec-chan?
   ^PersistentHashMap param-over]
  (into ["aws"
         "cloudformation"
         "deploy"
         "--template-file" hyper-targ-pack-json
         "--stack-name" (stack-name-rand-16-char stack-name)
         "--capabilities" (cond-capab capab)
         (if (true? no-exec-chan?) "--no-execute-changeset")]
    (cons-param-over param-over)))

(defn ^PersistentHashMap package
  [& {:keys [s3-bucket force-upload? kms-key-id]}]
  (split-out-info (apply-sh (pack-comm s3-bucket force-upload? kms-key-id))))

(defn ^PersistentHashMap -package
  [& {:keys [s3-bucket force-upload? kms-key-id]}]
  (exec (pack-comm s3-bucket force-upload? kms-key-id)))

(defn ^PersistentHashMap deploy
  [& {:keys [stack-name capabilities no-execute-changeset?
             parameter-overrides]}]
  (split-out-info (apply-sh (dep-comm stack-name capabilities no-execute-changeset?
                              parameter-overrides))))


