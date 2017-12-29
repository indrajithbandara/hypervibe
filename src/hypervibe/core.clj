(ns hypervibe.core
  (:require [clojure.java.shell :as shell]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn])
  (:import (java.io FileNotFoundException File IOException)
           (java.net URL)
           (clojure.lang PersistentVector LazySeq PersistentHashMap)
           (java.util Random Vector)))

(def ^:private hyper-edn-file (File. "hypervibe.edn"))
(def ^:private hyper-json-file (File. "hypervibe-packaged.json"))

(defn- ^String rand-16-char []
  (string/upper-case (Long/toHexString (Double/doubleToLongBits (.nextLong (Random.))))))

(defn ^Boolean file-exi?
  [^File file]
  (.exists (.getAbsoluteFile file)))

(defn ^Boolean edn-pref?
  [^File file]
  (.endsWith (.getAbsolutePath file) ".edn"))

(defn ^Boolean json-pref?
  [^File file]
  (.endsWith (.getAbsolutePath file) ".json"))

(defn ^Boolean json-file-exi?
  [^File file]
  (and (file-exi? file)
    (json-pref? file)))

(defn ^Boolean edn-file-exi?
  [^File file]
  (and (file-exi? file)
    (edn-pref? file)))

(defn ^PersistentHashMap slurp-edn
  [^File file]
  (if (edn-file-exi? file)
    (try (edn/read-string
           (slurp (.getAbsolutePath file)))
         (catch Exception _))))

(defn ^String edn->json
  [^File file]
  (if-let [edn (slurp-edn file)]
    (try (cheshire/generate-string edn
           {:pretty true})
         (catch Exception _))))

(defn ^File spit-json
  [^File edn ^File json]
  (if (json-pref? json)
    (try (do (spit (.getAbsolutePath json)
               (edn->json edn))
             (.getAbsoluteFile json))
         (catch IOException _))))

(defn ^PersistentHashMap package
  [& {:keys [s3-bucket use-json? force-upload?
             kms-key-id]
      :or {use-json? true
           force-upload? false}}]
  (if-let [json (spit-json hyper-edn-file hyper-json-file)]
    (apply shell/sh
      (remove nil?
        ["aws"
         "cloudformation"
         "package"
         "--template-file" (.getAbsolutePath json)
         "--s3-bucket" s3-bucket
         "--s3-prefix" "jars"
         "--output-template-file" "template-packaged.json"
         "--kms-key-id" kms-key-id
         (if use-json? "--use-json")
         (if force-upload? "--force-upload")]))))

(defn- str-eq-kv
  [m]
  (map (fn [[k v]]
         (str (name k) "=" v)) m))

(defn- apply-sh
  [^PersistentVector pv]
  (apply shell/sh (remove nil? pv)))

(defn- ^LazySeq params-over-arg [parameter-overrides]
  (if parameter-overrides (cons "--parameter-overrides" (str-eq-kv parameter-overrides))))

(defn- stack-name-arg [stack-name]
  (if stack-name-arg
    (str stack-name "-" (rand-16-char))
    "hypervibe"))

(defn- capab-arg
  [capabilities]
  (cond :CAPABILITY_IAM "CAPABILITY_IAM"
        :CAPABILITY_NAMED_IAM "CAPABILITY_NAMED_IAM"))

(defn- ^PersistentVector dep-comm
  [stack-name capabilities no-execute-changeset?
   parameter-overrides]
  (into ["aws"
         "cloudformation"
         "deploy"
         "--template-file" "template-packaged.json"
         "--stack-name" (stack-name-arg stack-name)
         "--capabilities" (capab-arg capabilities)
         (if no-execute-changeset? "--no-execute-changeset")]
    (params-over-arg parameter-overrides)))

(defn- ^PersistentVector pack-comm
  [template-file s3-bucket use-json?
   force-upload? kms-key-id]
  ["aws"
   "cloudformation"
   "package"
   "--template-file" template-file
   "--s3-bucket" s3-bucket
   "--s3-prefix" "jars"
   "--output-template-file" "template-packaged.json"
   "--kms-key-id" kms-key-id
   (if use-json? "--use-json")
   (if force-upload? "--force-upload")])

(defn ^PersistentHashMap deploy
  [& {:keys [stack-name capabilities no-execute-changeset?
             parameter-overrides]}]
  (apply-sh (dep-comm stack-name capabilities no-execute-changeset?
              parameter-overrides)))
;TODO
;(defn package)


