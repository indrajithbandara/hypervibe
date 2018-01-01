(ns hypervibe.core
  (:require [clojure.java.shell :as shell]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [com.rpl.specter :as s])
  (:import (java.io FileNotFoundException File IOException)
           (java.net URL)
           (clojure.lang PersistentVector LazySeq PersistentHashMap)
           (java.util Random)
           (java.nio.file StandardCopyOption Files)))

(def ^:const ^String hyper-edn "hypervibe.edn")
(def ^:const ^String hyper-json ".hypervibe/hypervibe.json")
(def ^:const ^String hyper-pack-json ".hypervibe/hypervibe-packaged.json")

;TODO read in hypervibe-test.edn, get packaged jar paths, move to .hypervibe


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

(defn ^PersistentHashMap slurp-edn
  [^File file]
  (if (edn-file-exist? file)
    (try (edn/read-string
           (slurp (.getAbsolutePath file)))
         (catch Exception _))))

(defn ^PersistentVector sel-code-uri
  [^PersistentHashMap phm]
  (s/select [:Resources s/MAP-VALS :Properties :CodeUri] phm))

(defn copy-jars
  [^PersistentVector pv]
  (map #(Files/copy % ".hypervibe/" StandardCopyOption/ATOMIC_MOVE)
    (into #{} (remove nil? pv))))

(defn- ^String rand-16-char
  []
  (string/upper-case (Long/toHexString (Double/doubleToLongBits (.nextLong (Random.))))))

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

(defn ^String edn->json
  [^File file]
  (if-let [edn (slurp-edn file)]
    (try (cheshire/generate-string edn
           {:pretty true})
         (catch Exception _))))

(defn ^File spit-json
  []
  (let [hyper-json-file (File. hyper-json)]
    (try (do (spit (.getAbsolutePath hyper-json-file)
               (edn->json (File. hyper-edn)))
             (.getAbsoluteFile hyper-json-file))
         (catch IOException _))))

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
  (if stack-name-arg (str stack-name "-" (rand-16-char)) "hypervibe"))

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
         "--template-file" hyper-pack-json
         "--stack-name" (stack-name-arg stack-name)
         "--capabilities" (capab-arg capabilities)
         (if (true? no-execute-changeset?) "--no-execute-changeset")]
    (params-over-arg parameter-overrides)))

(defn- ^PersistentVector pack-comm
  [s3-bucket use-json?
   force-upload? kms-key-id]
  (do (spit-json)
      ["aws"
       "cloudformation"
       "package"
       "--template-file" hyper-json
       "--s3-bucket" s3-bucket
       "--s3-prefix" "jars"
       "--output-template-file" hyper-pack-json
       "--kms-key-id" kms-key-id
       (if (true? use-json?) "--use-json")
       (if (true? force-upload?) "--force-upload")]))

(defn ^PersistentHashMap package
  [& {:keys [s3-bucket use-json? force-upload?
             kms-key-id]}]
  (apply-sh (pack-comm s3-bucket use-json? force-upload?
              kms-key-id)))

(defn ^PersistentHashMap deploy
  [& {:keys [stack-name capabilities no-execute-changeset?
             parameter-overrides]}]
  (apply-sh (dep-comm stack-name capabilities no-execute-changeset?
              parameter-overrides)))

;TODO
;(defn package)


