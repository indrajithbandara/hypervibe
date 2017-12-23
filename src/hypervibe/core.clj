(ns hypervibe.core
  (:require [clojure.java.shell :as shell]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn])
  (:import (java.io FileNotFoundException File IOException)
           (java.net URL)
           (clojure.lang PersistentArrayMap PersistentVector)
           (java.util Random Vector)))

(def file-template (File. "template.edn"))
(def file-template-packaged (File. "template-packaged.json"))

(defn- ^String rand16-char []
  (string/upper-case (Long/toHexString (Double/doubleToLongBits (.nextLong (Random.))))))

(defn ^Boolean file-exists?
  [^File file]
  (.exists (.getAbsoluteFile file)))

(defn ^Boolean edn-prefix?
  [^File file]
  (.endsWith (.getAbsolutePath file)
    ".edn"))

(defn ^Boolean json-prefix?
  [^File file]
  (.endsWith (.getAbsolutePath file)
    ".json"))

(defn ^Boolean json-file-exists?
  [^File file]
  (and (file-exists? file)
    (json-prefix? file)))

(defn ^Boolean edn-file-exists?
  [^File file]
  (and (file-exists? file)
    (edn-prefix? file)))

(defn ^PersistentArrayMap slurp-edn
  [^File file]
  (if (edn-file-exists? file)
    (try (edn/read-string
           (slurp (.getAbsolutePath file)))
         (catch Exception
                _))))

(defn ^String edn->json
  [^File file]
  (if-let [edn (slurp-edn file)]
    (try (cheshire/generate-string edn
           {:pretty true})
         (catch Exception
                _))))

(defn ^File spit-json
  [^File edn ^File json]
  (if (json-prefix? json)
    (try (do (spit (.getAbsolutePath json)
               (edn->json edn))
             (.getAbsoluteFile json))
         (catch IOException
                _))))

(def ^:const package-args "aws;cloudformation;package")

(def ^:private ^String template-file-args
  (if-let [json ^File (spit-json file-template file-template-packaged)]
    (str "--template-file" ";" (.getAbsolutePath json))
    (throw (Exception. "Error spitting JSON to file"))))

#_(defn package
    [& {:keys [s3-bucket use-json? force-upload? kms-key-id]
        :or {kms-key-id "" use-json? true force-upload? false}}]
    (apply shell/sh
      (remove nil?
        ["aws"
         "cloudformation"
         "package"
         "--template-file" (.getAbsolutePath (spit-json file-template file-template-packaged))
         "--s3-bucket" s3-bucket
         "--s3-prefix" "jars"
         "--output-template-file" "template-packaged.json"
         "--kms-key-id" kms-key-id
         (if use-json? "--use-json")
         (if force-upload? "--force-upload")])))


(defn package
  [& {:keys [s3-bucket use-json? force-upload? kms-key-id]
      :or {kms-key-id "" use-json? true force-upload? false}}]
  (if-let [json (spit-json file-template file-template-packaged)]
    (apply shell/sh
      (remove nil?
        (mapcat #(string/split % #";")
          ["aws;cloudformation;package"
           (str "--template-file" ";" (.getAbsolutePath json))
           (str "--s3-bucket" ";" s3-bucket)
           "--s3-prefix;jars"
           "--output-template-file;template-packaged.json"
           (str "--kms-key-id" ";" kms-key-id)
           (if use-json? "--use-json")
           (if force-upload? "--force-upload")])))))

(defn deploy
  [& {:keys [capabilities stack-name no-execute-changeset?]
      :or {capabilities "CAPABILITY_IAM" no-execute-changeset? false}}]
  (apply
    shell/sh
    (remove nil?
      ["aws"
       "cloudformation"
       "deploy"
       "--template-file" "template-packaged.json"
       "--stack-name" (str stack-name "-" (rand16-char))
       "--capabilities" capabilities
       (if no-execute-changeset? "--no-execute-changeset")])))

;TODO
(defn delete [])

