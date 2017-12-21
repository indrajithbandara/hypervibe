(ns hypervibe.core
  (:require [clojure.java.shell :as shell]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import (java.io FileNotFoundException File IOException)
           (java.net URL)
           (clojure.lang PersistentArrayMap)))

(def ^:const template "hypervibe.edn")
(def ^:const template-packaged "hypervibe.json")

(defonce file-template (File. template))
(defonce file-template-packaged (File. template-packaged))

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

(defn package
  [& {:keys [use-json? force-upload? kms-key-id]
      :or {kms-key-id "key" use-json? false force-upload? false}}]
  (if-let [json (spit-json file-template
                  file-template-packaged)]
    (shell/sh
      "aws"
      "cloudformation"
      "package"
      "--template-file" (.getAbsolutePath json)
      "--s3-bucket" "hypervibe"
      "--s3-prefix" "jars"
      "--output-template-file" template-packaged
      (if (true? use-json?) "--use-json")
      (if (true? force-upload?) "--force-upload")
      "--kms-key-id" kms-key-id)))

(defn deploy
  [& {:keys [capabilities stack-name no-execute-changeset]
      :or {capabilities "CAPABILITY_IAM" stack-name "hypervibe" no-execute-changeset false}}]
  (shell/sh
    "aws"
    "cloudformation"
    "deploy"
    "--template-file" template-packaged
    "--stack-name" stack-name
    "--capabilities" capabilities))

;TODO
(defn delete [])

