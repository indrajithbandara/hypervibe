(ns hypervibe.core-test
  (:require [clojure.test :refer :all]
            [hypervibe.core :as hyper])
  (:import (java.io File)))

(def ^:const ^String hyper-edn-file (str (:hyper hyper/files) (:edn hyper/exten)))

(deftest test-target-directory
  (testing "Hypervibe artifact target
            directory"
    (is (= (:targ hyper/dirs)
          "target/"))))

(deftest test-template-files
  (testing "Hypervibe template file"
    (is (= (:hyper hyper/files)
          "hypervibe")
      (= (:hyper-pack hyper/files)
        "hypervibe-packaged"))))

(deftest test-file-extensions
  (testing "Hypervibe template file
            extensions"
    (is (= (:edn hyper/exten)
          ".edn"))
    (is (= (:json hyper/exten)
          ".json"))))

(deftest test-edn-prefix
  (testing "Hypervibe test template
            has .edn prefix"
    (is (true? (hyper/edn-pref? (File. hyper-edn-file))))
    (is (boolean? (hyper/edn-pref? (File. hyper-edn-file))))))



