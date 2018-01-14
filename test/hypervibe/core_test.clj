(ns hypervibe.core-test
  (:require [clojure.test :refer :all]
            [hypervibe.core :as hyper]
            [clojure.java.io :as io])
  (:import (java.io File)))

;TODO use spec for tests

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
    (is (nil? (hyper/edn-pref? nil)))
    (is (nil? (hyper/edn-pref? (io/file nil))))
    (is (true? (hyper/edn-pref? (io/file hyper-edn-file))))
    (is (boolean? (hyper/edn-pref? (io/file hyper-edn-file))))))



