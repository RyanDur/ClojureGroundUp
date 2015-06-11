(ns scratch.crime
  (:require [cheshire.core :as json]))

(defn load-json
  "Given a filename, reads a JSON file and returns it, parsed, with keywords."
  [file]
  (json/parse-string (slurp file) true))

(def fips
  "A map of FIPS codes to their county names."
  (->> "FipsCountyCodes.json"
       load-json
       :table
       :rows
       (into {})))

(defn fips-code
  "Given a county (a map with :fips_state_code and :fips_county_code keys), returns the five-digit FIPS code for the county, as a string."
  [county]
  (str (:fips_state_code county) (:fips_county_code county)))

(defn most-duis
  "Given a JSON filename of UCR crime data for a particular year, finds the counties with the most DUIs."
  [file]
  (most-prevalent :driving_under_influence file))


(defn most-prevalent
  [crime file]
  (->> file
       load-json
       (filter #(not (zero? (:county_population %))))
       (map #(assoc % :prevalence (/ (crime %) (:county_population %))))
       (sort-by :prevalence)
       (take-last 10)
       (map (fn [county]
              [(fips (fips-code county))
               [(:prevalence county)
                (crime county)
                (:county_population county)]]))
       (into {})))
