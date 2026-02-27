#!/usr/bin/env bb

(ns url-tracker
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [shell-smith.core :as smith])
  (:import (java.nio.file Files)
           (java.time ZoneOffset ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.util Arrays)
           (java.util.regex Pattern)))

(def usage
  "Track URL content snapshots.

Usage:
  url-tracker [--url <url> --directory <directory> --prefix <prefix> --hook <command>] [--dry-run]
  url-tracker (-h | --help)

Options:
  -h --help                     Show this screen.
  --url <url>                  URL to fetch.
  --directory <directory>      Directory where snapshots are stored.
  --prefix <prefix>            Prefix used in snapshot filenames.
  --hook <command>             Command to run when a new version is detected.
  --dry-run                    Print actions without writing files.

Configuration file (url-tracker.yml) supports multiple targets:
  targets:
    - url: https://example.com/a.json
      directory: snapshots/a
      prefix: a
      hook: ./notify.sh
    - url: https://example.com/b.json
      directory: snapshots/b
      prefix: b

")

(def utc-stamp-format (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss'Z'"))

(defn utc-stamp []
  (.format utc-stamp-format (ZonedDateTime/now ZoneOffset/UTC)))

(defn normalize-key [k]
  (if (keyword? k)
    (-> k name (str/replace "_" "-") keyword)
    k))

(defn normalize-config [x]
  (cond
    (map? x) (into {} (map (fn [[k v]] [(normalize-key k) (normalize-config v)])) x)
    (sequential? x) (mapv normalize-config x)
    :else x))

(defn as-bool [v]
  (cond
    (boolean? v) v
    (string? v) (let [s (-> v str/lower-case str/trim)]
                  (cond
                    (contains? #{"1" "true" "yes" "on"} s) true
                    (contains? #{"0" "false" "no" "off"} s) false
                    :else false))
    :else false))

(defn parse-version [prefix latest-file]
  (let [re (re-pattern (str "^"
                            (Pattern/quote prefix)
                            "-.*-v(\\d+)\\.json$"))
        match (re-matches re latest-file)]
    (if match
      (Long/parseLong (second match))
      0)))

(defn file-bytes [path]
  (Files/readAllBytes (fs/path path)))

(defn files-equal? [a b]
  (Arrays/equals (file-bytes a) (file-bytes b)))

(defn latest-target-version [latest-symlink prefix]
  (if (and (fs/exists? latest-symlink) (fs/sym-link? latest-symlink))
    (let [latest-file (-> latest-symlink fs/read-link fs/file-name str)]
      (parse-version prefix latest-file))
    0))

(defn latest-target-file [directory latest-symlink]
  (when (and (fs/exists? latest-symlink) (fs/sym-link? latest-symlink))
    (let [link-target (fs/read-link latest-symlink)]
      (str (if (fs/absolute? link-target)
             link-target
             (fs/path directory link-target))))))

(defn run-command! [cmd env]
  (let [{:keys [exit out err]} (process/shell {:extra-env env
                                               :continue true
                                               :out :string
                                               :err :string}
                                              cmd)]
    (when-not (str/blank? out)
      (println out))
    (when-not (str/blank? err)
      (binding [*out* *err*]
        (println err)))
    (zero? exit)))

(defn required-target? [{:keys [url directory prefix]}]
  (and (some? url) (some? directory) (some? prefix)))

(defn config-targets [{:keys [targets url directory prefix hook] :as cfg}]
  (let [from-config (cond
                      (nil? targets) []
                      (map? targets) [targets]
                      (sequential? targets) (vec targets)
                      :else [])
        from-cli (when (required-target? cfg)
                   [{:url url
                     :directory directory
                     :prefix prefix
                     :hook hook}])]
    (vec (concat from-config from-cli))))

(defn validate-targets! [targets]
  (doseq [[idx target] (map-indexed vector targets)]
    (when-not (required-target? target)
      (throw (ex-info (str "Invalid target at index " idx ": url, directory, and prefix are required")
                      {:target target :index idx})))))

(defn track-target! [{:keys [url directory prefix hook dry-run]}]
  (let [dir (fs/path directory)
        latest-symlink (fs/path dir (str prefix "-latest.json"))
        tmp-file (fs/create-temp-file {:prefix "track-url-" :suffix ".json"})
        fetch-result (process/sh {:continue true :out :string :err :string}
                                 "curl" "-fsSL" url "-o" (str tmp-file))
        fetch-exit (:exit fetch-result)]
    (if-not (zero? fetch-exit)
      (do
        (fs/delete-if-exists tmp-file)
        {:status :error
         :message (str "Failed to fetch " url)})
      (let [latest-version (latest-target-version latest-symlink prefix)
            old-file (latest-target-file dir latest-symlink)
            changed? (not (and (fs/exists? latest-symlink)
                               (files-equal? tmp-file latest-symlink)))]
        (if-not changed?
          (do
            (fs/delete-if-exists tmp-file)
            {:status :unchanged
             :url url
             :prefix prefix})
          (let [new-version (inc latest-version)
                new-file (fs/path dir (str prefix "-" (utc-stamp) "-v" new-version ".json"))
                cmd hook
                env {"TRACK_URL_URL" (str url)
                     "TRACK_URL_DIRECTORY" (str directory)
                     "TRACK_URL_PREFIX" (str prefix)
                     "OLD" (or old-file "")
                     "NEW" (str new-file)
                     "TRACK_URL_NEW_FILE" (str new-file)
                     "TRACK_URL_NEW_VERSION" (str new-version)}]
            (when-not dry-run
              (fs/create-dirs dir)
              (fs/move tmp-file new-file)
              (fs/delete-if-exists latest-symlink)
              (fs/create-sym-link latest-symlink (fs/file-name new-file)))
            (when dry-run
              (fs/delete-if-exists tmp-file))
            (if (and cmd (not (str/blank? cmd)) (not dry-run))
              (if (run-command! cmd env)
                {:status :updated
                 :url url
                 :file (str new-file)
                 :version new-version}
                {:status :error
                 :message (str "Post-update command failed for " url)
                 :file (str new-file)})
              {:status :updated
               :url url
               :file (str new-file)
               :version new-version})))))))

(defn -main []
  (let [cfg (-> (smith/config usage :name "url-tracker" :defaults {:targets []})
                normalize-config)
        targets (config-targets cfg)
        dry-run (as-bool (:dry-run cfg))]
    (when (as-bool (:help cfg))
      (println usage)
      (System/exit 0))
    (when (empty? targets)
      (binding [*out* *err*]
        (println "No targets configured. Use --url/--directory/--prefix or define targets in url-tracker.yml."))
      (System/exit 1))
    (validate-targets! targets)
    (let [results (mapv #(track-target! (assoc % :dry-run dry-run)) targets)
          errors (filter #(= :error (:status %)) results)]
      (doseq [result results]
        (case (:status result)
          :updated (println "Created new version:" (:file result))
          :unchanged (println "No change detected for" (:url result))
          :error (binding [*out* *err*]
                   (println "Error:" (:message result)))))
      (System/exit (if (seq errors) 1 0)))))

(-main)
