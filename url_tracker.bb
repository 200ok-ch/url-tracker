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
  url-tracker <config-file> [--dry-run] [--verbose]
  url-tracker [--url <url> --directory <directory> --prefix <prefix> --hook <command>] [--dry-run] [--verbose]
  url-tracker (-h | --help)

Options:
  -h --help                     Show this screen.
  <config-file>                YAML config file to process.
  --url <url>                  URL to fetch.
  --directory <directory>      Directory where snapshots are stored.
  --prefix <prefix>            Prefix used in snapshot filenames.
  --hook <command>             Command to run when a new version is detected.
  --dry-run                    Print actions without writing files.
  -v --verbose                 Print detailed progress information.

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

(defn run-command! [cmd env verbose]
  (when verbose
    (println "[verbose] Hook command:" cmd)
    (println "[verbose] Hook environment:"))
  (when verbose
    (doseq [k (sort (keys env))]
      (println "[verbose]" k "=" (get env k))))
  (let [{:keys [exit out err]} (process/sh {:extra-env env
                                            :continue true
                                            :out :string
                                            :err :string}
                                           "bash" "-lc" cmd)]
    (when-not (str/blank? out)
      (println out))
    (when-not (str/blank? err)
      (binding [*out* *err*]
        (println err)))
    (zero? exit)))

(defn required-target? [{:keys [url directory prefix]}]
  (and (some? url) (some? directory) (some? prefix)))

(def value-flags #{"--url" "--directory" "--prefix" "--hook"})

(def bool-flags #{"--dry-run" "--help" "-h" "--verbose" "-v"})

(defn positional-args [args]
  (loop [remaining args
         positional []]
    (if-let [arg (first remaining)]
      (cond
        (contains? value-flags arg) (recur (drop 2 remaining) positional)
        (contains? bool-flags arg) (recur (rest remaining) positional)
        (str/starts-with? arg "-") (recur (rest remaining) positional)
        :else (recur (rest remaining) (conj positional arg)))
      positional)))

(defn any-target-flag? [args]
  (boolean (some value-flags args)))

(defn read-config-file! [config-file]
  (let [path (fs/absolutize config-file)]
    (when-not (fs/exists? path)
      (throw (ex-info (str "Config file not found: " config-file)
                      {:config-file config-file})))
    (when-not (fs/regular-file? path)
      (throw (ex-info (str "Config path is not a file: " config-file)
                      {:config-file config-file})))
    (let [raw (smith/config-from-file (str path))
          cfg (normalize-config (or raw {}))
          config-dir (or (some-> path fs/parent str) ".")]
      (assoc cfg :config-file-dir config-dir))))

(defn log-verbose [verbose & parts]
  (when verbose
    (apply println "[verbose]" parts)))

(defn config-targets [{:keys [targets url directory prefix hook config-file-dir] :as cfg}]
  (let [from-config (cond
                      (nil? targets) []
                      (map? targets) [targets]
                      (sequential? targets) (vec targets)
                      :else [])
        from-config (mapv (fn [target]
                            (if (some? (:directory target))
                              target
                              (assoc target :directory (or directory config-file-dir))))
                          from-config)
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

(defn track-target! [{:keys [url directory prefix hook dry-run verbose]}]
  (let [dir (fs/path directory)
        latest-symlink (fs/path dir (str prefix "-latest.json"))
        latest-file (some-> (latest-target-file dir latest-symlink) str)
        tmp-file (fs/create-temp-file {:prefix "track-url-" :suffix ".json"})
        fetch-start (System/nanoTime)
        fetch-result (process/sh {:continue true :out :string :err :string}
                                 "curl" "-fsSL" url "-o" (str tmp-file))
        fetch-elapsed-ms (double (/ (- (System/nanoTime) fetch-start) 1000000.0))
        fetch-exit (:exit fetch-result)]
    (log-verbose verbose "Checking" url)
    (log-verbose verbose "Latest snapshot:" (or latest-file "none"))
    (log-verbose verbose "Fetch time:" (format "%.1f ms" fetch-elapsed-ms))
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
                 env {"URL" (str url)
                      "DIRECTORY" (str directory)
                      "PREFIX" (str prefix)
                      "OLD_FILE" (or old-file "")
                      "NEW_FILE" (str new-file)
                      "OLD_VERSION" (str latest-version)
                      "NEW_VERSION" (str new-version)}]
            (when-not dry-run
              (fs/create-dirs dir)
              (fs/move tmp-file new-file)
              (fs/delete-if-exists latest-symlink)
              (fs/create-sym-link latest-symlink (fs/file-name new-file)))
            (when dry-run
              (fs/delete-if-exists tmp-file))
            (if (and cmd (not (str/blank? cmd)) (not dry-run))
              (if (run-command! cmd env verbose)
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
  (let [cli-cfg (-> (smith/config usage :name "url-tracker" :defaults {:targets []})
                    normalize-config)
        positional (positional-args *command-line-args*)]
    (when (> (count positional) 1)
      (binding [*out* *err*]
        (println "Only one positional argument is supported: <config-file>."))
      (System/exit 1))
    (when (and (seq positional)
               (any-target-flag? *command-line-args*))
      (binding [*out* *err*]
        (println "Do not combine <config-file> with --url/--directory/--prefix/--hook."))
      (System/exit 1))
    (try
            (let [cfg (if-let [config-file (first positional)]
                  (merge (read-config-file! config-file)
                         (select-keys cli-cfg [:dry-run :help :verbose]))
                  cli-cfg)
            targets (config-targets cfg)
            dry-run (as-bool (:dry-run cfg))
            verbose (as-bool (:verbose cfg))]
        (when (as-bool (:help cfg))
          (println usage)
          (System/exit 0))
        (when (empty? targets)
          (binding [*out* *err*]
            (println "No targets configured. Use <config-file> or --url/--directory/--prefix, or define targets in url-tracker.yml."))
          (System/exit 1))
        (validate-targets! targets)
        (let [results (mapv #(track-target! (assoc % :dry-run dry-run :verbose verbose)) targets)
              errors (filter #(= :error (:status %)) results)]
          (doseq [result results]
            (case (:status result)
              :updated (println "Created new version:" (:file result))
              :unchanged (println "No change detected for" (:url result))
              :error (binding [*out* *err*]
                       (println "Error:" (:message result)))))
          (System/exit (if (seq errors) 1 0))))
      (catch Exception e
        (binding [*out* *err*]
          (println "Error:" (ex-message e)))
        (System/exit 1)))))

(-main)
