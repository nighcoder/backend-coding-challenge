(defproject backend-coding-challenge "0.0.1"
  :description "REST API for finding North American cities"
  :url "https://github.com/nighcoder/backend-coding-challenge"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [http-kit "2.3.0"]
                 [compojure "1.6.1"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.json "0.2.7"]
                 [ring/ring-defaults "0.3.2"]]
  :main ^:skip-aot backend-coding-challenge.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
