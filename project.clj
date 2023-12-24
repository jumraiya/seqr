(defproject seqr "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "RELEASE"]
                 [com.github.igrishaev/virtuoso "0.1.0"]
                 [com.formdev/flatlaf "3.2.5"]
                 [com.github.flow-storm/flow-storm-dbg "RELEASE"]]
  :repl-options {:init-ns seqr.core}
  :aot [seqr.tui.ClipRenderer])
