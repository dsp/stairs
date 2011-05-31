(ns #^{:doc "Stairs - Core functionality"
       :author "David Soria Parra"}
  stairs.core
  (:import java.io.File)
  (:use clojure.walk)
  (:use clojure.contrib.condition)
  (:require [clojure.xml :as xml]
            [clojure.contrib.json :as json]
            [clojure.contrib.trace :as t]))

; forward declarations
(defn render-snippet
  [context template userdata])
(defn render
  [context template userdata])

#_(defn trace
  [value]
  (binding [*out* *err*]
    (println "TRACE :" value))
  value)

(defn html-doctype
  "Returns a doctype for a given mode.

  Valid mods are :html5 :html :html4 :xhtml."
  [mode]
  (condp = mode
    :html5 "<!DCOTYPE html>"
    :html  "<!DCOTYPE html>"
    :html4 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
    :xhtml "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
    "<!DOCTYPE html>"))

(defmacro context
  [& kv]
  `(apply hash-map :namespace *ns* ~@kv))

(def render-agents (ref (hash-map)))
(defn- render-reference
  "Render a <stairs:reference fn='test' /> tag.

  It will try to resolve the given fn name and call it.
  Returns the function return value or raises a condition."
  [context function userdata]
  (if-let [resolved (ns-resolve (:namespace context) (symbol function))]
    (resolved userdata)
    (raise (str "cannot resolve " function))))

(defn- render-parallel
  "Takes a function and dispatches the calculation to a separate thread.

  All function have a unique symbol assigned. the disaptched agents that
  yield the function and the symbol are stored in the render-agents ref.
  Returns an accessor function."
  [f]
  (let [sym (gensym)
        agt (agent (fn [] sym))]
    (dosync
      (alter render-agents assoc sym agt))
    (send-off agt
      (fn [s]
        (let [res (f)]
          (fn []
            (dosync
              (alter render-agents dissoc sym))
            res))))
    (fn [] (@agt))))

(defn- parse-snippet
  "Parses an given filename as XML and resolves <stairs> tags in parallel.
  
  Returns a new document with accessor to the values of the bounded
  <stairs> tags."
  [context file userdata]
  (postwalk
    (fn [elem]
      (if (vector? elem)
        (let [e (first elem)
              fh (:file (:attrs e))
              fun (:function (:attrs e))]
          (condp = (:tag e)
            :stairs:include
              (render-parallel #(render-snippet context fh userdata))
            :stairs:refer
              (render-parallel #(render-reference context fun userdata))
            elem))
        elem))
    (xml/parse file)))

(defn form
  [fn])

(defn- call-or-ajax-callback
  [partial-fn]
  (let [result (partial-fn)]
    (vector
      (if (symbol? result)
        [(str "<div id=\"" result "\"></div><script type=\"text/javascript\">load(\"" result "\");</script>")]
        result))))

(defn- render-snippet
  [context template userdata]
  (let [filename 
        (str (:snippet-directory context) "/" template)]
    (parse-snippet context (File. filename) userdata)))

(defn render
  "Renders the given template in the given context."
  [context template userdata]
  (let [filename (str (:snippet-directory context) "/" template)
        html     (html-doctype (:doctype context))
        document (render-snippet context template userdata)]
    (apply await-for (:max-request-time context) (vals @render-agents))
    (with-out-str
      (println html)
      (xml/emit-element
        (postwalk
          (fn [elem] (if (fn? elem) (call-or-ajax-callback elem) elem))
          document)))))

(defn ajax-postloader
  [id]
  (if-let [agt (get @render-agents (symbol id))]
    (do 
      (await agt)
        (@agt))))
