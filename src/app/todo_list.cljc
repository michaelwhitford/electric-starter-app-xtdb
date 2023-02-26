(ns app.todo-list
  
  ; trick shadow into ensuring that client/server always have the same version
  ; all .cljc files containing Electric code must have this line!
  #?(:cljs (:require-macros app.todo-list)) ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  (:require #?(:clj [datascript.core :as d]) ; datascript on server
            #?(:clj [xtdb.api :as xt]) ; xtdb on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]))

; connect to stand-alone xtdb node
(def !node #?(:clj (xt/new-api-client "http://localhost:3030") :cljs nil))
; injected db ref - always dynamic
(e/def db)

(defn tx-flow
  "Return a discrete flow of events from XTDB's event bus."
  []
  #?(:clj (m/observe (fn [!]
                       (let [listener (xt/listen !node {:xtdb.api/event-type :xtdb.api/indexed-tx :with-tx-ops? true} !)]
                         ;; Called with no args when observe cancels
                         #(.close listener))))))
(e/defn LatestTx
  "A continous flow of the latest transaction on this xtdb node.
  Starts with the result of `xt/latest-completed-tx`, then with successive
  transactions seen on the event bus."
  []
  (->> (tx-flow)
       (m/reductions {} (xt/latest-completed-tx !node)) ; intial value is the latest known tx
       (m/relieve {})
       (new) ; bring missionary flow into Photon land
       ))

(e/defn TodoItem [id]
  (e/server
     (let [e           (e/wrap (xt/entity db id))
           status      (:task/status e)
           description (:task/description e)]
       (e/client
        (dom/div
         (ui/checkbox
          (case status :active false, :done true)
          (e/fn [v]
            (e/server
              (e/wrap (xt/submit-tx !node [[:xtdb.api/put
                                            {:xt/id            id
                                             :task/description description
                                             :task/status      (if v :done :active)}]])
                      (reset! tx-time (xt/sync !node)))))))

        (dom/props {:id id})
        (dom/label (dom/props {:for id}) (dom/text (e/server description)))))))

(e/defn InputSubmit [F]
  ; Custom input control using lower dom interface for Enter handling
  (dom/input (dom/props {:placeholder "Buy milk"})
             (dom/on "keydown" (e/fn [e]
                                 (when (= "Enter" (.-key e))
                                   (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                                     (new F v)
                                     (set! (.-value dom/node) "")))))))

(e/defn TodoCreate []
  (e/client
   (InputSubmit. (e/fn [v]
                   (e/server
                    (e/wrap (xt/submit-tx !node [[:xtdb.api/put
                                                  {:xt/id            #?(:clj (java.util.UUID/randomUUID)
                                                                        :cljs (random-uuid))
                                                   :task/description v
                                                   :task/status      :active}]])
                            (reset! tx-time (xt/sync !node))))))))

#?(:clj (defn todo-count [db]
          (count
           (xt/q db '{:find  [e]
                      :where [[e :task/status :active]]}))))

#?(:clj (defn todo-records [db]
          (->> (xt/q db '{:find  [(pull e [*])]
                          :where [[e :task/status]]})
               seq
               flatten
               (sort-by :task/description))))

#?(:clj (def tx-time (atom nil)))

(e/defn Todo-list []
  (e/server
  (let [tx (e/watch tx-time)]
  (binding [db (if tx
                 (xt/db !node {:xtdb.api/tx-time tx})
                 (xt/db !node))]
   (let [todos       (e/wrap (todo-records db))
         todos-count (e/wrap (todo-count db))]
     (e/client
      (dom/link (dom/props {:rel  :stylesheet
                            :href "/todo-list.css"}))
      (dom/h1 (dom/text "minimal todo list"))
      (dom/p (dom/text "it's multiplayer, try two tabs"))
      (dom/div (dom/props {:class "todo-list"})
               (TodoCreate.)
               (dom/div {:class "todo-items"}
                        (e/server
                         (e/for-by :xt/id [{:keys [xt/id]} todos]
                                   (TodoItem. id))))
               (dom/p (dom/props {:class "counter"})
                      (dom/span (dom/props {:class "count"}) (dom/text todos-count)) 
                      (dom/text " items left")))))))))

(comment
  (type !node)
  (def d (xt/db !node))
  (xt/q d '{:find  [e]
            :where [[e :xt/id]]})
  (xt/q d '{:find  [e]
            :where [[e :task/status :active]]})
  (xt/recent-queries !node)
  (xt/entity (xt/db !node) #uuid "4094058d-c82c-4974-b1d2-d1d54c6d5589")
  (let [docs (xt/q d '{:find  [(pull e [*])]
                       :where [[e :task/status]]})]
    docs)
  (todo-records d)
  (xt/sync !node)
  )
