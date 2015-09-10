;; (ns editor.main
;;   (:require [wisp.core]))

(let [
      key-class "key"
      val-class "value"
      
      ls-class "ls"
      kv-class "kv"
      ]
  (defmacro ->
    [& operations]
    (reduce
     (fn [form operation]
       (cons (first operation)
             (cons form (rest operation))))
     (first operations)
     (rest operations)))
  
  (defn gen-editor [parent obj]
    (let [is-array? (Array.isArray obj)
          container (if is-array?
                      ($ "<ol>" {:start 0
                                 :class ls-class})
                      ($ "<div>" {:class kv-class}))]
      ($.each
       obj
       (fn [k v]
         (let [row (if is-array?
                     (-> ($ "<li>"))
                     (-> ($ "<div>" {:class "item"})
                         (.append (-> ($ "<input>"
                                         {:type "text"
                                          :class key-class})
                                      (.val k)))))]
           (-> container
               (.append row))
           (if (identical? (typeof v) "object")
             (gen-editor row v)
             (.append row (-> ($ "<input>"
                                 {:type "text"
                                  :class (+ val-class " " (typeof v))})
                              (.val v))))
           (.append parent container))))))

  ;; don't know whether the full traversal is necessary
  ;; but $.closest() traverses up, and there doesn't seem
  ;; to be one that does the same in the opposite direction
  (defn get-child-distance [$parent cls]
    (if (.hasClass $parent cls)
      0
      (let [dist-ls []]
        (.each
         (.find $parent (+ "." cls))
         (fn [i el]
           (.push dist-ls (aget (.parentsUntil ($ el) $parent) "length"))))
        (Math.min.apply nil dist-ls))))

  (defn form-to-json [$domel rtn]
    (if (identical? (.prop $domel "tagName") "INPUT")
      (if (.hasClass $domel "number")
        (parseInt (.val $domel))
        (.val $domel))
      (let [
            rtn (or rtn
                    (let [ls-dist (get-child-distance $domel ls-class)
                          kv-dist (get-child-distance $domel kv-class)]
                      (if (< ls-dist kv-dist)
                        [] {})))
            pushing-to-array? (Array.isArray rtn)
            ]
        ;; reason we use loop here, is because when we hit an input that is a key,
        ;; we want to "store" the key and move on, but do it functionally
        (loop [el-arr (.children $domel)
               key nil]
          (if (< 0 (aget el-arr "length"))
            (let [$el ($ (aget el-arr 0))
                  el-val (.val $el)
                  el-tag (.prop $el "tagName")]

              (if (identical? el-tag "INPUT")
                
                ;; TRUE
                (if pushing-to-array?
                  
                  (.push rtn (form-to-json $el))
                  
                  ;; the first input to be hit during the loop,
                  ;; for a non-array, must be a key. we then
                  ;; expect the remainder to be exactly length 1
                  (set! (aget rtn el-val)
                        (form-to-json ($ (aget el-arr 1)) nil)))
                
                ;; ELSE
                ;; not input, traverse deeper
                (let []
                  (if (and pushing-to-array?
                           (.hasClass $el kv-class))
                    ;; TRUE
                    ;; next is a member of the list, and the member is an object
                    (.push rtn (form-to-json $el nil))
                    ;; next is an object at the same level
                    ;; ELSE
                    (form-to-json $el rtn))
                  (recur (.slice el-arr 1) key))))))
        rtn)))
  
  ((fn [$]
     
     (set! (aget $.fn "json_editor")
           (fn [opt]
             (let [
                   frm ($ "<form>")
                   
                   is-custom-text? (and opt (aget opt "text"))

                   ta (if is-custom-text?
                        ($ (aget opt "text"))
                        ($ "<textarea>"))
                   ]
               
               (if is-custom-text?
                 (let []
                   (.append ($ this)
                            frm))
                 (let [main-row (-> ($ "<div>" {:class "viewer-row"}))
                       main-view (-> ($ "<div>" {:class "viewer-main"})
                                     (.append main-row))
                       ]
                   ;; demo data
                   (.val ta
                         (JSON.stringify
                          ["one"
                           {
                            :a 1
                            :b {:foo "bar"
                                :baz "quux"}
                            1 "some number"
                            "x" [1 2 3]

                            "y" {
                                 :nested "map"

                                 "within" ["another array"
                                           "cool"
                                           {:yet "more"}
                                           1
                                           {:snowman ["five" 5 "f5"]}
                                           {:ice {:cream "cone"}}]
                                 }
                            }
                           "tow"]
                          nil 4))

                   (-> main-row
                       (.append (-> ($ "<div>" {:class "viewer-cell"})
                                    (.append ta)))
                       (.append (-> ($ "<div>" {:class "viewer-cell"})
                                    (.append frm))))
                   (-> this
                       (.empty)
                       (.append main-view))))
               
               (defn respit-json []
                 (let [final (form-to-json frm)]
                   (.val ta (JSON.stringify final nil 4))))
               
               (.change ta
                        (fn [_]
                          (let [text (.val ta)
                                json (JSON.parse text)]
                            (gen-editor (.empty frm) json)
                            (-> frm
                                (.find "input")
                                (.change respit-json)))))
               (.change ta)

               (respit-json)
               )

             ))
     )
   jQuery)
  )
