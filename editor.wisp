(def js-null (eval "null"))
(let [
      key-class "key"
      val-class "value"
      
      ls-class "ls"
      kv-class "kv"

      null-class "null"
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
          is-null? (identical? obj js-null)
          container (if is-array?
                      ($ "<ol>" {:start 0
                                 :class ls-class})
                      (if is-null?
                        ($ "<div>" {:class null-class})
                        ($ "<div>" {:class kv-class})))]
      (.append parent container)
      (if obj
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
               (.append row (-> (if (identical? (typeof v) "string")
                                  ($ "<textarea>"
                                     {:class (+ val-class " " (typeof v))})
                                  ($ "<input>"
                                     {:type "text"
                                      :class (+ val-class " " (typeof v))}))
                                (.val v))))))))))

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
    ;; better off a cond but for now,
    ;; 1. if INPUT, return the typed value it contains
    ;; 2. if null, return null -- niche case
    ;; 3. process potential children
    (if (identical? (.prop $domel :tagName) "INPUT")
      ;; get value
      (if (.hasClass $domel "number")
        (parseInt (.val $domel))
        (.val $domel))
      
      (if (.hasClass $domel null-class)
        ;; niche case of null
        js-null

        ;; potential children
        (let [rtn (or rtn
                      ;; most likely one is the one with minimum distance
                      (-> [[(get-child-distance $domel ls-class) []]
                           [(get-child-distance $domel kv-class) {}]]
                          (.sort)
                          (aget 0)
                          (aget 1)))
              pushing-to-array? (Array.isArray rtn)]
          ;; reason we use loop here, is because when we hit an input that is a key,
          ;; we want to "store" the key and move on, but do it functionally
          (loop [el-arr (.children $domel)
                 key nil]
            (if (< 0 (aget el-arr "length"))
              (let [$el ($ (aget el-arr 0))
                    el-val (.val $el)
                    el-tag (.prop $el :tagName)]

                (if (identical? el-tag "INPUT")
                  
                  ;; TRUE
                  (if pushing-to-array?
                    
                    (.push rtn (form-to-json $el))
                    
                    ;; the first input to be hit during the loop,
                    ;; for a non-array, must be a key. we then
                    ;; expect the remainder to be exactly length 1
                    (let [next (aget el-arr 1)]
                      (set! (aget rtn el-val)
                            (form-to-json ($ next) nil))))

                  ;; ELSE
                  ;; not input, traverse deeper
                  (let []
                    ;; another ugly 3-way cond replacement;
                    ;; we're handling 2 cases within push to array,
                    ;; that of null, vs that of kv. TODO: beautify
                    (if (and pushing-to-array?
                             (.hasClass $el null-class))
                      (.push rtn (form-to-json $el js-null))
                      (if (and pushing-to-array?
                               (.hasClass $el kv-class))
                        ;; TRUE
                        ;; next is a member of the list, and the member is an object
                        (.push rtn (form-to-json $el nil))
                        ;; next is an object at the same level
                        ;; ELSE
                        (form-to-json $el rtn)))
                    (recur (.slice el-arr 1) key))))))
          rtn))))
  
  ((fn [$]
     
     (set! (aget $.fn "json_editor")
           (fn [opt]
             (let [
                   frm ($ "<form>")
                   
                   is-custom-text? (and opt (aget opt "text"))

                   ta (if is-custom-text?
                        ($ (aget opt "text"))
                        ($ "<textarea>" {:class "editor"}))
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
                            "x" [1 2 3 4]
                            "None" js-null
                            :emptylist []
                            :emptyobj {}
                            :listwithnull [js-null js-null]
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
