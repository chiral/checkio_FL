(ns checkio.words-finder
  (:use [clojure.set :only [intersection]]))

(defn- checkio [text, words]
  (let [lt (.toLowerCase text)
        r (-> text .length range)
        index (fn [w] 
                (filter #(.. lt (substring (first %)) (startsWith w))
                        (map vector r (iterate inc (.length w)))))
        span (mapcat index (.. words toLowerCase (split "\\s+")))
        spans (-> (fn [i] (set (filter #(some #{i} (apply range %)) span)))
                  (map r) vec)
        border #(and (not (empty? %1)) (empty? (intersection %1 %2)))
        tag #(if (border (get spans %1) (get spans (+ %1 %2))) %3)
        trans (fn [i,c] (str (tag i -1 "<span>") c (tag i +1 "</span>")))]
    (apply str (map #(apply trans %) (map vector r text)))))

(defn- my-assert [info args expected]
  (let [ans (apply checkio args)]
        (if (not (= ans expected))
          (do
            (println (str "Failed case: " info))
            (println ans)))))

(defn -main [] 
  (do
    (my-assert 1 ["This is only a text example for task example." "example"] 
            "This is only a text <span>example</span> for task <span>example</span>.")

    (my-assert 2 ["Python is a widely used high-level programming language.", "pyThoN"]
               "<span>Python</span> is a widely used high-level programming language.")

    (my-assert 3 ["It is experiment for control groups with similar distributions.", "is im"]
               (str "It <span>is</span> exper<span>im</span>ent"
                    " for control groups with s<span>im</span>ilar d<span>is</span>tributions."))

    (my-assert 4 ["The National Aeronautics and Space Administration (NASA).", "nasa  THE"]
               "<span>The</span> National Aeronautics and Space Administration (<span>NASA</span>).")

    (my-assert 5 ["Did you find anything?", "word space tree"]
               "Did you find anything?")

    (my-assert 6 ["Hello World! Or LOL", "hell world or lo"]
               "<span>Hello</span> <span>World</span>! <span>Or</span> <span>LO</span>L")

    (println "done.")))

