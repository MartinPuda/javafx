(ns javafx.core
  (:require [cljfx.api :as fx]
            [clojure.math :as m])
  (:import (javafx.scene.paint Color)
           (javafx.scene.input MouseEvent)))

(defn index+y-seq [discs]
  (let [init-y (- 290 (* (- discs 3) 25))]
    (->> (range init-y 500 25)
         (take discs)
         (map-indexed vector))))

(defn disc-seq [discs]
  (for [[index y] (index+y-seq discs)]
    {:x          100
     :y          y
     :index      index
     :view-order 0
     :draggable  false
     :stack      0}))

;stacks are numbered 0,1,2
(defn initial-state [discs]
  (-> (disc-seq discs)
      vec
      (assoc-in [0 :draggable] true)))

(defn make-state [discs]
  {:positions (initial-state discs)
   :discs     discs
   :moves     0})

(def *state*
  (atom (make-state 3)))

(defmulti event-handler :event/type)

(defn clamp [mn number mx]
  (cond (> number mx) mx
        (> mn number) mn
        :else number))

(defn y-limits [height]
  [(+ 0 (* 0.5 height))
   (- 350 (* 0.5 height))])

(defn x-limits [width]
  [(+ 0 (* 0.5 width))
   (- 500 (* 0.5 width))])

(defn x-y [fx-event]
  [(.getSceneX fx-event)
   (- (.getSceneY fx-event) 100)])

(defn stack-by-x [x]
  (cond
    (>= 175 x 0) 0
    (>= 325 x 175) 1
    :else 2))

(defn disable-item [item]
  (assoc item :draggable false))

(defn enable-item [item]
  (assoc item :draggable true))

(defn disable-drag []
  (swap! *state* update :positions #(mapv disable-item %)))

(defn enable-top-drag []
  (->> (group-by :stack (:positions @*state*))
       (run! (fn [[_ v]]
               (swap! *state* update-in [:positions (:index (first v))] enable-item)))))

(defn stack-items [stack]
  (filter #(= (:stack %) stack)
          (:positions @*state*)))

(defn indices-in-stack [stack]
  (->> (stack-items stack)
       (map :index)))

(defn min-index-in-stack [stack]
  (let [indices (indices-in-stack stack)]
    (if (seq indices)
      (apply min indices)
      7)))

(defn y-for-stack [stack]
  (let [discs-in-stack (count (stack-items stack))]
    (- 340 (* 25 discs-in-stack))))

(defn to-background [disc]
  (assoc disc :view-order 0))

(defmethod event-handler ::mouse-pressed [& {:keys [index]}]
  (swap! *state* update :positions #(mapv to-background %))
  (swap! *state* update-in [:positions index] assoc :view-order -1))

(defmethod event-handler ::mouse-dragged [& {:keys [index width height] :as e}]
  (let [fx-event ^MouseEvent (:fx/event e)
        [x y] (x-y fx-event)
        [x-left-limit x-right-limit] (x-limits width)
        [y-top-limit y-bottom-limit] (y-limits height)]
    (swap! *state* update-in [:positions index] assoc
           :x (clamp x-left-limit x x-right-limit)
           :y (clamp y-top-limit y y-bottom-limit))))

(defmethod event-handler ::mouse-released [& {:keys [index stack] :as event}]
  (let [fx-event ^MouseEvent (:fx/event event)
        [x _] (x-y fx-event)
        new-stack (stack-by-x x)
        legal-move? (and (not= stack new-stack)
                         (< index (min-index-in-stack new-stack)))]
    (swap! *state* update :moves (if legal-move? inc identity))
    (swap! *state* update-in [:positions index] assoc
           :stack (if legal-move? new-stack stack)
           :x (if legal-move?
                (cond
                  (>= 175 x 0) 100
                  (>= 325 x 175) 250
                  :else 400)
                ({0 100 1 250 2 400} stack))
           :y (if legal-move? (y-for-stack  new-stack)
                              (+ (y-for-stack stack) 25)))
    (disable-drag)
    (enable-top-drag)))

(defmethod event-handler :default [e]
  (prn e))

(def text
  {:fx/type  :text-flow
   :children [{:fx/type :text
               :text    "Tower of Hanoi "
               :style   "-fx-font: 50px Tahoma;
                -fx-fill: linear-gradient(from 0% 0% to 100% 200%, repeat, yellow 0%, orange 50%);
                -fx-background: gold;
                -fx-stroke: black;
                -fx-stroke-width: 1;"}]})

(defn disc [{:keys [position index disc-count]}]
  (let [width (+ 40 (* (inc index)
                       (- 30 (* 5 (- disc-count 3)))))
        height 25
        {:keys [x y view-order draggable stack]} position]
    (cond-> {:fx/type    :rectangle
             :arc-width  10
             :arc-height 10
             :stroke     Color/BLACK
             :view-order view-order
             :style      "-fx-fill: linear-gradient(from 0% 0% to 100% 200%, repeat, yellow 0%, orange 50%);"
             :width      width
             :height     height
             :x          (- x (* 0.5 width))
             :y          (- y (* 0.5 height))}
            draggable
            (assoc :on-mouse-pressed {:event/type ::mouse-pressed
                                      :index      index}
                   :on-mouse-dragged {:event/type ::mouse-dragged
                                      :index      index
                                      :width      width
                                      :height     height}
                   :on-mouse-released {:event/type ::mouse-released
                                       :index      index
                                       :width      width
                                       :height     height
                                       :stack      stack}))))

(def sticks
  (let [width 15]
    (for [x [100 250 400]]
      {:fx/type    :rectangle
      ; :stroke     Color/BLACK
       :fill       Color/BROWN
       :x          (- x (* 0.5 width))
       :y          155
       :arc-width  10
       :arc-height 10
       :width      width
       :height     200})))

(def static-elements
  (concat
    [
     {:fx/type :rectangle
      ;:fill    Color/WHITE
     ; :stroke Color/BLACK
      :style   "-fx-font: 50px Tahoma;
                -fx-fill: radial-gradient(center 50% 75%, radius 150%, rgb(104, 163, 193) 0%, rgb(23, 56, 99) 100%);"
      :x       0
      :y       0
      :width   500
      :height  400}
     {:fx/type :rectangle
      :fill    Color/BROWN
      :x       0
      :y       350
      :width   500
      :height  50}]
    sticks))

(defn controls [state]
  (let [discs (:discs state)]
    [{:fx/type :label
      :text    (str "Discs: " discs)}
     {:fx/type          :spinner
      :max-width        75
      :on-value-changed #(reset! *state* (make-state %))
      :value-factory    {:fx/type           :integer-spinner-value-factory
                         :amount-to-step-by 1
                         :min               3
                         :max               6
                         :value             discs}}
     {:fx/type :label
      :text    (str "Moves: " (:moves state))}
     {:fx/type   :button
      :on-action (fn [_] (reset! *state* (make-state discs)))
      :text      "Reset"}
     {:fx/type :label
      :text    (str "Minimum moves: " (dec (long (m/pow 2 discs))))}]))

(defn root [{:keys [state]}]
  {:fx/type    :stage
   :title      "JavaFX"
   :showing    true
   :resizable  false
   :min-width  500
    :max-width  500
   :min-height 540
   :max-height 540
   :scene      {:fx/type :scene
                :root    {:fx/type :v-box
                          :children
                                   [{:fx/type    :h-box
                                     :style      "-fx-background-color: whitesmoke;"
                                     :min-width  500
                                     :min-height 75
                                     :max-height 75
                                     :alignment  :center
                                     :children   [text]}
                                    {:fx/type    :h-box
                                     :style      "-fx-background-color: whitesmoke; -fx-border-color: black;"
                                     :spacing    25
                                     :min-width  500
                                     :min-height 25
                                     :max-height 25
                                     :alignment  :center
                                     :children   (controls state)}
                                    {:fx/type :h-box
                                     :style   "-fx-background-color: white; -fx-border-color: black;"
                                     :children
                                              [{:fx/type  :group
                                                :children (concat static-elements
                                                                  (map-indexed
                                                                    (fn [index position]
                                                                      {:fx/type    disc
                                                                       :index      index
                                                                       :position   position
                                                                       :disc-count (:discs state)})
                                                                    (:positions state)))}]}]}}})

(defn renderer []
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type root
                                     :state   state}))
    :opts {:fx.opt/map-event-handler event-handler}))

(defn -main [& {:keys [d] :or {d 3}}]
  (reset! *state* (make-state 3))
  (fx/mount-renderer *state* (renderer)))