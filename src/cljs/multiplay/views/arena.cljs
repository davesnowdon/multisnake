(ns multiplay.views.arena
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async
             :refer [chan sliding-buffer alts! >! <! timeout close!]]
            [multiplay.utils :refer [log]]
            [multiplay.game.params :as game-params]
            [goog.events]
            [goog.dom]))

; Canvas reference: http://www.w3schools.com/tags/ref_canvas.asp

;;; TODO - complete you client side rendering game play here!!

(def size 15)

(defn rect [ctx [x y] [r g b]]
  (set! (.-fillStyle ctx) (str "rgb(" r "," g "," b ")"))
  (.fillRect ctx (* size x) (* size y) size size))

(defn draw-snakes [ctx snakes]
  (doseq [snake snakes
          cell (:body snake)]
    (rect ctx cell (:color snake))))

(defn draw-apples [ctx apples]
  (doseq [apple apples]
    (rect ctx apple [255 0 0])))

(defn draw-walls [ctx walls]
  (doseq [wall walls]
    (rect ctx wall [0 0 0])))

(defn draw-players 
  [context {:keys [players]}]
  (set! (.-font context) "12px Arial")
  (set! (.-textAlign context) "start")
  (set! (.-fillStyle context) "#000")
  (doseq [{id :id name :name [x y] :position :as player} players]
    (log ["draw-player" player])
    (doto context
      (.fillText (str "Player " id " : " name) x y))))

(defn update-view
  [canvas game-state]
  (let [context (.getContext canvas "2d")
        w (.-width canvas)
        h (.-height canvas)]
    (doto context
      (.clearRect 0 0 w h)
      (draw-snakes (:snakes game-state))
      (draw-apples (:apples game-state))
      (draw-walls (:walls game-state)))))

(defn create!
  []
  (let [canvas (.getElementById js/document "canvas")
        c (chan (sliding-buffer 1))]
    (go (loop [game-state (<! c)]
          (update-view canvas game-state)
          (recur (<! c))))
    c))
