(ns multiplay.game.core
  (:require [multiplay.game.params :as params]
            [clojure.set :refer [difference]]))

(def field-width 30)
(def field-height 30)
(def number-of-apples 5)


(def initial-game-state
   {:apples #{}
    :snakes []
    :walls #{[20 20] [22 22]
             }})

(def dirs {:right [1 0]
           :left [-1 0]
           :up [0 -1]
           :down [0 1]})

(defn random-color
  ([]
     (->> #(+ 127 (rand-int 127))
          (repeatedly 3)
          vec))
  ([forbidden]
     (let [colors (repeatedly #(random-color))
           forbidden (set forbidden)]
       (->> colors distinct (remove forbidden) first))))

(defn neib-cell [cell dir]
  (let [[new-x new-y] (map + cell (dirs dir))]
    [(mod (+ new-x field-width) field-width)
     (mod (+ new-y field-height) field-height)]))

(defn rand-cells [occupied]
  (->> #(vector (rand-int field-width) (rand-int field-height))
       repeatedly
       (remove occupied)
       distinct))

(defn move-snake [{:keys [body dir] :as snake} apples]
  (let [new-head (neib-cell (first body) dir)
        new-body (if (apples new-head)
                   (cons new-head body)
                   (cons new-head (butlast body)))]
    (assoc snake :body new-body)))

(defn update-apples [snakes walls apples]
  (let [heads (set (map #(first (:body %)) snakes))
        apples (difference apples heads)
        to-add (- number-of-apples (count apples))]
    (if (pos? to-add)
      (let [occupied (set (concat apples walls (mapcat :body snakes)))]
        (->> (rand-cells occupied)
         (take to-add)
         (into apples)))
      apples)))

(defn update-world [world]
  (let [{:keys [snakes apples walls]} world
        new-snakes (map #(move-snake % apples) snakes)
        occupied (frequencies (concat walls
                                      (mapcat :body new-snakes)))
        dead? (fn [snake] (> (occupied (first (:body snake))) 1))
        alive (remove dead? new-snakes)]
    {:walls walls
     :snakes (vec alive)
     :apples (update-apples alive walls apples)}))

(defn add-snake [world id]
  (let [{:keys [apples walls snakes]} world
        occupied (set (concat apples walls (mapcat :body snakes)))
        snake-head (first (rand-cells occupied))
        colors (map :color snakes)
        snake {:body [snake-head]
               :dir :down
               :id id
               :color (random-color colors)}]
    (update-in world [:snakes] conj snake)))

(defn remove-snake [world id]
  (letfn [(remove-by-id [snakes]
            (remove #(= id (:id %)) snakes))]
   (update-in world [:snakes] remove-by-id)))

#_(defn find-snake-idx [id]
  (let [snake-ids (vec (keep :id (:snakes @world)))
        idx (.indexOf snake-ids id)]
    (if (= idx -1) nil idx)))

#_(defn change-snake-dir [world dir id]
  (if-let [idx (find-snake-idx id)]
    (assoc-in world [:snakes (find-snake-idx id) :dir] dir)))

(defn update-player [player-to-update dir {:keys [snakes]}]
  (println "Update!")
  (map (fn [{:keys [id] :as snake}]
    (if (= id player-to-update)
      (assoc snake :dir dir)
      snake)) snakes))


(defmulti handle-command
  (fn [game-state command]
    (prn "handle-command" command game-state)
    (first command)))

(defmethod handle-command :default
  [game-state [command id]]
  game-state)

(defmethod handle-command :player/leave
  [game-state [command id]]

  (assoc game-state :players
         (vec (remove #(= (:id %) id) (:players game-state)))))

(defmethod handle-command :player/join
  [game-state [command id name]]
  (add-snake game-state id))

(defmethod handle-command :player/up
  [game-state [command id]]
  (assoc game-state :snakes (update-player id :up game-state)))

(defmethod handle-command :player/down
  [game-state [command id]]
  (assoc game-state :snakes (update-player id :down game-state)))

(defmethod handle-command :player/left
  [game-state [command id]]
  (assoc game-state :snakes (update-player id :left game-state)))

(defmethod handle-command :player/right
  [game-state [command id]]
  (assoc game-state :snakes (update-player id :right game-state)))

(defn- handle-commands
  [game-state commands]
  (reduce (fn [current-state command]
            (handle-command current-state command))
          game-state commands))

;;; TODO - Put your server side game logic here!!

(defn advance
  "Given a game-state and some inputs, advance the game-state one
  tick"
  [game-state commands]
  (let [new-game-state (-> game-state
                           (handle-commands commands))]
    (update-world new-game-state)))
