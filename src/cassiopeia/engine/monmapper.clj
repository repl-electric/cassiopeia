(ns cassiopeia.engine.monmapper
  (:use [overtone.live])
  (:require  [monome.fonome :as fon]))

(defn bind [the-fon handlers]
  (let [event-id (:id the-fon)
        led-id (str event-id "-led")
        press-id (str event-id "-press")]

    (on-event [:fonome :led-change event-id]
              (fn [{:keys [x y new-leds]}]
                (let [on? (get new-leds [x y])]
                  (if on?
                    ((:on handlers))
                    ((:off handlers)))))
              led-id)

    (on-event [:fonome :press event-id]
              (fn [{:keys [x y fonome]}]
                (fon/toggle-led fonome x y))
              press-id)))
