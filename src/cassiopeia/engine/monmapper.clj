(ns cassiopeia.engine.monmapper
  (:use [overtone.live])
  (:require [monome.polynome :as poly]
            [monome.fonome :as fon]))

(defn bind
  "Quickly bind an on/off to a button on the monome"
  [m128 handle x y handlers]
  (let [the-fon (fon/mk-fonome handle 1 1)
        event-id (:id the-fon)
        led-id (str event-id "-led")
        press-id (str event-id "-press")]

    (poly/dock-fonome! m128 the-fon handle x y)

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
              press-id)

    the-fon))
