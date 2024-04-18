(ns kit.ipcalc.core
    (:require
     [clojure.string :as str]
      [reagent.core :as r]
      [reagent.dom :as d]))

;; -------------------------
;; Views
;; Connet to shadowjs with (shadow.cljs.devtools.api/repl :app)

(defn exp [x n]
  "Calcuclates an exponent x^n"
     (if (zero? n) 1
         (* x (exp x (dec n)))))

(defn calc-bits ([dec-str]
                 "Takes a decimal number between 0 and 255 and returns
a string representing the binary reprsentation."
                 (cond
                   (> dec-str 255) (str "Max is 255")
                   (< dec-str 128) (calc-bits dec-str "0 " 6)
                   (>= dec-str 128) (calc-bits (- dec-str 128) "1 " 6)))
  ([dec-str bit-str bit-pos]
   (if (= bit-pos 0)
     (str bit-str dec-str)
     (if (< dec-str (exp 2 bit-pos))
       (recur dec-str (str bit-str "0 ") (- bit-pos 1))
       (recur (- dec-str (exp 2 bit-pos)) (str bit-str "1 ") (- bit-pos 1))))))

(defn atom-input [value]
  "Basic string input that updates an atom"
  [:input {:type "text"
           :value @value
           :on-change #((if (and (>= (-> % .-target .-value) 0)
                                 (<= (-> % .-target .-value) 255))
                          (reset! value (-> % .-target .-value))))}])

(defn apply-mask [ip-octet sub-octet]
  "Applies a subnet mask to an IP address by doing a bitwise comparison of
the bits. The octets must be strings of 8 bits seperated by spaces."
  (let [ip (str/split (calc-bits ip-octet) " ")
        sub (str/split (calc-bits sub-octet) " ")]
    (str/trim (apply str (map #(if (and (= %1 "1") (= %2 "1")) "1 " "0 ") ip sub)))))

(defn bits->decimal ([bits]
                     "Takes a string representing bits and returns a decimal
represetation of the same value."
                     (let [bit-arr (str/split bits " ")]
                       (bits->decimal bit-arr 0)))
([bits total]
 (if (= (count bits) 0)
   total
   (bits->decimal (rest bits) (if (= (first bits) "1")
                                (+ total (exp 2 (- (count bits) 1)))
                                total)))))

(defn bin->vec [string]
  "turns a binary IP into a reversed order array array."
  (reverse (str/split string " ")))

(defn vec->octet [bit-vec drop-num]
  "takes a vector of 8 bits represented as a 1 or 0 followed by a space
and returns a string of 1s and 0s seperated by spaces."
  (apply str (take 8 (drop drop-num bit-vec))))

(defn host-bits [sub-one sub-two sub-three sub-four]
  (let [mask (str
              (calc-bits sub-one) " "
              (calc-bits sub-two) " "
              (calc-bits sub-three) " "
              (calc-bits sub-four))]
    (count (filter #(= % "0") (str/split mask " ")))))

(defn apply-mask->decimal [ip-octet sub-octet]
  "Returns decimal representation of applied subnet mask"
  (bits->decimal (apply-mask ip-octet sub-octet)))

(defn last-subnet [mask octet]
  "Takes an octet string and returns a broadcast address"
  (let [host-count (- 255  (js/parseInt mask))
        octet (js/parseInt octet)]
    (if (> (+ host-count octet) 255) "255" (str (+ host-count octet)))))

(defn class-a-type [ip]
  (cond (= 255 (ip :two) (ip :three) (ip :four)) (str " broadcast")
        (= 0 (ip :two) (ip :three) (ip :four)) (str " network address")
        :else (str " host address")))

(defn class-b-type [ip]
  (cond (= 255 (ip :three) (ip :four)) (str " broadcast")
        (= 0 (ip :three) (ip :four)) (str " network address")
        :else (str " host address")))

(defn class-c-type [ip]
  (cond (= 255 (ip :four)) (str " broadcast")
        (= 0 (ip :four)) (str " network address")
        :else (str " host address")))

(defn class-a [ip subnet]
  "Test for class A subnet mask."
  (if (and (= (subnet :one) 255)
           (< (subnet :two) 255)
           (< (subnet :three) 255)
           (< (subnet :four) 255))
  (str " --- Class A" (class-a-type ip))))
   
(defn class-b [ip subnet]
  "Test for class B subnet mask."
  (if (and (= (subnet :one) 255)
           (= (subnet :two) 255)
           (< (subnet :three) 255)
           (< (subnet :four) 255))
  (str " --- Class B")))

(defn class-c [ip subnet]
  "Test class C subnet mask"
  (if (and (= (subnet :one) 255)
           (= (subnet :two) 255)
           (= (subnet :three) 255)
           (< (subnet :four) 255))
  (str " --- Class C")))

(defn class-d [subnet]
  "Test class D subnet mask"
  (if (and (= (subnet :one) 240)
           (= (subnet :two) 0)
           (= (subnet :three) 0)
           (= (subnet :four) 0))
  (str " --- Class D")))

(defn class-e [subnet]
  "Test class D subnet mask"
  (if (and (= (subnet :one) 0)
           (= (subnet :two) 0)
           (= (subnet :three) 0)
           (= (subnet :four) 0))
  (str " --- Class E")))

(defn ip-class [ip subnet]
  "Test ip address for IP class"
  (if (and (>= (ip :two) 0) (>= (ip :three) 0) (>= (ip :four) 0)
           (<= (ip :two) 255) (<= (ip :three) 255) (<= (ip :four) 255))
  (cond (and (>= (ip :one) 0) (<= (ip :one) 127))
        (class-a ip subnet)
        (and (>= (ip :one) 128) (<= (ip :one) 191))
        (class-b ip subnet)
        (and (>= (ip :one) 192) (<= (ip :one) 224))
        (class-c ip subnet)
        (and (>= (ip :one) 224) (<= (ip :one) 239))
        (class-d subnet)
        (and (>= (ip :one) 240) (<= (ip :one) 255))
        (class-e subnet))))

;; IP address atoms
(def byte-one (r/atom "0"))
(def byte-two (r/atom "0"))
(def byte-three (r/atom "0"))
(def byte-four (r/atom "0"))

;; Subnets input atoms
(def sub-one (r/atom "0"))
(def sub-two (r/atom "0"))
(def sub-three (r/atom "0"))
(def sub-four (r/atom "0"))

;; CIDR atoms
(def cidr (r/atom "0"))
(def toggle-cidr (r/atom "Toggle Input: Decimal"))

(defn reset-subnet []
  "Resets all subnet values to 0 and toggles the subnet input type."
  (reset! toggle-cidr (if (= @toggle-cidr "Toggle Input: Decimal")
                        "Toggle Input: CIDR" "Toggle Input: Decimal"))
  (reset! cidr "0")
  (reset! sub-one "0")
  (reset! sub-two "0")
  (reset! sub-three "0")
  (reset! sub-four "0"))

(defn toggle-cidr-button []
  "Button to toggle subnet input type."
  [:div
   [:input.cidr {:type "button" :value @toggle-cidr
            :on-click #(reset-subnet)}]])

(defn cidr->subnet [cidr]
  "Takse a CIDR notation for a subnet mask and sets the decimal version.
I.E.: 255 255 255 0"
  (let [bit-vec (into
                 (vec (replicate cidr "1 "))
                 (vec (replicate (- 32 cidr ) "0 ")))]
    (reset! sub-one (bits->decimal (vec->octet bit-vec 0)))
    (reset! sub-two (bits->decimal (vec->octet bit-vec 8)))
    (reset! sub-three (bits->decimal (vec->octet bit-vec 16)))
    (reset! sub-four (bits->decimal (vec->octet bit-vec 24)))))

(defn input-cidr []
  "Input for cider notaiton subnet that sets the decimal version of
each octet"
  [:div
   [:span "/ "]
   [:input {:type "text"
            :value @cidr
            :on-change (fn [e]
                         (if (and (>= (-> e .-target .-value) 0 ) (<= (-> e .-target .-value) 30))
                           (reset! cidr (-> e .-target .-value)))
                         (cidr->subnet (js/parseInt @cidr)))}]])

(defn ip-address [bit-one bit-two bit-three bit-four]
  "Componenet for the IP inputs including decimal notation sub net"
    (fn []
      [:div.ip-input
       [atom-input bit-one]
       [:span " . "]
       [atom-input bit-two]
       [:span " . "]
       [atom-input bit-three]
       [:span " . "]
       [atom-input bit-four]]))

(defn broadcast []
  "Component to display broadcast address in binary and decimal formats"
  (fn []
    [:p.mono "Broadcast Address:"
     (loop [bit-i 1 bits (bin->vec
                          (str
                           (calc-bits
                            (last-subnet @sub-one
                                         (apply-mask->decimal @byte-one @sub-one))) " | "
                           (calc-bits
                            (last-subnet @sub-two
                                         (apply-mask->decimal @byte-two @sub-two))) " | "
                           (calc-bits
                            (last-subnet @sub-three
                                         (apply-mask->decimal @byte-three @sub-three))) " | "
                           (calc-bits
                            (last-subnet @sub-four
                                         (apply-mask->decimal @byte-four @sub-four))) " "))
            spans '()]
       (if (empty? bits)
         spans
         (if (= "|" (first bits))
           (recur bit-i (rest bits)
                  (conj spans [:span.divider {:key (str bit-i "-div")} " " (first bits) " "]))
           (cond (< (host-bits @sub-one @sub-two @sub-three @sub-four) bit-i)
                 (recur (inc bit-i) (rest bits)
                        (conj spans 
                              [:span {:key bit-i :class "network"} " " (first bits) " "]))
                 :else (recur (inc bit-i) (rest bits)
                              (conj spans 
                                    [:span {:key bit-i :class "host"} " " (first bits) " "]))))))
     " : " (last-subnet @sub-one (apply-mask->decimal @byte-one @sub-one))
     "." (last-subnet @sub-two (apply-mask->decimal @byte-two @sub-two))
     "." (last-subnet @sub-three (apply-mask->decimal @byte-three @sub-three))
     "." (last-subnet @sub-four (apply-mask->decimal @byte-four @sub-four))]))


(defn last-host []
  "Componenet to display the last host of a subnet in both decimal and binary formats"
  (fn []
    [:p.mono "Last host:::::::::"
     (loop [bit-i 1 bits (bin->vec
                          (str
                           (calc-bits
                            (last-subnet @sub-one (apply-mask->decimal @byte-one @sub-one))) " | "
                           (calc-bits
                            (last-subnet @sub-two (apply-mask->decimal @byte-two @sub-two))) " | "
                           (calc-bits
                            (last-subnet @sub-three (apply-mask->decimal @byte-three @sub-three))) " | "
                           (calc-bits (- (js/parseInt
                                          (last-subnet @sub-four
                                                       (apply-mask->decimal @byte-four @sub-four))) 1))))
            spans '()]
       (if (empty? bits)
         spans
         (if (= "|" (first bits))
           (recur bit-i (rest bits)
                  (conj spans
                        [:span {:key (str bit-i "-div") :class "divider"} " " (first bits) " "]))
           (cond (< (host-bits @sub-one @sub-two @sub-three @sub-four) bit-i)
                 (recur (inc bit-i) (rest bits) (conj spans 
                                                      [:span {:key bit-i :class "network"}  " " (first bits) " "]))
                 :else (recur (inc bit-i) (rest bits) (conj spans 
                                                            [:span {:key bit-i :class "host"} " " (first bits) " "]))))))
     " : " (last-subnet @sub-one (apply-mask->decimal @byte-one @sub-one))
     "." (last-subnet @sub-two (apply-mask->decimal @byte-two @sub-two))
     "." (last-subnet @sub-three (apply-mask->decimal @byte-three @sub-three))
     "." (- (js/parseInt (last-subnet @sub-four (apply-mask->decimal @byte-four @sub-four))) 1)]))

(defn first-host []
  "Component to display the first host address in a sub net in both binary and decimal formats"
  (fn []
    [:p.mono "First host::::::::"
     (loop [bit-i 1 bits (bin->vec (str (calc-bits (apply-mask->decimal @byte-one @sub-one)) " | "
                                         (calc-bits (apply-mask->decimal @byte-two @sub-two)) " | "
                                         (calc-bits (apply-mask->decimal @byte-three @sub-three)) " | "
                                         (calc-bits (+ 1(apply-mask->decimal @byte-four @sub-four)))))
            spans '()]
       (if (empty? bits)
         spans
         (if (= "|" (first bits))
           (recur bit-i (rest bits)
                  (conj spans 
                        [:span {:key (str bit-i "-div") :class "divider"} " " (first bits) " "]))
           (cond (< (host-bits @sub-one @sub-two @sub-three @sub-four) bit-i)
                 (recur (inc bit-i) (rest bits) (conj spans 
                                                      [:span {:key bit-i :class "network"} " " (first bits) " "]))
                 :else (recur (inc bit-i) (rest bits) (conj spans 
                                                            [:span {:key bit-i :class "host"} " " (first bits) " "]))))))
     " : " (apply-mask->decimal @byte-one @sub-one)
     "." (apply-mask->decimal @byte-two @sub-two)
     "." (apply-mask->decimal @byte-three @sub-three)
     "." (+ 1 (apply-mask->decimal @byte-four @sub-four))]))


(defn network-address []
  "Component to dispaly the network address of a sub net in both decimal and binary"
  (fn []
    [:p.mono "Network Address:::"
     (loop [bit-i 1 bits (bin->vec (str (apply-mask @byte-one @sub-one) " | "
                                         (apply-mask @byte-two @sub-two) " | "
                                         (apply-mask @byte-three @sub-three) " | "
                                         (apply-mask @byte-four  @sub-four)))
            spans '()]
       (if (empty? bits)
         spans
         (if (= "|" (first bits))
           (recur bit-i (rest bits)
                  (conj spans 
                        [:span {:key (str bit-i "-div") :class "divider"} " " (first bits) " "]))
           (cond (< (host-bits @sub-one @sub-two @sub-three @sub-four) bit-i)
                 (recur (inc bit-i) (rest bits) (conj spans 
                                                      [:span {:key bit-i :class "network"} " " (first bits) " "]))
                 :else (recur (inc bit-i) (rest bits) (conj spans 
                                                            [:span {:key bit-i :class "host"} " " (first bits) " "]))))))
     " : " (apply-mask->decimal @byte-one @sub-one)
     "." (apply-mask->decimal @byte-two @sub-two)
     "." (apply-mask->decimal @byte-three @sub-three)
     "." (apply-mask->decimal @byte-four @sub-four)]))

(defn subnet-mask []
  "Component to display a subnet mask in both binary and decimal formats."
  (fn []
    [:p.mono "Subnet Mask:::::::"
     (loop [bit-i 1 bits (bin->vec (str (calc-bits @sub-one) " | "
                                         (calc-bits @sub-two) " | "
                                         (calc-bits @sub-three) " | "
                                         (calc-bits @sub-four)))
            spans '()]
       (if (empty? bits)
         spans
         (if (= "|" (first bits))
           (recur bit-i (rest bits)
                  (conj spans 
                        [:span {:key (str bit-i "-div") :class "divider"} " " (first bits) " "]))
           (cond (< (host-bits @sub-one @sub-two @sub-three @sub-four) bit-i)
                 (recur (inc bit-i) (rest bits)
                        (conj spans 
                             [:span {:key bit-i :class "network"} " " (first bits) " "]))
                 :else (recur (inc bit-i) (rest bits)
                             (conj spans [:span {:key bit-i :class "host"} " " (first bits) " "]))))))
     " : "   @sub-one "." @sub-two "." @sub-three "." @sub-four " --- 2 ^ "
     (host-bits @sub-one @sub-two @sub-three @sub-four) " - 2 is "
     (- (exp 2 (host-bits @sub-one @sub-two @sub-three @sub-four)) 2) " hosts. "]))

(defn ipv4-subnet []
  "Main component. Entry point for the IPv4 caclculator."
    (fn []
      [:div
       [:form.ip-input
       [:label "IP Address"]
       [:label.subnet "Subnet Mask"]
       [ip-address byte-one byte-two byte-three byte-four]
        [:div.subnet
       (if (= @toggle-cidr "Toggle Input: CIDR")
         [ip-address sub-one sub-two sub-three sub-four]
         [input-cidr])
       [toggle-cidr-button]]]
       [:p.mono [:span.network "Network bits |"][:span.host " Host Bits"]]
       [:p.mono
        "IP address::::::::  "
        (calc-bits @byte-one) " | "
        (calc-bits @byte-two) " | "
        (calc-bits @byte-three) " | "
        (calc-bits @byte-four) " : "
        @byte-one "." @byte-two "." @byte-three "." @byte-four
        [ip-class
         {:one (js/parseInt @byte-one)
          :two (js/parseInt @byte-two)
          :three (js/parseInt @byte-three)
          :four (js/parseInt @byte-four)}
         {:one (js/parseInt @sub-one)
          :two (js/parseInt @sub-two)
          :three (js/parseInt @sub-three)
          :four (js/parseInt @sub-four)}]]
       [subnet-mask]
       [network-address]
       [first-host]
       [last-host]
       [broadcast]]))

;; -------------------------
;; Initialize app

(defn ^:dev/after-load mount-root []
  (d/render [ipv4-subnet] (.getElementById js/document "app")))

(defn ^:export ^:dev/once init! []
  (mount-root))
