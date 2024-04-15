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
           :on-change #(reset! value (-> % .-target .-value))}])

(defn apply-mask [ip-octet sub-octet]
  "Applies a subnet mask to an IP address by doing a bitwise comparison of
the bits. The octets must be strings of 8 bits seperated by spaces."
  (let [ip (str/split (calc-bits ip-octet) " ")
        sub (str/split (calc-bits sub-octet) " ")]
    (apply str (map #(if (and (= %1 "1") (= %2 "1")) "1 " "0 ") ip sub))))

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

(comment
  (host-bits "255" "255" "255" "192")
  )

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

(comment
  (- 255 (bits->decimal "1 1 1 1 1 1 0 0"))
  (> (+ 2 "12") 255)
  (last-subnet "1 1 1 1 1 1 1 1" "12"))

;; IP address

(def bit-one (r/atom "0"))
(def bit-two (r/atom "0"))
(def bit-three (r/atom "0"))
(def bit-four (r/atom "0"))

;; Subnets input
(def sub-one (r/atom "0"))
(def sub-two (r/atom "0"))
(def sub-three (r/atom "0"))
(def sub-four (r/atom "0"))

(def cidr (r/atom "0"))
(def toggle-cidr (r/atom "Octet "))

(defn reset-subnet []
  "Resets all subnet values to 0 and toggles the subnet input type."
  (reset! toggle-cidr (if (= @toggle-cidr "CIDR ")
                        "Octet " "CIDR "))
  (reset! cidr "0")
  (reset! sub-one "0")
  (reset! sub-two "0")
  (reset! sub-three "0")
  (reset! sub-four "0"))

(defn toggle-cidr-button []
  "Button to toggle subnet input type."
  [:div
   [:input {:type "button" :value (str @toggle-cidr " notation")
            :on-click #(reset-subnet)}]])

(defn vec->octet [bit-vec drop-num]
  "takes a vector of 8 bits represented as a 1 or 0 followed by a space
and returns a string of 1s and 0s seperated by spaces."
  (apply str (take 8 (drop drop-num bit-vec))))

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
  [:h2 "Input CIDR style subnet affix /" ]
  [:input {:type "text"
           :value @cidr
           :on-change (fn [e]
                        (reset! cidr (-> e .-target .-value))
                        (cidr->subnet (js/parseInt @cidr)))}])

(defn ip-address [bit-one bit-two bit-three bit-four]
    (fn []
      [:div
       [:p "First octet: " [atom-input bit-one]]
       [:p "Second octet: " [atom-input bit-two]]
       [:p "Third octet: " [atom-input bit-three]]
       [:p "Fourth octet: " [atom-input bit-four]]]))

(defn host []
  (fn []
    [:div
     ;; Binary first host
     [:p.mono
      "First host:::::::: "
      (calc-bits (apply-mask->decimal @bit-one @sub-one)) " | "
      (calc-bits (apply-mask->decimal @bit-two @sub-two)) " | "
      (calc-bits (apply-mask->decimal @bit-three @sub-three)) " | "
      (calc-bits (+ 1 (apply-mask->decimal @bit-four @sub-four))) " --- "
      (apply-mask->decimal @bit-one @sub-one) "."
      (apply-mask->decimal @bit-two @sub-two) "."
      (apply-mask->decimal @bit-three @sub-three) "."
      (+ 1 (apply-mask->decimal @bit-four @sub-four))]
     [:p.mono
      "Last host::::::::: "
      (calc-bits (last-subnet @sub-one (apply-mask->decimal @bit-one @sub-one))) " | "
      (calc-bits (last-subnet @sub-two (apply-mask->decimal @bit-two @sub-two))) " | "
      (calc-bits
       (last-subnet @sub-three (apply-mask->decimal @bit-three @sub-three))) " | "
      (calc-bits (- (js/parseInt (last-subnet @sub-four
                                   (apply-mask->decimal @bit-four @sub-four))) 1)) " --- "
      (last-subnet @sub-one (apply-mask->decimal @bit-one @sub-one)) "."
      (last-subnet @sub-two (apply-mask->decimal @bit-two @sub-two)) "."
      (last-subnet @sub-three (apply-mask->decimal @bit-three @sub-three)) "."
      (- (js/parseInt (last-subnet @sub-four
                                   (apply-mask->decimal @bit-four @sub-four))) 1)]
     [:p.mono
      "Broadcast address: "
      (calc-bits
       (last-subnet @sub-one (apply-mask->decimal @bit-one @sub-one))) " | "
      (calc-bits
       (last-subnet @sub-two (apply-mask->decimal @bit-two @sub-two))) " | "
      (calc-bits
       (last-subnet @sub-three (apply-mask->decimal @bit-three @sub-three))) " | "
      (calc-bits
       (last-subnet @sub-four (apply-mask->decimal @bit-four @sub-four))) " --- "
      (last-subnet @sub-one (apply-mask->decimal @bit-one @sub-one)) "."
      (last-subnet @sub-two (apply-mask->decimal @bit-two @sub-two)) "."
      (last-subnet @sub-three (apply-mask->decimal @bit-three @sub-three)) "."
      (last-subnet @sub-four (apply-mask->decimal @bit-four @sub-four))]
  ]))

(defn subnet []
  (fn []
    [:div
     [:p.mono
      "Network Address::: "
      (apply-mask @bit-one @sub-one) " | "
      (apply-mask @bit-two @sub-two) " | "
      (apply-mask @bit-three @sub-three) " | "
      (apply-mask @bit-four  @sub-four) " --- "
      (apply-mask->decimal @bit-one @sub-one) "."
      (apply-mask->decimal @bit-two @sub-two) "."
      (apply-mask->decimal @bit-three @sub-three) "."
      (apply-mask->decimal @bit-four @sub-four)]]))

(comment

  (def bin-ip (vec
                '[ "1" "1" "1" "1" "1" "1" "1" "1" "|"
                "1" "1" "1" "1" "1" "1" "1" "1" "|"
                "1" "1" "1" "1" "1" "1" "1" "1" "|"
                "1" "1" "1" "1" "1" "1" "1" "1" ]))

  (display-bin-ip-test 13 bin-ip)
  (display-bin-ip 3 3))

;; (defn display-bin-ip [host-bits bin-ip]
;;   (fn []
;;   [:p.mono 
;;    (loop [bit-i 0 bits bin-ip]
;;      (if (not (empty? bits))
;;        (if (= "|" (first bits))
;;          (do [:span.divider (first bits)] (recur bit-i (rest bits)))
;;          (cond (< (- 31 host-bits ) bit-i) (do [:span.host (first bits)] (recur (inc bit-i) (rest bits)))
;;                :else (do [:span.network (first bits)] (recur (inc bit-i) (rest bits)))))))]))

  ;; (let [host-bits (r/atom (host-bits @sub-one @sub-two @sub-three @sub-four))
  ;;       bin-ip (r/atom (str/split (str (calc-bits @bit-one) " | "
  ;;                      (calc-bits @bit-two) " | "
  ;;                      (calc-bits @bit-three) " | "
  ;;                      (calc-bits @bit-four)) " "))
  ;;       bin-subnet (r/atom (str/split (str (calc-bits @sub-one) " | "
  ;;                          (calc-bits @sub-two) " | "
  ;;                          (calc-bits @sub-three) " | "
  ;;                          (calc-bits @sub-four)) " " ))]

(defn subnet-mask []
  (fn []
    [:p.mono "Subnet Mask:::::::"
     (loop [bit-i 1 bits (reverse (str/split (str (calc-bits @sub-one) " | "
                                         (calc-bits @sub-two) " | "
                                         (calc-bits @sub-three) " | "
                                         (calc-bits @sub-four)) " " ))
            spans '()]
       (if (empty? bits)
         spans
         (if (= "|" (first bits))
           (recur bit-i (rest bits) (conj spans [:span.divider " " (first bits) " "]))
           (cond (< (host-bits @sub-one @sub-two @sub-three @sub-four) bit-i)
                 (recur (inc bit-i) (rest bits) (conj spans [:span.network " " (first bits) " "]))
                 :else (recur (inc bit-i) (rest bits) (conj spans [:span.host " " (first bits) " "]))))))
     " --- "   @sub-one "." @sub-two "." @sub-three "." @sub-four " --- 2 ^ "
     (host-bits @sub-one @sub-two @sub-three @sub-four) " is "
     (exp 2 (host-bits @sub-one @sub-two @sub-three @sub-four)) " hosts. "]))

(defn ip-input []
    (fn []
      [:div
       [:h2 "IP Addesss" ]
       [ip-address bit-one bit-two bit-three bit-four]
       [:h2 "Subnet Mask"]
       [toggle-cidr-button]
       (if (= @toggle-cidr "CIDR ")
         [ip-address sub-one sub-two sub-three sub-four]
         [input-cidr])
       [:p.mono
        "IP address::::::::  "
        (calc-bits @bit-one) " | "
        (calc-bits @bit-two) " | "
        (calc-bits @bit-three) " | "
        (calc-bits @bit-four) " --- " ]
       [subnet-mask]
       [subnet]
       [host]]))

  

;; -------------------------
;; Initialize app

(defn ^:dev/after-load mount-root []
  (d/render [ip-input] (.getElementById js/document "app")))

(defn ^:export ^:dev/once init! []
  (mount-root))
