(ns shadowdark-companion.character
  (:require [clojure.spec.alpha :as spec]
            [taoensso.timbre :as log]
            [shadowdark-companion.dice :refer :all]))

(defrecord PlayerCharacter [name
                            STR
                            DEX
                            CON
                            INT
                            WIS
                            CHA
                            HP
                            ancestry
                            class
                            alignment
                            background
                            ;deity
                            ;talents
                            ;spells
                            ;gear
                            ])

(def classes [:fighter :priest :thief :wizard])
(def alignments [:lawful :neutral :chaotic])
(def ancestries [:human :elf :dwarf :halfling :half-orc :goblin])

(def dwarf-names ["Hilde" "Torbin" "Marga" "Bruno" "Karina" "Naugrim" "Brenna" "Darvin" "Elga" "Alric" "Isolde" "Gendry" "Bruga" "Junnor" "Vidrid" "Torson" "Brielle" "Ulfgar" "Sarna" "Grimm"])
(def elf-names ["Eliara" "Ryarn" "Sariel" "Tirolas" "Galira" "Varos" "Daeniel" "Axidor" "Hiralia" "Cyrwin" "Lothiel" "Zaphiel" "Nayra" "Ithior" "Amriel" "Elyon" "Jirwyn" "Natinel" "Fiora" "Ruhiel"])
(def goblin-names ["Iggs" "Tark" "Nix" "Lenk" "Roke" "Fitz" "Tila" "Riggs" "Prim" "Zeb" "Finn" "Borg" "Yark" "Deeg" "Nibs" "Brak" "Fink" "Rizzo" "Squib" "Grix"])
(def halfling-names ["Willow" "Benny" "Annie" "Tucker" "Marie" "Hobb" "Cora" "Gordie" "Rose" "Ardo" "Alma" "Norbert" "Jennie" "Barvin" "Tilly" "Pike" "Lydia" "Marlow" "Astrid" "Jasper"])
(def half-orc-names ["Vara" "Gralk" "Ranna" "Korv" "Zasha" "Hrogar" "Klara" "Tragan" "Brolga" "Drago" "Yelena" "Krull" "Ulara" "Tulk" "Shiraal" "Wulf" "Ivara" "Hirok" "Aja" "Zoraan"])
(def human-names ["Zali" "Bram" "Clara" "Nattias" "Rina" "Denton" "Mirena" "Aran" "Morgan" "Giralt" "Tamra" "Oscar" "Ishana" "Rogar" "Jasmin" "Tarin" "Yuri" "Malchor" "Lienna" "Godfrey"])

(def backgrounds ["Urchin" "Wanted" "Cult Initiate" "Thieves' Guild" "Banished" "Orphaned" "Wizard's Apprentice" "Jeweler" "Herbalist" "Barbarian" "Mercenary" "Sailor" "Acolyte" "Soldier" "Ranger" "Scout" "Minstrel" "Scholar" "Noble" "Chirurgeon"])

(spec/def :character/name string?)
(spec/def :character/ability-score (spec/int-in 3 19))
(spec/def :character/STR :character/ability-score)
(spec/def :character/DEX :character/ability-score)
(spec/def :character/CON :character/ability-score)
(spec/def :character/INT :character/ability-score)
(spec/def :character/WIS :character/ability-score)
(spec/def :character/CHA :character/ability-score)
(spec/def :character/ancestry ancestries)
(spec/def :character/class classes)
(spec/def :character/alignment alignments)
(spec/def :character/background string?)
;(spec/def :character/deity string?)
;(spec/def :character/talents (spec/coll-of string?))
;(spec/def :character/spells (spec/coll-of string?))
;(spec/def :character/gear (spec/coll-of string?))

(defn in-range? [start end val]
  (spec/int-in-range? start (inc end) val))

(defn ability-mod [score]
  (int (Math/floor (/ (- score 10) 2))))

(defn- generate-alignment []
  (let [result (d6)]
    (cond
      (in-range? 1 3 result) :lawful
      (in-range? 4 5 result) :neutral
      :else :chaotic)))

(defn- generate-ancestry []
  (let [result (d12)]
    (cond
      (in-range? 1 4 result) :human
      (in-range? 5 6 result) :elf
      (in-range? 7 8 result) :dwarf
      (in-range? 9 10 result) :halfling
      (= 11 result) :half-orc
      :else :goblin)))

(defn- hit-die [cls]
  (get {:fighter d8
        :priest d6
        :thief d4
        :wizard d4} cls))

(defn generate-hp [cls ancestry con]
  (let [hd (hit-die cls)
        con-bonus (ability-mod con)
        hp (if (= ancestry :dwarf)
             (reduce + [(hd) (hd) con-bonus])
             (reduce + [(hd) con-bonus]))
        hp2 (if (< hp 1) 1 hp)]
    (if (= ancestry :dwarf) (+ hp2 2) hp2)))

(defn generate-name [ancestry]
  (cond
    (= ancestry :dwarf) (rand-nth dwarf-names)
    (= ancestry :elf) (rand-nth elf-names)
    (= ancestry :goblin) (rand-nth goblin-names)
    (= ancestry :halfling) (rand-nth halfling-names)
    (= ancestry :half-orc) (rand-nth half-orc-names)
    (= ancestry :human) (rand-nth human-names)))

(defn roll-character []
  (let [class (rand-nth classes)
        ancestry (generate-ancestry)
        abilities (map (fn [_] (roll-3d6)) (range 6))
        hp (generate-hp class ancestry (nth abilities 2))]
    (->PlayerCharacter (generate-name ancestry)
                       (nth abilities 0)
                       (nth abilities 1)
                       (nth abilities 2)
                       (nth abilities 3)
                       (nth abilities 4)
                       (nth abilities 5)
                       hp
                       ancestry
                       class
                       (generate-alignment)
                       (rand-nth backgrounds)
                       )))