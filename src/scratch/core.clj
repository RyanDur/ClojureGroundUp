(ns scratch.core)

(defn myinc
  [[x & xs]]
  (when x (cons (inc x)(myinc xs))))

(defn mymap
  [f [x & xs]]
  (when x (cons (f x) (mymap f xs))))

(take 10 (iterate  (fn [x] x)  10))

(take 10 (iterate (fn [x] (if (odd? x) (+ 1 x) (/ x 2))) 10))


(defn pal?
  "Write a function to find out if a string is a palindrome
   that is, if it looks the same forwards and backwards."
  [s]
  (let [[_ a b c] (re-matches #"(^.)(.*)(.$)" s)]
    (or (<= (count s) 1) (and (= a c) (recur b)))))

;; Find the number of ‘c’s in “abracadabra”.
(count (filter (fn [x] (= (str x) "c")) "abracadabra"))
((frequencies "abracadabra") \c)

(defn myfilter
  "Write your own version of filter."
  [f [h & tl]]
  (when h
    (if (f h) (cons h (myfilter f tl)) (myfilter f tl))))

(count (myfilter (fn [x] (= (str x) "c")) "abracadabra"))

;; Find the first 100 prime numbers: 2, 3, 5, 7, 11, 13, 17, ….
(defn prime?
  [x]
  (or (<= x 3) (not-any? #(= (mod x %) 0) (range 2 (+ 1 (Math/sqrt x))))))

(take 100 (filter prime? (cons 2 (iterate (partial + 2) 3))))

(->> (iterate (partial + 2) 3)
     (cons 2)
     (filter prime?)
     (take 100))


(defn schedule
  "Using the control flow constructs we’ve learned, write a
   schedule function which, given an hour of the day, returns
   what you’ll be doing at that time. (schedule 18), for me,
   returns :dinner."
  [time]
  (condp <= time
    22 :sleep
    19 :TV
    18 :dinner
    13 :TV
    12 :lunch
    8  :TV
    :sleep))


;; Using the threading macros, find how many numbers from 0 to 9999
;; are palindromes: identical when written forwards and backwards.
;; 121 is a palindrome, as is 7447 and 5, but not 12 or 953.

(defn parse-int [s]
  "http://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number"
  (Integer. (re-find  #"\d+" s )))
;;

(take 9999 (map parse-int (filter pal? (map str (range 10000)))))

(->> (range 10000)
     (map str)
     (filter pal?)
     (map parse-int)
     (take 9999))


(defmacro id
  "Write a macro id which takes a function and a list of args:
   (id f a b c), and returns an expression which calls that
   function with the given args: (f a b c)."
  [f & args]
  (eval (cons f args)))

(id pal? "ab")


(def logging-enabled false)

(defmacro log
  "Write a macro log which uses a var, logging-enabled, to
   determine whether or not to print an expression to the console
   at compile time. If logging-enabled is false, (log :hi) should
   macroexpand to nil. If logging-enabled is true, (log :hi)
   should macroexpand to (prn :hi). Why would you want to do this
   check during compilation, instead of when running the program?
   What might you lose?"
  [& body]
  `(when logging-enabled
     (do (prn ~@body))))


(defmacro exact
  "(Advanced) Using the rationalize function, write a macro exact
   which rewrites any use of +, -, *, or / to force the use of
   ratios instead of floating-point numbers. (* 2452.45 100)
   returns 245244.99999999997, but (exact (* 2452.45 100)) should
   return 245245N"
  [[f & body]]
  (let [args (map rationalize body)]
    (eval `(~f ~@args))))

;; (macroexpand-1 '(exact (* 2452.45 100)))

(exact (* 2452.45 100))
