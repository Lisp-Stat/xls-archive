;;
;;  GEE tests - a port of the test file distributed with the Splus gee()
;;  procedure by V. Carey and Aidan McDermott. Comments indicate models
;;  that are not yet implemented. This is in case you want to compare the 
;;  output file with the Splus version.
;;
;;
;;
(dribble "geetest.out")
(load "gee.1.0.lsp")
(defvar data (read-data-columns "testdata.dat" 12))
(defvar id (select data 1))
(defvar x1 (select data 2))
(defvar x2 (select data 3))
(defvar x3 (select data 4))
(defvar x4 (select data 5))
(defvar gsind (select data 6))
(defvar gs (select data 7))
(defvar lgind (select data 8))
(defvar lg (select data 9))
(defvar lg1 (select data 10))
(defvar nn (select data 11))
;; 
(gee-model :x (list x2 x3 x4) :y gsind :g id :error normal-error)
;;
(gee-model :x (list x2 x4) :y gs :g id :error normal-error :correlation exchangeable-corr)
;;
(gee-model :x (list (sin x2) (log x3) x4) :y gsind :g id :link identity-link)
;;
;;  model omitted - uses gam() fit.
;;
(def a (gee-model :x (list x2 x3 x4) :y lgind :g id :error binomial-error))
;; model omitted -- sets scale parameter to 1
;;
(gee-model :x (list x2 x3 x4) :y lg :g id :link logit-link :correlation exchangeable-corr)
;;
;; two ways of specifying stationary 1-dependence
;; 
;;
;; Unlike S-PLUS, SPIDA and SAS this program requires a times variable 
;; whenever it might matter (ie except for exchangeable & independence)
;; This is a FEATURE, not a bug.
;;
(def obstimes (repeat (iseq 1 20) 25))
;
(gee-model :x (list x2 x3 x4) :y lg :g id :error binomial-error :correlation (m-dependence 1 :stationary t) :times obstimes)
(gee-model :x (list x2 x3 x4) :y lg :g id :error binomial-error :correlation (stat-m-dependence 1) :times obstimes)
;;
(gee-model :x (list x2 x3 x4) :y lg :g id :link probit-link)
;; A fixed error model
(def subset (- (* (iseq 1 100) 5) 1))
(def x2s (select x2 subset))
(def x4s (select x4 subset))
(def lg1s (select lg1 subset))
(def R #2A((1.0 0.2 0.3 0.1)
	   (0.2 1.0 0.2 0.3)
	   (0.3 0.2 1.0 0.2)
	   (0.1 0.3 0.2 1.0)))
(def obstimess (select obstimes subset))
(def ids (select id subset))

(gee-model :x (list x2s x4s) :y lg1s :g ids :error poisson-error :correlation (fixed-corr R) :times obstimess)
;; other examples not in the S_PLUS file
(gee-model :x (list x2s x4s) :y lg1s :g ids :error poisson-error :correlation saturated-corr :times obstimess)
(gee-model :x (list x2s x4s) :y lg1s :g ids :error poisson-error :correlation saturated-ml-corr :times obstimess)
(gee-model :x (list x2s x4s) :y lg1s :g ids :error poisson-error :correlation (stat-m-dependence 3) :times obstimess)
;;

(dribble)
(dribble "modelexamples.out")
;
; some imaginary data
;
(defun flatten (x) (apply #'append x))
(def smoking (repeat '("never" "past" "current") 20))
(def sex (repeat '("M" "F") 30))
(def group (flatten (mapcar #'(lambda (x) (repeat x 2)) (iseq 1 30))))
(def age (+ 40 (* 5 (normal-rand 60))))
(def y (+ (/ age 100) (normal-rand 60)))

;
(def maineffects (as-formula '( (factor smoking) (factor sex) (term age))))
(def plusinteractions (as-formula '( (factor smoking) (factor sex) (term age) (interaction (list smoking sex)) (interaction (list smoking age) :is-factor (list t nil)))))
;
(def main-model (gee-model :x maineffects :y y :g group :error normal-error ))
(send main-model :display)
(send main-model :display-with-formula)
(send main-model :display-with-formula :block-only nil)
(def *gee-display-block-only* nil)
(def big-model (gee-model :x plusinteractions :y y :g group :error normal-error))
(send big-model :display)
(send big-model :display-with-formula)
(send big-model :display-with-formula :block-only t)
(dribble)

