;(load "twoway")

;(def bt   (boxmat tw :transpose t			       :title "bt"))
;(def bx   (boxmat tw		   :reverse-x t 	       :title "bx"))
;(def btx  (boxmat tw :transpose t :reverse-x t 	       :title "btx"))
;(def br   (boxmat tw				:reverse-row t :title "br"))
;(def btr  (boxmat tw :transpose t		:reverse-row t :title "btr"))
;(def bxr  (boxmat tw		   :reverse-x t :reverse-row t :title "bxr"))
;(def btxr (boxmat tw :transpose t :reverse-x t :reverse-row t :title "btxr"))

(def tt   (boxmat-ct tw :like btw :transpose t				   :title "tt"))
(def ttx  (boxmat-ct tw :like btw :transpose t :reverse-x t		   :title "ttx"))
(def ttr  (boxmat-ct tw :like btw :transpose t		    :reverse-row t :title "ttr"))
(def ttxr (boxmat-ct tw :like btw :transpose t :reverse-x t :reverse-row t :title "ttxr"))

(def c	  (boxmat-ct tw :like btw				   :title "c"))
(def cx   (boxmat-ct tw :like btw      :reverse-x t		   :title "cx"))
(def cr   (boxmat-ct tw :like btw		    :reverse-row t :title "cr"))
(def cxr  (boxmat-ct tw :like btw      :reverse-x t :reverse-row t :title "cxr"))


(send btw  :linked t)

;(send bt   :linked t)
;(send bx   :linked t)
;(send btx  :linked t)
;(send br   :linked t)
;(send btr  :linked t)
;(send bxr  :linked t)
;(send btxr :linked t)

(send tt   :linked t)
(send ttx  :linked t)
(send ttr  :linked t)
(send ttxr :linked t)
(send c    :linked t)
(send cx   :linked t)
(send cr   :linked t)
(send cxr  :linked t)

(dolist (p (send btw :links))
	(send p :point-color 0 'green)
	(send p :point-color 1 'red)
	(send p :point-color 2 'cyan)
	(send p :point-color 5 'blue)
	(send p :point-color 6 'magenta)
	(send p :redraw)
)

