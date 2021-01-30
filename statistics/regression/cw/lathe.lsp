(DEF LATHE (QUOTE ((-1 -1 1 1 -1 -1 1 1 0 0 -1.414 1.414 0 0 0 0 0 0 0 0) (-1 -1 -1 -1 1 1 1 1 -1.414 1.414 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 0 0 1.999396 1.999396 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 1.999396 1.999396 0 0 0 0 0 0 0 0 0 0) (1 1 -1 -1 -1 -1 1 1 0 0 0 0 0 0 0 0 0 0 0 0) (3.998201 4.189655 2.4681 2.639057 1.648659 1.098612 -0.2231436 -0.6931472 4.460144 -0.9162907 3.00072 1.064711 1.335001 0.7884574 1.163151 1.386294 1.029619 1.163151 1.386294 1.252763))))

(setf names '("Speed" "Feed" "Speed^2" "Feed^2" "Speed*Feed" "Life"))

(setf lathe-reg (regression-model (rmel 5 lathe) (nth 5 lathe)
	:predictor-names (rmel 5 names)
	:response-name (nth 5 names)))

(send lathe-reg :graphics-menu "Lathe-data")
