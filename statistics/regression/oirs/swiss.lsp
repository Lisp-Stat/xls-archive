(def agriculture '(17.0 45.1 39.7 36.5 43.5 35.3 70.2 67.8 53.3 45.2 64.5 62.0 67.5 60.7 69.3 72.6 34.0 19.4 15.2 73.0 59.8 55.1 50.9 54.1 71.2 58.1 63.5 60.8 26.8 49.5 85.9 84.9 89.7 78.2 64.9 75.9 84.6 63.1 38.4 7.7 16.7 17.6 37.6 18.7 1.2 46.6 27.7))
(def exam '(15 6 5 12 17 9 16 14 12 16 14 21 14 19 22 18 17 26 31 19 22 14 22 20 12 14 6 16 25 15 3 7 5 12 7 9 3 13 26 29 22 35 15 25 37 16 22))
(def education '(12 9 5 7 15 7 7 8 7 13 6 12 7 12 5 2 8 28 20 9 10 3 12 6 1 8 3 10 19 8 2 6 2 6 3 9 3 13 12 11 13 32 7 7 53 29 29))
(def catholic '(10.0 84.8 93.4 33.8 5.2 90.6 92.9 97.2 97.7 91.4 98.6 8.5 2.3 4.4 2.8 24.2 3.3 12.1 2.2 2.8 5.2 4.5 15.1 4.2 2.4 5.2 2.6 7.7 18.5 6.1 99.7 99.7 100.0 99.0 98.2 99.1 99.5 96.8 5.6 13.8 11.2 16.9 5.0 8.6 42.3 50.4 58.3))
(def infant '(22.2 22.2 20.2 20.3 20.6 26.6 23.6 24.9 21.0 24.4 24.5 16.5 19.1 22.7 18.7 21.2 20.0 20.2 10.8 20.0 18.0 22.4 16.7 15.3 21.0 23.8 18.0 16.3 20.9 22.5 15.1 19.8 18.3 19.4 20.2 17.8 16.3 18.1 20.3 20.5 18.9 23.0 20.0 19.5 18.0 18.2 19.3))
(def fertility '(80.2 83.1 92.5 85.8 76.9 76.1 83.8 92.4 82.4 82.9 87.1 64.1 66.9 68.9 61.7 68.3 71.7 55.7 54.3 65.1 65.5 65.0 56.6 57.4 72.5 74.2 72.0 60.5 58.3 65.4 75.5 69.3 77.3 70.5 79.4 65.0 92.2 79.3 70.4 65.7 72.7 64.4 77.6 67.6 35.0 44.7 42.8))
(setf x (list agriculture exam education catholic infant))
(setf y fertility)
(def varnames '("agriculture" "exam" "education" "catholic" "infant"))
(def respname "swiss")