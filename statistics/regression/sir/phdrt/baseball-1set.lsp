
  (load "baseball-256")

  (load "baseball")


  (def x1 (select baseball (iseq 0 15)))

  (def y1 (log (nth 16 baseball)))

  (def x(subset x1 baseball-256))
;  (def x(select x '(3 4 5 6 7 8 9 10 11 12)))

  (def y(select y1 baseball-256))
