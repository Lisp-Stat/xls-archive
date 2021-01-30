(defun logam (x) (declare (type double-float x))
 (prog
  ((w (make-array '(200) :element-type 'float)) (logam 0.0d0) (d 0.0d0)
   (z 0.0d0) (t_ 0.0d0) (n 0)
  )
  (declare (type (simple-array float (*)) w)) (declare (type real logam))
  (declare (type double-float d)) (declare (type double-float z))
  (declare (type double-float t_)) (declare (type fixnum n))
  (setq d 0.41893855) (replace w '(12.801827) :end 0)
  (replace w '(11.689333) :end 0) (replace w '(10.604603) :end 0)
  (replace w '(9.549267) :end 0) (replace w '(8.525162) :end 0)
  (replace w '(7.534364) :end 0) (replace w '(6.5792513) :end 0)
  (replace w '(5.662562) :end 0) (replace w '(4.787492) :end 0)
  (replace w '(3.957814) :end 0) (replace w '(3.1780539) :end 0)
  (replace w '(2.4537365) :end 0) (replace w '(1.7917595) :end 0)
  (replace w '(1.2009736) :end 0) (replace w '(0.6931472) :end 0)
  (replace w '(0.28468287) :end 0) (replace w '(0.0) :end 0)
  (replace w '((- 0.12078224)) :end 0) (replace w '(0.0) :end 0)
  (replace w '(0.5723649) :end 0) (replace w '(39.339886) :end 0)
  (replace w '(37.861088) :end 0) (replace w '(36.395447) :end 0)
  (replace w '(34.943317) :end 0) (replace w '(33.505074) :end 0)
  (replace w '(32.081116) :end 0) (replace w '(30.67186) :end 0)
  (replace w '(29.277754) :end 0) (replace w '(27.899271) :end 0)
  (replace w '(26.536915) :end 0) (replace w '(25.191221) :end 0)
  (replace w '(23.862766) :end 0) (replace w '(22.552164) :end 0)
  (replace w '(21.260077) :end 0) (replace w '(19.987215) :end 0)
  (replace w '(18.734348) :end 0) (replace w '(17.502308) :end 0)
  (replace w '(16.292) :end 0) (replace w '(15.104413) :end 0)
  (replace w '(13.940625) :end 0) (replace w '(71.25704) :end 0)
  (replace w '(69.569084) :end 0) (replace w '(67.88974) :end 0)
  (replace w '(66.21918) :end 0) (replace w '(64.55754) :end 0)
  (replace w '(62.90499) :end 0) (replace w '(61.261703) :end 0)
  (replace w '(59.627846) :end 0) (replace w '(58.003605) :end 0)
  (replace w '(56.389168) :end 0) (replace w '(54.78473) :end 0)
  (replace w '(53.190495) :end 0) (replace w '(51.606674) :end 0)
  (replace w '(50.033493) :end 0) (replace w '(48.47118) :end 0)
  (replace w '(46.91998) :end 0) (replace w '(45.38014) :end 0)
  (replace w '(43.851925) :end 0) (replace w '(42.335617) :end 0)
  (replace w '(40.8315) :end 0) (replace w '(106.63176) :end 0)
  (replace w '(104.796776) :end 0) (replace w '(102.9682) :end 0)
  (replace w '(101.14612) :end 0) (replace w '(99.33061) :end 0)
  (replace w '(97.521774) :end 0) (replace w '(95.719696) :end 0)
  (replace w '(93.92446) :end 0) (replace w '(92.13618) :end 0)
  (replace w '(90.35493) :end 0) (replace w '(88.580826) :end 0)
  (replace w '(86.81397) :end 0) (replace w '(85.05447) :end 0)
  (replace w '(83.30243) :end 0) (replace w '(81.55796) :end 0)
  (replace w '(79.82118) :end 0) (replace w '(78.092224) :end 0)
  (replace w '(76.3712) :end 0) (replace w '(74.65823) :end 0)
  (replace w '(72.95347) :end 0) (replace w '(144.56575) :end 0)
  (replace w '(142.61728) :end 0) (replace w '(140.67392) :end 0)
  (replace w '(138.73572) :end 0) (replace w '(136.80272) :end 0)
  (replace w '(134.87498) :end 0) (replace w '(132.95258) :end 0)
  (replace w '(131.03554) :end 0) (replace w '(129.12393) :end 0)
  (replace w '(127.21783) :end 0) (replace w '(125.31727) :end 0)
  (replace w '(123.42233) :end 0) (replace w '(121.53308) :end 0)
  (replace w '(119.649574) :end 0) (replace w '(117.77188) :end 0)
  (replace w '(115.90007) :end 0) (replace w '(114.03421) :end 0)
  (replace w '(112.17438) :end 0) (replace w '(110.32064) :end 0)
  (replace w '(108.473076) :end 0) (replace w '(184.53383) :end 0)
  (replace w '(182.49294) :end 0) (replace w '(180.4563) :end 0)
  (replace w '(178.42392) :end 0) (replace w '(176.39584) :end 0)
  (replace w '(174.37213) :end 0) (replace w '(172.3528) :end 0)
  (replace w '(170.33789) :end 0) (replace w '(168.32744) :end 0)
  (replace w '(166.3215) :end 0) (replace w '(164.32011) :end 0)
  (replace w '(162.3233) :end 0) (replace w '(160.33113) :end 0)
  (replace w '(158.34363) :end 0) (replace w '(156.36084) :end 0)
  (replace w '(154.38281) :end 0) (replace w '(152.40959) :end 0)
  (replace w '(150.44122) :end 0) (replace w '(148.47777) :end 0)
  (replace w '(146.51926) :end 0) (replace w '(226.19055) :end 0)
  (replace w '(224.07169) :end 0) (replace w '(221.95644) :end 0)
  (replace w '(219.84485) :end 0) (replace w '(217.73694) :end 0)
  (replace w '(215.63272) :end 0) (replace w '(213.53224) :end 0)
  (replace w '(211.43552) :end 0) (replace w '(209.34259) :end 0)
  (replace w '(207.25346) :end 0) (replace w '(205.1682) :end 0)
  (replace w '(203.0868) :end 0) (replace w '(201.00932) :end 0)
  (replace w '(198.93576) :end 0) (replace w '(196.86618) :end 0)
  (replace w '(194.8006) :end 0) (replace w '(192.73904) :end 0)
  (replace w '(190.68156) :end 0) (replace w '(188.62817) :end 0)
  (replace w '(186.57892) :end 0) (replace w '(269.2911) :end 0)
  (replace w '(267.1048) :end 0) (replace w '(264.92166) :end 0)
  (replace w '(262.7417) :end 0) (replace w '(260.56494) :end 0)
  (replace w '(258.39142) :end 0) (replace w '(256.22113) :end 0)
  (replace w '(254.05412) :end 0) (replace w '(251.8904) :end 0)
  (replace w '(249.73) :end 0) (replace w '(247.5729) :end 0)
  (replace w '(245.41919) :end 0) (replace w '(243.26884) :end 0)
  (replace w '(241.1219) :end 0) (replace w '(238.9784) :end 0)
  (replace w '(236.83832) :end 0) (replace w '(234.70172) :end 0)
  (replace w '(232.56862) :end 0) (replace w '(230.43904) :end 0)
  (replace w '(228.313) :end 0) (replace w '(313.65283) :end 0)
  (replace w '(311.4071) :end 0) (replace w '(309.16418) :end 0)
  (replace w '(306.9241) :end 0) (replace w '(304.68686) :end 0)
  (replace w '(302.45245) :end 0) (replace w '(300.22095) :end 0)
  (replace w '(297.9923) :end 0) (replace w '(295.7666) :end 0)
  (replace w '(293.5438) :end 0) (replace w '(291.32394) :end 0)
  (replace w '(289.10706) :end 0) (replace w '(286.89313) :end 0)
  (replace w '(284.68222) :end 0) (replace w '(282.4743) :end 0)
  (replace w '(280.2694) :end 0) (replace w '(278.06757) :end 0)
  (replace w '(275.8688) :end 0) (replace w '(273.67313) :end 0)
  (replace w '(271.48056) :end 0) (replace w '(359.13422) :end 0)
  (replace w '(356.8354) :end 0) (replace w '(354.5391) :end 0)
  (replace w '(352.24533) :end 0) (replace w '(349.95413) :end 0)
  (replace w '(347.66547) :end 0) (replace w '(345.3794) :end 0)
  (replace w '(343.09592) :end 0) (replace w '(340.81506) :end 0)
  (replace w '(338.5368) :end 0) (replace w '(336.26117) :end 0)
  (replace w '(333.9882) :end 0) (replace w '(331.7179) :end 0)
  (replace w '(329.45026) :end 0) (replace w '(327.1853) :end 0)
  (replace w '(324.92303) :end 0) (replace w '(322.6635) :end 0)
  (replace w '(320.4067) :end 0) (replace w '(318.15265) :end 0)
  (replace w '(315.90134) :end 0) (if (> x 100.0) (go label10))
  (setf n (+ (* 2.0 x) 0.1)) (setf logam (fref w n)) (go end_label) label10
  (setf t_ (expt (/ 1.0 x) 2))
  (setf z
   (/ (+ (* (+ (* (+ (* (* -1 0.75) t_) 1.0) t_) (- 3.5)) t_) 105.0)
    (* x 1260.0)
  ))
  (setf logam (+ (+ d z) (* (+ x (- 0.5)) (+ (alog x) (- 1.0)))))
  (go end_label) end_label (return logam)
))
