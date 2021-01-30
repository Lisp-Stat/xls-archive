;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Demo.lsp
;;  Stat. Reasearch Institute of Sung Kyun Kwan University,
;;                   July 10 1997
;;  Programmed by Lee Kyungmi, & Huh, MoonYul
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def beer '((144  15  4.7  .43)
            (151  19  4.9  .43)
            (157  15  4.9  .48)
            (170   7  5.2  .73)
            (152  11  5.0  .77)
            (145  23  4.6  .28)
            (175  24  5.5  .40)
            (149  27  4.7  .42)
            ( 99  10  4.3  .43)
            (113   8  3.7  .44)
            (140  18  4.6  .44)
            (102  15  4.1  .46)
            (135  11  4.2  .50)
            (150  19  4.7  .76)
            (149   6  5.0  .79)
            ( 68  15  2.3  .38)
            (136  19  4.4  .43)
            (144  24  4.9  .43)
            ( 72   6  2.9  .46)
            ( 97   7  4.2  .47)))

(def obsnames (list "Budweiser" "Schlitz" "Lowenbrau" "kronenbourg" 
                    "Heineken"  "Old Milwaukee"  "Augsberger"  
                    "Strohs Bohemian style" "Miller lite" "Budweiser Light" 
                    "Coors" "Coors light" "Michelob light"  "Becks" "Kirin" 
                    "Pabst Extra light" "Hamms" "Heilemans Old style" 
                    "Olympia gold light" "Schlitzm light"))

(def varnames (list  "Calories" "Sodium" "Alcohol" "Cost"))


(dendrogram beer :linkage "average" :proximity "euclidean" :draw t)