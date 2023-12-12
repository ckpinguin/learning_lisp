(setq my-hand '((3 hearts) (5 clubs) (2 diamonds)
                          (1 spades) (4 diamonds)))
(setq colors '((clubs black) (diamonds red) (hearts red)
                             (spades black)))
(setq all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun rank (card) (first card))

(defun suit (card) (second card))

(defun count-suit (s hand)
  (length (remove-if-not
            #'(lambda (card)
                (eq (suit card) s))
            hand )))

(defun color-of (cards)
  (second (assoc (suit cards) colors)))

(defun first-red (hand)
  (find-if #'(lambda (card) 
               (eq (color-of card) 'red)) hand))

(defun black-cards (hand)
  (remove-if-not #'(lambda (card)
                     (eq (color-of card) 'black)) hand))

(defun what-ranks (s hand)
  (mapcar #'rank
         (remove-if-not #'(lambda (card)
                            (eq (suit card) s)) hand)))
(defun beforep (x y l)
  "Returns true if X appears before Y in L.
  This makes sorting possible without a natural
  order but based on a given order list"
  (member y (member x l)))

(defun higher-rank-p (card1 card2)
  (beforep (rank card1)
           (rank card2) 
           all-ranks))

(defun high-card (hand)
  (assoc (find-if
           #'(lambda (r)
               (assoc r hand))
           (reverse all-ranks))
         hand))

(defun high-card2 (hand)
  (reduce
    #'(lambda (card1 card2)
        (if (higher-rank-p card1 card2)
          card1
          card2))
    hand))

