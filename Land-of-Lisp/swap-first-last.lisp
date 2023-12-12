(defun swap-first-last (x)
    (let* ((a (reverse (rest x)))
                    (b (reverse (rest a))))
        (cons (first a) (append b (list (first x))))))
