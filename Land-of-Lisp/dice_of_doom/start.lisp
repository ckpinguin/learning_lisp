(load "webserver")
(load "dice_of_doom_4")

(defun startit ()
  (serve #'dod-request-handler)
  )
