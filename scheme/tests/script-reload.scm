;; send 100 presses of button 1
;; for benchmarking amaya's reload
;;
;;(bind-key 'all "C-S-M-b" (lambda ()
;;			   (let loop ((i 0))
;;			     (send-button-press 1)
;;			     (if (< i 100)
;;				 (loop (+ 1 i))))))

;; 18 seconds
;; vs. 25 seconds
