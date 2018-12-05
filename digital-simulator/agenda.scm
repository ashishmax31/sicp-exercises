(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda))
                    action
                    the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-action (first-item-from-agenda the-agenda)))
             (first-action)
             (remove-first-action-from-agenda! the-agenda)
             (propagate))))
