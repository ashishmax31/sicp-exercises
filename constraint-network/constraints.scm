(define (adder a1 a2 sum)
    (define (process-new-value)
        (cond ((and (has-value? a1) (has-value? a2)) (set-value! sum (+ (get-value a1) (get-value a2)) me))
              ((and (has-value? a1) (has-value? sum)) (set-value! a2  (- (get-value sum) (get-value a1)) me))
              ((and (has-value? a2) (has-value? sum)) (set-value! a1  (- (get-value sum) (get-value a2)) me))))

    (define (forget-value)
        (forget-value! sum me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))

    (define (me request)
        (cond ((eq? request 'new-value-present ) (process-new-value))
              ((eq? request 'inform-about-no-value ) (forget-value))
              (else (error "Unknown operation!"))))
              
    (connect a1 me)
    (connect a2 me)
    (connect sum me))

(define (multiplier a1 a2 product)
    (define (process-new-value)
        (cond ((and (has-value? a1) (has-value? a2)) (set-value! product (* (get-value a1) (get-value a2)) me))
              ((and (has-value? a1) (has-value? product)) (set-value! a2  (/ (get-value product) (get-value a1)) me))
              ((and (has-value? a2) (has-value? product)) (set-value! a1  (/ (get-value product) (get-value a2)) me))))

    (define (forget-value)
        (forget-value! product me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))

    (define (me request)
        (cond ((eq? request 'new-value-present ) (process-new-value))
              ((eq? request 'inform-about-no-value ) (forget-value))
              (else (error "Unknown operation!"))))
              
    (connect a1 me)
    (connect a2 me)
    (connect product me))


(define (divide a1 a2 result)
    (define (process-new-value)
        (cond ((and (has-value? a1) (has-value? a2)) (set-value! result (/ (get-value a1) (get-value a2)) me))
              ((and (has-value? a1) (has-value? result)) (set-value! a2  (/ (get-value a1) (get-value result)) me))
              ((and (has-value? a2) (has-value? result)) (set-value! a1  (* (get-value result) (get-value a2)) me))))

    (define (forget-value)
        (forget-value! result me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))

    (define (me request)
        (cond ((eq? request 'new-value-present ) (process-new-value))
              ((eq? request 'inform-about-no-value ) (forget-value))
              (else (error "Unknown operation!"))))
              
    (connect a1 me)
    (connect a2 me)
    (connect result me))


(define (constant value connector)
    (define (me request)
        (error "Unknown operation!"))

    (connect connector me)
    (set-value! connector value me))

(define (squarer a b)
    (define (square a)
        (* a a))
    
    (define (process-new-value)
        (if (has-value? a)
            (set-value! b (square (get-value a)) me)
            (if (has-value? b)
                (if (< (get-value b) 0)
                    (error "Square root of a negative number!")
                    (set-value! a (sqrt (get-value b)) me))
                'ignored )))
    
    
    
    (define (forget-value)
        (forget-value! a me)
        (forget-value! b me)
        (process-new-value))
        
    (define (me request)
        (cond ((eq? request 'new-value-present ) (process-new-value))
              ((eq? request 'inform-about-no-value ) (forget-value))
              (else (error "Unknown operation!"))))
    (connect a me)
    (connect b me))


    


(define (inform-about-new-value constraint)
    (constraint 'new-value-present ))

(define (inform-about-no-value constraint)
    (constraint 'inform-about-no-value ))