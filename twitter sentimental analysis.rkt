#lang racket
(require data-science-master)
(require csv-reading)
(require plot)
(require math)

(define file-path "C:/Users/LENOVO/Desktop/SICP project/dataset/uganda twitter data.csv")

(define (read-csv file-path
		  #:->number? [->number? #f]
		  #:header? [header? #t]
                  )
  (let ((csv-reader (make-csv-reader-maker
                     '((separator-chars #\|)
                       (comment-chars #\#))))) 
    (with-input-from-file file-path
      (lambda ()
	(let* ((tmp (csv->list (csv-reader (current-input-port)))))
	  (if ->number?
	     
	      (if header?
		  (cons (car tmp) (map (lambda (x) (map string->number x)) (cdr tmp)))
		  (map (lambda (x) (map string->number x)) tmp))
	      tmp))))))



(define data (read-csv file-path 
                       
                       )
  )

(define testing
  (let ([tmp (map (λ (x) (list (list-ref x 0))) data)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))
(define f_sample_data (flatten testing))

(define full_tweet (apply string-append f_sample_data))


(define tweets (string-normalize-spaces
		   (remove-punctuation
		    (string-downcase full_tweet) #:websafe? #t)))



(define frame (document->tokens tweets #:sort? #t))


(define sentiment (list->sentiment frame #:lexicon 'nrc))

 
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 1000))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "LightSeaGreen"
	    #:line-color "OrangeRed"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))

(define sentiment_p (list->sentiment frame #:lexicon 'bing))
(parameterize ([plot-height 400])
  (plot (discrete-histogram
	 (aggregate sum ($ sentiment_p 'sentiment) ($ sentiment_p 'freq))
	 #:y-min 0
	 #:y-max 120000
	 #:invert? #t
	 #:color "OrangeRed"
	 #:line-color "LightSeaGreen")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))

