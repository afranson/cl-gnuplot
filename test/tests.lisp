;;;; tests.lisp - test suite for cl-gnuplot functionality

(in-package :cl-gnuplot/test)

(5am:def-suite plotting-suite)

(5am:in-suite plotting-suite)

(5am:test utilities
  (5am:is (equal '(0.0 1.0 2.0 3.0 4.0 5.0) (plt:linspace 0 5 :len 6)))
  (5am:is (equal '(5.0 4.0 3.0 2.0 1.0 0.0) (plt:linspace 5 0 :len 6)))
  (5am:is (equal '(0.0 1.0 2.0 3.0 4.0 5.0) (plt:linspace 0 5 :step 1)))
  (5am:is (equal '(2 3 4 5) (plt:linspace 2 5 :step 1 :type 'integer)))
  (5am:is (equal '(0 1 2 3 4 5) (plt:range 6)))
  (5am:is (equal '(4 5 6 7 8) (plt:range 4 9)))
  (5am:is (equal '((1 2) (3 4) (5 6)) (plt:transpose '((1 3 5) (2 4 6)))))
  (5am:is (equal nil (plt:transpose nil)))
  (5am:is (equal '((1 2) (3 4) (5 6)) (plt::partition 2 '(1 2 3 4 5 6))))
  (5am:is (equal nil (plt::partition 2 nil)))
  (5am:is (equal '(((1 4 14) (1 5 15) (1 6 16)) ((2 4 24) (2 5 25) (2 6 26)) ((3 4 34) (3 5 35) (3 6 36)) ((9 4 94) (9 5 95) (9 6 96)))
		 (plt::x-list-y-list-z-matrix-to-3d-data '(1 2 3 9) '(4 5 6) '((14 15 16) (24 25 26) (34 35 36) (94 95 96)))))
  (5am:is (equal '(((1 4 14) (1 5 15)) ((2 4 24) (2 5 25)) ((4 4 34) (4 5 35)) ((9 4 94) (9 5 95)))
		 (plt::x-list-y-list-z-matrix-to-3d-data '(1 2 4 9) '(4 5) '((14 15 16) (24 25 26) (34 35 36) (94 95 96)))))
  (5am:is (equal nil (plt::x-list-y-list-z-matrix-to-3d-data nil '(4 5 6) '((14 15 16) (24 25 26) (34 35 36)))))
  (5am:is (equal nil (plt::x-list-y-list-z-matrix-to-3d-data '(1 2 3) nil '((14 15 16) (24 25 26) (34 35 36)))))
  (5am:is (equal nil (plt::x-list-y-list-z-matrix-to-3d-data '(1 2 3) '(4 5 6) nil))))

(5am:test basic-plotting
  (5am:is-true (plt:reset))
  (5am:is-false (plt:send-plot-options :size "ratio 1" :lmargin "-1"))
  (5am:is-false (plt:plot "cos(x)" '(1 2 3) '(4 5 6) "w lp title 'plot test'" "sin(x)"))
  (5am:is-false (plt:plot-add '((1 3) (2 10) (3 -4)) "w lp title 'plot-add test'" '((1 5) (2 5) (3 5)) "w p ps 4 lc rgb 'black'"))
	  (5am:is-false (plt:send-plot-options-and-replot :xrange "[-1:4]" :yrange "[-10:10]" :xlabel "'Time'" :ylabel "'Millions of Dollars'" :xtics "0.5 offset -1,0 font ',16'" :key "bottom left title 'Example key with options' font ',15' font ',10' noopaque"))
  (5am:is-false (plt:send-strings "set grid noytics"))
  ;; (5am:is (string= (with-open-stream (s (make-string-output-stream))
;; 		     (setf *standard-output* s)
;; 		     (plt:help :log)
;; 		     (get-output-stream-string s))
;; 		   "
;; \" The `log(x)` function returns the natural logarithm (base `e`) of its
;;  argument.  See `log10`.

;; \" "))
  )

(5am:test 3d-plotting
  (5am:is-true (plt:reset))
  (5am:is-false (plt:send-plot-options :lmargin "4"))
  (5am:is-false (plt:plot "cos(2*x)+sin(2*y)+15 w pm3d"))
  (5am:is-false (plt:plot-add '(((0 0 3) (3 0 0) (5 0 -3)) ((0 2 0) (3 2 -3) (5 2 0)) ((0 6 -3) (3 6 0) (5 6 3))) "u 1:2:3 w pm3d title '3d matrix'"
			  '(0 3 5) '(0 2 6) '((0 1 2) (4 5 6) (9 10 11)) "u 1:2:3 w pm3d title 'vector-vector-2d matrix'"
			  "cos(x)**2+sin(y)**2-10 w pm3d"))
  (5am:is-false (plt:send-plot-options-and-replot :pm3d "depthorder" :zlabel "rotate parallel" :key "opaque")))

(5am:test multiplot
  (5am:is-true (plt:reset))
  (5am:is-false (plt:send-command :terminal "qt enhanced font 'arial,16' size 1920,1080"))
  (5am:is (equal '(nil nil nil nil) (plt:multiplot "layout 2,2 title 'Multiplot Title'"
						   (list '(1 2 3 4) "w lp title 'plot 1'" '(2 1 4 3) "w lp title 'Double plot!'"
							 :xlabel "'Amps'" :ylabel "'Fun'" :key "top left" :grid "")
						   (list '(10 25 30) "w lp title 'plot 2'"
							 :xlabel "'Bees'")
						   (list '(1 0 3 10) "w lp title 'plot 3'"
							 :xlabel "'Volts'" :key "unset")
						   (list '(-2 -4 -6 2) "w lp title 'plot 4'"
							 :xlabel "'Dogs'" :grid "unset" :mxtics "10")))))

(defun run-tests ()
  (5am:run! 'plotting-suite))

(export 'run-tests)
