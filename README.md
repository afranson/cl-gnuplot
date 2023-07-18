# cl-gnuplot

Interact with gnuplot via common lisp in a simple, intuitive manner. Made specifically for easy plotting of 2d and 3d data.

# Quick Example
```
(require 'cl-gnuplot)
(plt:plot (plt:basic-read-file "./resources/quick-example-file.txt" :manual-delim #\tab) "w lp title 'Sample 1'"
          :yrange "[*:2e6]"
          :xlabel "'Raman Shift (1/cm)'"
          :ylabel "'Counts (a.u.)'"
          :grid "x mx"
          :xtics "0,100,1000"
          :mxtics "4")
(plt:plot-function :function (lambda (x) (/ (* 200 1.16d6) (+ (expt (- x 635) 2) 200)))
                   :x (plt:linspace 40 960 :len 400)
                   :plot-format "w p pt 6 ps 3 title 'Eyeballing'"
                   :add t)
(plt:save-last-plot "pngcairo lw 4 font ',40' size 1920,1080" "./resources/quick-example.png")
```
![quick-example](./resources/quick-example.png "Quick Data Loading and Plotting")

# 2D Plotting
```
(require 'cl-gnuplot)
(plt:plot '((1 2) (3 5) (5 7) (6 2) (7 5) (10 7) (14 8)) ;; 2d data as pairs of xy data
          '(1 3 5 6 7 10 14) '(1 2 3 4 5 6 7) ;; 2d data as a list of x and a list of y data
		  '(10 9 8 7 6 5 4 3 2 1) ;; 2d data as just a list of y data (assuming 0,1,2... as x data)
		  )
(plt:plot plt::example-2d-data "w lp title 'You can also'"
          '(10 9 8 7 6 5 4 3 2 1) "w lp title 'put format strings'"
		  '(1 3 5 6 7 10 14) '(1 2 3 4 5 6 7) "w lp title 'after data'"
		  :ylabel "'And extra commands'"
          :xlabel "'before and after'")
(plt:save-last-plot "pngcairo lw 4 font ',40' size 1920,1080" "./resources/2d-plotting.png")
```
![2d-plotting](./resources/2d-plotting.png "2D Plotting")

# 3D Plotting
```

```

# Typical Commands

```
;;; Loading
(require 'cl-gnuplot) ;; nicknamed plt ;; Automatically spins up a gnuplot process

;;; Inquiring
(plt:help :xtics)
(plt:show :terminal\ pngcairo)

;;; 2D plotting
(plt:reset)
(plt:send-plot-options :terminal "qt enhanced font 'arial,32' size 1920,1080")
(plt:plot "cos(2*x)+3" '(1 2 3) '(4 5 6) :xrange "[*:4]" :yrange "[0:7]" :grid "" :key "bottom left opaque")
(plt:plot-add '(2 3 4) "w lp title 'added line'")

;;; 3D plotting
(plt:reset)
(plt:send-plot-options :terminal "qt enhanced font 'arial,32' size 1920,1080")
(plt:plot "cos(2*x)+sin(2*y)+15 w pm3d")
(plt:plot-add '(((0 0 3) (3 0 0) (5 0 -3)) ((0 2 0) (3 2 -3) (5 2 0)) ((0 6 -3) (3 6 0) (5 6 3))) "u 1:2:3 w pm3d title '3d matrix'"
	'(0 3 5) '(0 2 6) '((0 1 2) (4 5 6) (9 10 11)) "u 1:2:3 w pm3d title 'vector-vector-2d matrix'")
(plt:send-plot-options-and-replot :pm3d "depthorder" :zlabel "rotate parallel" :key "opaque")

;;; Multiplot
(plt:reset)
(plt:multiplot "layout 2,2 title 'Multiplot Title'"
	(list '(1 2 3 4) "w lp title 'plot 1'" '(2 1 4 3) "w lp title 'Double plot!'"
		  :xlabel "'Amps'" :ylabel "'Fun'" :key "top left" :grid "")
	(list '(10 25 30) "w lp title 'plot 2'" :xlabel "'Bees'")
	(list '(1 0 3 10) "w lp title 'plot 3'" :xlabel "'Volts'" :key "unset")
	(list '(-2 -4 -6 2) "w lp title 'plot 4'" :xlabel "'Dogs'" :grid "unset" :mxtics "10"))
							 
;;; Saving
(plt:plot plt::example-3d-data)
(plt:save-last-plot "pngcairo" "myfilename.png")
(plt:save-plot "pngcairo" "myfilename.png"
    (plt:plot plt::example-2d-data))
(plt:save-gnuplot-script "myscript.gp")
(plt:load-gnuplot-script "myscript.gp")
(plt:replot)

;;; Panicking
(plt:restart-gnuplot)
(plt:help-cl-gnuplot)

;;; Useful Utilities
(plt:basic-read-file "filename")
(plt:plot-function :function (lambda (x) (/ (x + 4)) :x (plt:linspace 0 10 :len 100))
(plt:linspace 0 10 :step 1 :type 'integer)
(plt:range 0 -5)
(plt:transpose (plt:range 5) '(3.4 6.7 -2.3 4.5))
```

# Just Let Me Use Gnuplot Syntax Dangit
```
(plt:send-strings (list "set terminal qt enhanced font 'arial,16' size 1920,1080"
                        "plot 'mydata.txt' w lp u 1:2 title 'override'"
	                    "set xlabel 'Booyah' font ',300'"))
```

## License

GPLv3
