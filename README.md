# cl-gnuplot

	Interact with gnuplot via common lisp in a simple, intuitive manner. Made specifically for easy plotting of 2d and 3d data.

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

;;; 3D plottin
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
						   (list '(10 25 30) "w lp title 'plot 2'"
							 :xlabel "'Bees'")
						   (list '(1 0 3 10) "w lp title 'plot 3'"
							 :xlabel "'Volts'" :key "unset")
						   (list '(-2 -4 -6 2) "w lp title 'plot 4'"
							 :xlabel "'Dogs'" :grid "unset" :mxtics "10"))
							 
;;; Saving
(plt:save-plot "pngcairo" "myfilename.png"
    (plt:plot plt::example-2d-data))
(plt:save-gnuplot-script "myscript.gp")
(plt:load-gnuplot-script "myscript.gp")
(plt:replot)

;;; Panicking
(plt:restart-gnuplot)
(plt:help-cl-gnuplot)
```

# Just let me use Gnuplot syntax dangit
```
(plt:send-strings (list "set terminal qt enhanced font 'arial,16' size 1920,1080"
                        "plot 'mydata.txt' w lp u 1:2 title 'override'"
						"set xlabel 'Booyah' font ',300'"))
```

## License

GPLv3

