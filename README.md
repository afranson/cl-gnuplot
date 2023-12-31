# cl-gnuplot

Interact with gnuplot via common lisp in a simple, intuitive manner. Made specifically for easy plotting of 2d and 3d data.

Key Features:
* Vast majority of gnuplot functionality (2d, 3d, builtins, send data, read files, etc.) via just the ```(plt:plot ...)``` command
* Use the full power of Lisp to read, generate, and analyze data then easily throw it straight into ```(plt:plot ...)```
* Combine data and settings into one function call
* Rearrange and add plots after initial ```(plt:plot ...)``` call
* Save plots via ```(plt:save-last-plot terminal-info filename)```

# Quick Example
Load data with plt:basic-read-file, plot functions by providing x data and a function, throw it all into one call to ```(plt:plot ...)``` with all your settings (e.g. ```:grid "x mx"``` to set or ```:grid "unset"``` to unset) and you're off to the races. ```(plt:help :grid)``` will print gnuplot help info for grid (or any other command). ```(plt:show :grid)``` prints the current state of grid (or any other command).
```
(require 'cl-gnuplot)
(plt:reset)
(plt:plot (plt:basic-read-file "./resources/quick-example-file.txt" :manual-delim #\tab) "w lp title 'Sample 1'"
          (plt:linspace 40 960 :len 400) (lambda (x) (/ (* 200 1.16d6) (+ (expt (- x 635) 2) 200))) "w p pt 6 ps 3 title 'Eyeballing'"
          :yrange "[*:2e6]"
          :xlabel "'Raman Shift (1/cm)'"
          :ylabel "'Counts (a.u.)'"
          :grid "x mx"
          :xtics "0,100,1000"
          :mxtics "4")

(plt:save-last-plot "pngcairo lw 4 font ',40' size 1920,1080" "./resources/quick-example.png")
```
![quick-example](./resources/quick-example.png "Quick Data Loading and 2D Plotting")

# 2D Plotting
Default is "with linespoints" ("w lp"). You must use a format string with "with lines" ("w l") or "with points" ("w p") do just do one or the other.

Below is an example of all the different accepted ways to make a 2d plot by the 'plot' function.
* xy pairs list
* x-list y-list
* x-list function
* y-list
* gnuplot builtin, e.g. "airy(x)"
* direct file read
```
(require 'cl-gnuplot)
(plt:reset)
;; Data w/o format strings for quick plotting
(plt:plot '((1 2) (3 5) (5 7) (6 2) (7 5) (10 7) (14 8)) ;; 2d data as pairs of xy data
          '(1 3 5 6 7 10 14) '(1 2 3 4 5 6 7) ;; 2d data as a list of x and a list of y data
          (plt:linspace 0 13 :step 0.5) (lambda (x) (- 10 (/ 10 (+ 2 (expt (- x 7) 2))))) ;; 2d data as a list of x data followed by a function
          ;; 1d data all by itself (with no format string) can only appear as the last data item
          '(10 9 8 7 6 5 4 3 2 1) ;; 2d data as just a list of y data (assuming 0,1,2... as x data)
          )

;; Data with format strings (can also mix and match)
(plt:plot :terminal "qt lw 4 font ',40' size 1920,1080"
          plt::example-2d-data "w lp title 'strings after data'"
          (plt:linspace 0 13 :step 0.5) (lambda (x) (- 10 (/ 10 (+ 2 (expt (- x 7) 2))))) "w lp title 'from function'"
          '(10 9 8 7 6 5 4 3 2 1) "w lp title 'And rearrange them later'"
          '(1 3 5 6 7 10 14) '(1 2 3 4 5 6 7) "w lp title 'You can also put format'"
          "7+sin(x) w lp title 'gnuplot''s sin(x)'"
          "'./resources/2d-plotting-example.txt' w lp title 'from file'"
          :ylabel "'And add extra commands'"
          :xlabel "'before and after'")
(plt:plot-add '((2 6) (6 8) (10 5)) "w lp title 'And add data later'"
              :key "bottom right font ',25'")

(plt:rearrange-plots '(3 0 2 6 1 5 4)) ;; Shuffle the plots into a more desirable order
(plt:resend-plots) ;; Different than (plt:replot) which just sends "replot" to gnuplot. This resends all plot information.

(plt:save-last-plot "pngcairo lw 4 font ',40' size 1920,1080" "./resources/2d-plotting.png")
```
![2d-plotting](./resources/2d-plotting.png "2D Plotting")

## More Specialized 2D Plotting
For reference. Forgive the compactness.
* Candlestick
* Histogram
* Multiaxis
* Parametric
* Piecewise
* Box Plot
* Rug Plot
* Spider Plot
* Error Bars
```
(require 'cl-gnuplot)
(plt:reset)
(plt:save-plot 
    ("pngcairo lw 1 font ',12' size 1920,1080" "./resources/niche-2d.png") 
	;;(plt:send-plot-options :terminal "qt lw 1 font ',12' size 1920,1080")
	(plt:string-code-to-2d)
	(plt:multiplot "layout 3,3 title 'Specialized 2D Plots'"
                   (list :title "'Candlestick'" '((1 3 0 6 5) (2 5 0 5 4.5) (3 4.5 3 8 6)) "w candle title 'XMR'"
                         :mxtics "default" :xtics "auto" :xrange "[0:4]" :boxwidth "0.2" :xlabel "'Time'" :ylabel "'$$$'" :key "top left" :grid "")
                   (list :title "'Histogram'" :style "histogram cluster gap 1" :style "data histogram" :style\ fill "pattern 1"
                         '(4 2 9 7) "u 1 ti 'histo 1'" '(9 8 2 4) "u 1 ti 'histo 2'" '(4 4 5 5) "u 1 ti 'histo 3'"
                         :boxwidth "1" :ylabel "'Stigs'" :xrange "[-1:4]" :yrange "[0:12]" :xtics "('before' 0, 'during' 1, 'after' 2, 'GT' 3)")
                   (list :title "'Multiaxis'" '(1 0 3 10) "w lp ti 'small axis' axes x1y1" '(1000 700 200 0) "w lp ti 'large axis' axes x1y2" :xlabel "'Volts'" :ytics "nomirror" :y2tics "" :ylabel "'Current 1'" :y2label "'Current 2'" :grid "")
                   (list :title "'Parametric Plot'" :parametric "" "cosh(t),sin(t) w lp title 'cosh(t),sin(t)'" :xlabel "'X'" :ylabel "'Y'" :grid "")
                   (list :title "'Piecewise'" "sample [*:1] cos(x), [1:3] 0.3*sin(x*x), [3:*] airy(-x) w lp" :yrange "[-1.5:1.5]" :xlabel "'Angle'" :ylabel "'a.u.'")
                   (list :title "'Box Plot'" :style "boxplot outliers pt 7" '(1 2 3 4 5 6 11) "u (0):1 w boxplot notitle" '(2 4 5 6 3 11 2 4) "u (1):1 w boxplot notitle" :xtics "('A' 0, 'B' 1)")
                   (list :title "'Rugplot'" '(1 2 2.3 2.4 2.5 2.55 2.8 3.2 4) '(5 4 4.3 4.9 3.5 4.2 5.3 4.1 4) "u 1:2:xtic(\"\"):ytic(\"\") w p ps 3 notitle" :xtics "out scale 2" :ytics "out scale 2" :xrange "[0:5]" :yrange "[3:6]")
                   (list :title "'Spiderplot'" :spiderplot "" :style "spiderplot fs transparent solid border lw 3.0" :style "data spiderplot" :for "[i=1:5] paxis i range [0:10]" :paxis "2 label offset 0.2,0.4" :paxis "5 label offset -0.2,0.4" :paxis "1 tics" :for "[i=2:5] paxis i tics format \"\""  '(\"STR\" 4 1) "title columnhead" '(\"WIS\" 9 5) "title columnhead" '(\"INT\" 1 8) "title columnhead" '(\"CHR\" 1 6) "title columnhead" '(\"STA\" 4 3) "title columnhead" :grid "")
                   (list :title "'Points with Error Bars'" '((250 3 12 0.4) (275 4 5 0.1) (300 5 1 1)) "w xyerrorbars title 'That''s life'" :xrange "[225:325]" :yrange "[0:10]" :xlabel "'Temperature (K)'" :ylabel "'Conductivity (S/m)'")))
```
![niche-2d-plotting](./resources/niche-2d.png "Specialized 2D Plotting")

# 3D Plotting
Default is "with pm3d". You must use a format string "with lines" ("w l") to do wireframe style plots.

Below are all the ways to make a 3d plot via the 'plot' function.
* xyz-matrix
* x-list y-list 2d-z-matrix
* x-list y-list function
* xy-matrix function
* gnuplot builtin, e.g. "airy(x) + cos(y)"
* direct file read
```
(require 'cl-gnuplot)
(plt:reset)
(plt:string-code-to-3d) ;; Hopefully temporary, forces pure string inputs (builtin, direct read) to use splot
(plt:plot :terminal "qt lw 4 font ',25' size 1920,1080"
          '(((0 0 15) (1 0 20) (2 0 10))
            ((0 1 10) (1 1 15) (2 1 10))
            ((0 2 15) (1 2 10) (2 2 10))) "w l pal title 'Top 3d'" ;; 3d xyz-matrix
          '(-2 0 2) '(0 2 4) (lambda (x y) (+ (sin x) (cos y))) "w lp ps 2 pal title 'function'" ;; x-list y-list function
          '(((1.5 -3) (1.9 -2.9)) ((1.6 -2.1) (2.0 -2))) (lambda (x y) (+ (cos x) (sin y))) "w pm3d pal title 'function 2'" ;; 3d xy-matrix function
          '(-1 0 1) '(-2 0 2) 
          '((7 6 5) (6 5 4) (5 4 3)) "w l pal title 'Next 1d-1d-2d'" ;; x-list y-list 2d-z-matrix
          "-10 + 3*(cos(5*x) - sin(5*y)) w l pal title 'builtins'" ;; gnuplot builtins
          "'./resources/quick-3d-data.txt' w lp ps 2 pal title 'direct read'" ;; direct file read
          :pm3d "border"
          :hidden3d ""
          :xlabel "'X'" :ylabel "'Y'" :zlabel "'Z'" :cblabel "'cb' offset 0,1" :colorbox "horizontal user origin 0.2,0.9 size 0.5,0.03"
          :xtics "offset 0,-0.3"
          :view "60,30")
		  
(plt:rearrange-plots '(0 3 1 2 4 5))
(plt:resend-plots)

(plt:save-last-plot "pngcairo lw 4 font ',25' size 1920,1080" "./resources/3d-plotting.png")
```
![3d-plotting](./resources/3d-plotting.png "3D Plotting")

## 3D Data Loading and Heatmaps
This can be done via a direct read (with string-code-to-3d) or ```(plt:basic-read-file ...)``` and ```(plt:partition ...)```. Option 2 is great when you need to get the data and do manipulations like derivatives, background subtraction, scaling, etc.
```
(require 'cl-gnuplot)
(plt:reset)

(plt:plot :terminal "qt lw 4 font ',25' size 1920,1080" 
          (plt:partition 81 (plt:basic-read-file "./resources/3d-data-example.txt")) "u 1:2:8 w pm3d notitle"
          :pm3d "at sb depthorder"
          :view "60,30"
          :xlabel "'X'" :ylabel "'Y'" :cblabel "'Z (a.u.)'"
          :ztics "unset" :border "15" :xtics "offset 0,-0.25")
(plt:save-last-plot "pngcairo lw 4 font ',25' size 1920,1080" "./resources/3d-file-plotting.png")
```
![3d-file-plotting](./resources/3d-file-plotting.png "3D File Plotting and Heatmap")

# Multiplot
Multiplot works by wrapping multiple plots inside one function call. The 'layout' option allows for easy position of plots on a grid. You can also forgo using 'layout' and manually position plots via the 'origin' and 'size' options in the individual plot arguments. Besides that, everything in the 4 plots below is the exact same syntax as what would be given to a ```(plt:plot ...)``` call like above.

```(plt:save-last-plot ...)``` does not work with multiplot. Instead, the macro ```(plt:save-plot ...)``` is wrapped around the function. ```(plt:save-plot ...)``` will also work with any plots that do not 'replot' properly (```(save-last-plot ...)``` uses ```(replot)``` to capture the last plot).
```
(require 'cl-gnuplot)
(plt:reset)
(plt:save-plot
    ("pngcairo lw 4 font ',25' size 1920,1080" "./resources/multiplot.png") 
    (plt:multiplot "layout 2,2 title 'Multiplot Title'"
                   (list '(1 2 3 4) "w lp title 'plot 1'" '(2 1 4 3) "w lp title 'Double plot!'"
                         :xlabel "'Amps'" :ylabel "'Fun'" :key "top left" :grid "")
                   (list '(10 25 30) "w lp title 'plot 2'" :xlabel "'Bees'" :key "bottom right")
                   (list '(1 0 3 10) "w lp title 'plot 3'" :xlabel "'Volts'" :key "unset")
                   (list '(-2 -4 -6 2) "w lp title 'plot 4'" :xlabel "'Dogs'" :grid "unset" :mxtics "10")))
```
![multiplot-plotting](./resources/multiplot.png "Multiplotting")

# Typical Commands

```
;;; Loading
(require 'cl-gnuplot) ;; nicknamed plt ;; Automatically spins up a gnuplot process

;;; Inquiring
(plt:help :xtics)
(plt:show :terminal\ pngcairo)

;; Command syntax
;;     equivalent to: "set grid" "unset xtics" "unset ytics" "set yrange [0:10]"      "#set title '#foobar'" (comment)
(plt:send-plot-options :grid "" :xtics "unset" :ytics null :string "set yrange [0:10]" :title "#'foobar'")
;;     = to:          "set xtics 10,3,30"       "array A[100]; A[1] = 10; print A[1]"
(plt:send-plot-options :xtics "10,3,30" :string "array A[100]; A[1] = 10; print A[1]")

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
(plt:linspace 0 10 :step 1 :type 'integer)
(plt:range 0 -5)
(plt:transpose (plt:range 5) '(3.4 6.7 -2.3 4.5))
```

# Just Let Me Use Gnuplot Syntax Dangit
```
(plt:send-strings (list "set terminal qt enhanced font 'arial,30' size 1920,1080"
                        "set xlabel 'Proof of Concept' font ',20'"
                        "plot './resources/quick-example-file.txt' u 1:2 w lp title 'Override'"))
```
or
```
(plt:plot :string "set terminal qt enhanced font 'arial,30' size 1920,1080"
          :string "set xlabel 'Proof of Concept' font ',20'"
          :string "plot './resources/quick-example-file.txt' u 1:2 w lp title 'Override'"
          :string "plot 5e5*sin(x/50)+5e5"
          :string "f(x) = 200 * 1.16e6 / (200 + (x - 635)**2)"
          :string "array xs[400]; do for [i=0:399] {xs[i+1] = 40.0+(960-40)/399.0*i}"
          :string "plot [i=1:400:1] '+' u (xs[i]):(f(xs[i])) w p pt 6 ps 3 title 'Eyeballing'")
```

## License

GPLv3

## TODO

TODO: Allow for entry of data as datablocks (```$DBLOCK << EOD; ...bunch of data...; EOD```), for embedding in gnuplot files (via ```(plt:save-gnuplot-script)```) in addition to currently supported '-' style.
