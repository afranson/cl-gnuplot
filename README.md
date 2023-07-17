# cl-gnuplot
### _Andrew Franson_

# Typical Commands

Maybe just plt:set :view "map"
Maybe instead of plot-spec, plot-script
Maybe instead of update and modify, 

```
(require 'cl-gnuplot) ;; nicknamed to plt
;; by default initializes the global plotting specification (plot-spec)
;; defaults are listed in the 'plt:create-plot-spec' function

(plt:plot '(0 1 2 3 4 5 6 7 8 9 10) '(1 0 1 0 2 0 4 2 0 10 5) "'-' w lp title \"OEIS A293815\" ps 5 pt 5 lt 2"
          '(0 2 4 6 8 10) '(4 7 3 3 6 6) "'-' w lp title \"Stack plots,\""
		  '((1 7) (3 8) (5 3) (7 2) (9 8)) "'-' w lp title \"give 2d data as ((x y) (x y)...) or (x x ...) (y y ...),\""
		  "5+sin(x) w lp title \"and function plot commands in strings.\"")
(plt:send-command :xrange "[-1:11] noreverse" :xtics "2") ;; sends command(s) to gnuplot (does not update plot-spec)
(plt:send-string "set yrange [-1:11] noreverse nowriteback") ;; send string to gnuplot
(plt:modify-plot-spec :xrange "[-10:20]") ;; modify plot-spec and don't send
(plt:update-plot-spec :title "\"Some Sequences\"") ;; update plot-spec and replot the whole plot-spec (overwrites previous commands if the aren't in the plot-spec)

(plt:plot-add '(0 1 2 3 4 5 6 7 8 9 10) '(9 4 6 4 0 10 11 3 3 2) "'-' w lp title \"OEIS A322088 (add plots to existing plots without resending all the previous data)\"")
(plt:send-command :xrange "unset" :xtics "unset")
```
You can create a script (the plot-spec) that is stored in plt:\*plot-spec\*. Use plt:plot or plt:plot-add to add a plot command to the plot-spec; plot overwrites previous plots, plot-add does not. 

# 3D Plots

```
(require 'cl-gnuplot)

(plt:plot-3d '(() () ()) "'-' u 1:2:3 w pm3d")
(plt:send-command :view "map")
```

# Multiplot

6) Several suggestable default plot styles




## License

GPLv3

