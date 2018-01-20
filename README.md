# earthgen
an earth-like planet generator

## instructions

### setting up racket

DrRacket can be downloaded [here](https://download.racket-lang.org/)

once installed, set memory to unlimited (in Racket -> Limit Memory), then restart DrRacket

### installing packages

in File -> Install Package, install the package located in "earthgen/package/vraid/"

### running the program

open "earthgen.rkt". Use ctrl+r to run. This might take a couple of minutes, so be patient

once the green man in the bottom-right has stopped running, the program is ready

open the evaluation window (ctrl+e), and enter e.g `(planet-display (default-planet 6))`, where 6 is the planet size. Anything from 0 to 8 is feasible

use z/x or the mouse wheel to zoom in/out, and the left mouse button to rotate
