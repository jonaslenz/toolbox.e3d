# toolbox.e3d

A open-source (GPL-3) R wrapper package to the soil erosion modelling tool EROSION-3D (available from Geognostics at http://www.geognostics.de/e3d_demo.html).
The package interacts via command line for easy and reproducable work with EROSION-3D.

## Installation

```r
# Install devtools from CRAN
install.packages("devtools")

# Install toolbox.e3d
devtools::install_github("jonaslenz/toolbox.e3d")
```

## included Functions

- determination of calibration parameters skinfactor and resistance to erosion
- creating input files for EROSION-3D
- changing modelling options
- set defined matrix potential for infiltration sub model

Further functions planned to be included:
- validation modelling
- reproducable catchment modelling
