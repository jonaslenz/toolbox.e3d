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

If you do not have devtools installed, you can also download and install the source file (toolbox.e3d_0.0.0.90.tar.gz).

```r
install.packages("[...path to downloaded file...]/toolbox.e3d_0.0.0.90.tar.gz", repos = NULL, type = "source")
```

## included Functions

- determination of calibration parameters skinfactor and resistance to erosion
- creating input files for EROSION-3D
- changing modelling options
- set defined matrix potential for infiltration sub model

Further functions planned to be included:
- reproducable catchment modelling
