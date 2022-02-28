# Clean Data

Author: Wen-Yu Kelly Chin

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/KellyWYChin/ketosis-fertility/HEAD)
[![DOI](https://sandbox.zenodo.org/badge/463110794.svg)](https://sandbox.zenodo.org/badge/latestdoi/463110794)

## Available data

Data were sponsored by DeLaval, a producer of dairy and farming machinery, with a head office in Tumba, Sweden. A total of 38 dairy herds in the Netherlands with Herd Navigator (HN) were included in the dataset. Reproduction data, milk data, and ketosis data at cow level were collected between June 2018 to July 2020.

## R version

Data editing was performed with RStudop (Version 4.0.2, URL: https://cran.rstudio.com)

## Packages installation

library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reader)
library(lubridate)

## Project organization
- PG = project-generated
- HW = human-writable
- RO = read only
```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```

## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)
