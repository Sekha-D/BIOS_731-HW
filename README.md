# example_project


## Structure

* `analysis/` is intended to contain all the source R Markdown files and the pdf reports that
summarise the analysis results for the project.
* `data/` will either be a symbolic link to an external data directory, or
a subdirectory
* `results/` will contain results exported by the analysis files
* `source/` will contain bare scripts (typically containing functions sourced
by the full analysis files)
* `Simulation/` will contain scripts to simulate the data for the differeny scenarios



## Libraries used


Below are the libraries used in analysis

```
library(tidyverse)
library(tictoc)
```
