# example_project


## Structure of the project folders

* `analysis/` is intended to contain all the source R Markdown files and the pdf reports that
summarise the analysis results for the project.
* `data/` will either be a symbolic link to an external data directory, or
a subdirectory
* `results/` will contain results exported by the analysis files
* `source/` will contain bare scripts (typically containing functions sourced
by the full analysis files)
* `Simulation/` will contain scripts to simulate the data for the differeny scenarios
* `Report/` will contain rmd and pdf file submitted on canvas containing the final report and answers

## Running order to reproduce results
* Open up the HW1.Rproj file. 
* To generate the results first run the Simulation.R file in Simulation folder. 
        **This will load all the libraries and functions first. The run the scenarios using parallelization.
         It will save .RDA files for each scenario in results folder. After all 18 scenarios are done, 
         a summary table will also be saved summarizing all the results of the 18 scenarios
* To get the final report, run the rmd file under reports folder

## Libraries used

Below are the libraries used in analysis

```
library(tidyverse)
library(tictoc)
library(doParallel)
library(foreach)
library(ggplot2)
```
