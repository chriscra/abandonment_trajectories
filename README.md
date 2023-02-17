# Abandonment trajectories: investigating the persistence of cropland abandonment and its implications for biodiversity and carbon sequestration

Code archive: [![DOI](https://zenodo.org/badge/278699632.svg)](https://zenodo.org/badge/latestdoi/278699632) | 
Data archive: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5348287.svg)](https://doi.org/10.5281/zenodo.5348287)


This repository houses analysis scripts and documentation for:

> Crawford C.L.\*, Yin, H., Radeloff, V.C., and Wilcove, D.S. *2022*. Rural land abandonment is too ephemeral to provide major benefits for biodiversity and climate. *Science Advances* [doi.org/10.1126/sciadv.abm8999](https://doi.org/10.1126/sciadv.abm8999)

\*@chriscra, ccrawford@princeton.edu, Robertson Hall, Princeton University, Princeton, NJ

In this project, we tracked cropland abandonment and recultivation at 11 sites using annual land cover maps produced by [Yin et al. 2020](https://doi.org/10.1016/j.rse.2020.111873) at a 30-meter spatial resolution, based on publicly available satellite imagery from the Landsat archive (1987-2017). 

The annual land cover maps (1987-2017, 30-meter resolution) that underlie our analysis were developed on Google Earth Engine using publicly available Landsat satellite imagery ([Yin et al. 2020, *Remote Sensing of Environment*](https://doi.org/10.1016/j.rse.2020.111873)).
These data are archived and publicly available at Zenodo ([DOI: 10.5281/zenodo.5348287](http://doi.org/10.5281/zenodo.5348287)), along with other datasets produced during the analysis.
Primary derived data products include maps of abandonment age and datasets describing the area abandoned (in hectares) at each site in a given year, separated by year of initial abandonment ("cohort"). 

This is a living repository. 
An archive of this repository is available at Zenodo: ([DOI 10.5281/zenodo.6383127](https://zenodo.org/badge/latestdoi/278699632)).
Please refer to this archive and the public release for the code used at time of publication.

## Overview

This project involved three broad groups of analyses:  

1. Processing annual land cover maps in order to identify and map abandoned croplands, and their duration of abandonment ("age"), in each year of the time series. Time series data were analyzed in the form of rasters (via the [`raster`](https://rspatial.github.io/raster/reference/raster-package.html) and [`terra`](https://rspatial.github.io/terra/reference/terra-package.html) packages) and data.tables (via [`data.table`](http://r-datatable.com)).
2. Modeling the recultivation of abandoned croplands as a function of time since initial abandonment, using linear models. After identifying abandoned croplands and calculating how long they had been abandoned for in each year of our time series, we grouped abandonment at each site according to the year of initial abandonment. We then modeled the proportion of each group of abandoned croplands (i.e., "cohort," grouped by year of initial abandonment) that remained abandoned as a function of time (including a linear and log term of time). From these linear models, we calculated the mean recultivation ("decay") trajectory for each site across a range of common endpoints (in order to ensure that mean coefficient estimates were derived from a consistent number of observations for each cohort), and used these to calculate the "half-life" of abandoned croplands, defined as the time required for half of the croplands abandoned in a given year would be recultivated. We also used a linear model to see whether recultivation (in the form of the half-lives of each cohort) was changing over time.
3. Estimating the carbon accumulation in abandoned croplands, as a function time since abandonment and biome, using two recently published spatially-explict datasets of forest carbon accumulation ([Cook-Patton et al. 2020, Nature](https://doi.org/10.1038/s41586-020-2686-x)) and soil organic carbon accumulation ([Soils Revealed, Sanderman et al. 2020](https://doi.org/10.7910/DVN/HA17D3)).

In addition to observed abandonment, we also developed a simple scenario assuming that no recultivation took place during our time series, and that all abandoned croplands remained abandoned from their initial abandonment through the end of the time series. 
This allowed us to directly quantify the effects of recultivation by comparing the observed abandonment area, age distribution, and carbon accumulation to their corresponding potential values.

## Components of this repository

This repository has one primary directory housing the scripts used in our analysis.
The "scripts" directory contains a handful of working scripts along with two other directories: "cluster" and "_util." 

The working R scripts and Rmarkdown files are numbered based on the general order in which parts of the analysis were conducted. 
Note that the `.Rmd` scripts are intended to be run interactively chunk by chunk (sometimes line by line), not knit together all at once. 
They also include code for data visualization and exploratory data analysis; therefore, not all code is strictly required. 
Some chunks produce and save data files, which other chunks sometimes reload and use for further analysis or visualization.

These files include:

- **0_Start.R** serves as the starting place for all scripts, loading required packages and custom functions.
- **1_summary_stats.Rmd** manipulates and summarizes the data analysis products, producing summary datasets and figures for the manuscript.
- **2_decay_models.Rmd** contains the code for developing linear models of the recultivation ("decay") of abandoned cropland as a function of time following initial abandonment.
- **3_carbon.Rmd** outlines our pixel-based quantification of carbon accumulation in abandoned croplands, based on [Cook-Patton et al. 2020](https://doi.org/10.1038/s41586-020-2686-x) (forest biomass) and [Sanderman et al. 2020](https://doi.org/10.7910/DVN/HA17D3) (SOC). This is accompanied by "7_carbon.R" (see below).
- **4_figures.Rmd** contains code for exploratory data analysis and visualization, including the production of more figures for the manuscript and supporting information.
- **Crawford_abandonment_MS+SI.Rmd** contains the text of the main text of the manuscript as well as the supporting information. Data is loaded and manipulated directly and programmatically in this document, and the document requires access to data files and figures as a result to knit correctly.
- **preamble.tex** contains common LaTeX code for both the manuscript and the supporting information file.


### cluster scripts

The **cluster** folder contains scripts performing much of the heavy-lifting of the analysis of the land cover time series, which was conducted on Princeton's High Performance Computing cluster system.
These scripts produce the input files for the above ".Rmd" files.
Each R script is accompanied by a "slurm" script, which sets the parameters for each scripts cluster usage through the Slurm scheduling system.
Two slurm scripts, **\_1_4.slurm** & **\_5_7.slurm**, run the full analysis in two sections: first, scripts numbered 1 through 4 below, which are run in parallel on 11 cores (one for each site), and second, scripts numbered 5 through 7, which can be run on a single core in sequence.
Scripts are as follows:

- **0_cluster_packages.R** loads R packages required for cluster analysis.
- **1_prep_r_to_dt.R** merges, recodes, and preps land cover rasters for eleven sites from [Yin et al. 2020](https://doi.org/10.1016/j.rse.2020.111873), saving them as data.tables for further analysis.
- **2_analyze_abn.R** is the most important analysis script for this work, taking the prepped land cover time series from "1_prep_r_to_dt.R" and applying a series of custom functions to it, filtering the time series to only those pixels that represent abandoned croplands, calculating their age in each year, calculating the maximum age throughout the time series for each pixel, saving a map of the age of abandoned cropland as both rasters and data.tables, and finally, extracting summary statistics from these abandonment data products.
  + **2.1_summarize_abn_dts_only.R** performs *only* the final step of the previous script ("2_analyze_abn.R"), extracting a range of summary statistics for each site based on the derived maps of abandonment. This is not strictly necessary if running the full workflow. 
- **3_calc_recult_age.R** calculates how long periods of *recultivation* last, following qualifying periods of abandonment. This script produces similar duration stats as those produced for observed abandonment in "2_analyze_abn.R", and potential abandonment in "4_potential_age_without_recultivation.R".
- **4_potential_age_without_recultivation.R** calculates the duration of *potential* abandonment, for a scenario in which no recultivation took place and abandoned croplands were left abandoned through the end of the time series. This script produces similar duration stats as those produced for observed abandonment in "2_analyze_abn.R" and "3_calc_recult_age.R" above in 
- **5_distill_lengths.R** takes raw data.tables of the length of each abandonment period (for each pixel) observed during the time series at each site, and distills these tens of millions of pixels down to the frequency of each length (i.e., abandonment duration) at each site. 
- **6_lc_of_abn.R** calculates the proportion of abandoned cropland (as of 2017) that is classified as woody vegetation vs. grassy (herbaceous) vegetation. This results in summary files "abn_lc_count_2017" & "abn_prop_lc_2017," along with a figure "abn_prop_lc_2017."
- **7_carbon.R** calculates carbon accumulation in abandoned croplands based on the duration of abandonment and biome. This script is based on a custom function ("cc_calc_carbon_accumulation()"), which calculates total carbon accumulation for each year of the time series, using SpatRasters and data.tables.


### util scripts

The "util" folder contains scripts that include custom analysis functions and pathnames for managing the project:

- **\_util_main.R** runs the four "_util" scripts described below.
- **\_util_functions.R** contains custom functions written by Christopher Crawford, which underlie the bulk of this land cover time series analysis. These functions accomplish a variety of tasks, including: cleaning, filtering, and organizing data; calculating abandonment age; extracting area, persistence, and turnover statistics from abandonment maps; calculating the duration of potential abandonment and recultivation periods; calculating carbon accumulation; custom `data.table` helper functions; and saving plots.
- **\_util_files.R** loads maps and other input and derived data files.
- **\_util_misc.R** includes miscellaneous functions for development, analysis, and plotting.
- **\_util_pathnames.R** includes user-specific pathnames for managing the project, which are used throughout the rest of the analysis scripts. Note, however, that **cluster** scripts make use of alternative pathnames (specific to the Princeton high-performance computing clusters used for the analysis), as specified in each script.
