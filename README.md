# Abandonment trajectories: the environmental implications of short-lived cropland abandonment
## Investigating the timing, duration, and persistence of cropland abandonment

This repository houses analysis scripts and documentation for:

> Crawford C.L.\*, Yin, H., Radeloff, V.C., and Wilcove, D.S. 2021. Rural cropland abandonment is too ephemeral to benefit carbon sequestration or biodiversity conservation. *Submitted.*

\*@chriscra, ccrawford@princeton.edu, Robertson Hall, Princeton University, Princeton, NJ

In this project, we tracked cropland abandonment and recultivation at 11 sites using annual land cover maps produced by Yin et al. 2020 at a 30 meter spatial resolution, based on publicly available satellite imagery from the Landsat archive (1987-2017). 
After identifying abandoned croplands and calculating how long they had been abandoned for in each year of our time series, we grouped abandonment at each site according to the year of initial abandonment. 
We then modeled the proportion of each group ("cohort") of abandoned croplands that remained abandoned as a function of time (including a linear and log term of time). 
From these linear models, we calculated the mean recultivation ("decay") trajectory for each site, and used these to investigate the predicted time that would elapse before half of the croplands abandoned in a given year would be recultivated (defined as the "half-life"). 
We also used a linear model to see whether recultivation (in the form of these half-life values) was slowing down, speeding up, or staying the same over time. 
Finally, we used the mean recultivation trajectories determined from our linear models to perform a simple projection to see how the mean duration of abandonment would evolve into the future (based on a few simple assumptions).

The annual land cover maps (1987-2017, 30 meter resolution) that underlie our analysis were developed on Google Earth Engine using publicly available Landsat satellite imagery ([Yin et al. 2020, *Remote Sensing of Environment*](https://doi.org/10.1016/j.rse.2020.111873)).
These data are archived and publicly available at Zenodo (URL TBD), along with other datasets produced during the analysis.
Primary derived data products include maps of abandonment age and datasets describing the area abandoned (in hectares) at each site in a given year, separated by year of initial abandonment ("cohort"). 

This is a living repository. See the public release and Zenodo archive for the code used at time of publication. 

## Components of this repository

This repository has one primary directory housing the scripts used in our analysis. 

The four primary scripts are numbered based on the general order in which parts of the analysis were conducted. 
Note that the `.Rmd` scripts are intended to be run interactively chunk by chunk (sometimes line by line), not knit together all at once. 
They also include code for data visualization and exploratory data analysis; therefore, not all code is strictly required. 
Some chunks produce and save data files, which other chunks sometimes reload and use for further analysis or visualization.

These files include:

- 
- **0_Start.R** serves as the starting place for all scripts, loading required packages and custom functions.
- **1_summary_stats.Rmd** outlines
- **2_decay_models.Rmd**
- **3_figures.Rmd**
- **3.1_plot_lengths.R**
- **Crawford_abandonment_MS+SI.Rmd**
- **Crawford_abandonment_SI.Rmd**

There are two other directories within "scripts": "cluster" and "util." 

The "util" folder contains scripts that include custom analysis functions and pathnames for managing the project:
- **_util_functions.R** contains custom functions written by Christopher Crawford, which underlie the bulk of this analysis. These functions accomplish a variety of tasks, 
- **_util_misc.R**
- **_util_pathnames.R** includes user-specific pathnames for managing the project, which are used throughout the rest of the analysis scripts. Note, however, that **cluster** scripts make use of alternative pathnames (specific to the Princeton high-performance computing clusters used for the analysis), as specified in each script.
- **_util_master.R** runs the four "util" scripts described above.

The "cluster" folder contains scripts performing much of the heavy-lifting of the analysis of the land cover time series, which was conducted on Princeton's High Performance Computing cluster system. 
Each R script is accompanied by a "slurm" script, which sets the parameters for each scripts cluster usage through the Slurm scheduling system:
- **0_cluster_packages.R**
- **1_prep_r_to_dt.R**
- **2_analyze_abn.R**
- **2.1_summarize_abn_dts_only.R**
- **3_distill_lengths.R**
- **4_lc_of_abn.R** calculates the proportion of abandoned cropland (as of 2017) that is classified as woody vegetation vs. grassy (herbaceous) vegetation. This results in summary files "abn_lc_count_2017" & "abn_prop_lc_2017," along with a figure "abn_prop_lc_2017"
- **plot_abn_age_r_ggplot.R**


