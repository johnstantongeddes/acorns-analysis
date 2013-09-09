Example analysis of "stats_data.csv" for survey of statistical methods
========================================================

**Author:** [John Stanton-Geddes](www.johnstantongeddes.org), <john.stantongeddes.research@gmail.com>
**Created:** 2013-05-13

Data is a subset of data file from the paper "Walter D. Koenig and Johannes M. H. Knops. 2013. Large-scale spatial synchrony and cross-synchrony in acorn production by two California oaks. Ecology. 94:83–93. http://dx.doi.org/10.1890/12-0940.1" downloaded from [Ecological Archives](http://esapubs.org/archive/ecol/E094/009/). Seven of the original 13 environmental variables were selected based on an analysis of the full data set (acorn_analysis.Rmd) to reduce complexity of model fitting for participants. Some of the columns were renamed for convenience of analysis in R.

Response is acorn_count (per 30 seconds of counting, log transformed) collected from multiple sites over multiple years with many potential predictors. 

We in no way intend this as a criticism or an endorsement of the analysis performed by Koenig and Knops. We quite liked their paper, but simply selected this data set as it was freely available from *Ecological Archives* and fit our requirements of a single response and a number of potentially-important and associated predictors.
