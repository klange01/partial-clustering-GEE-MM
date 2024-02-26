# partial-clustering-GEE-MM
R code for simulation study comparing GEEs and mixed models for the analysis of partially clustered trials. The accompanying article is currently in preparation.

## Files
**R/:** Functions for generating simulated data, analysing the datasets, and generating nested loop plots.  
  
**simdata/:** The generated data files and analyis objects will be saved to this folder.  
  
**output/:** The tables and plots for the manuscript will be saved to this folder.  
  
**scripts/:** The R scripts for running the simulation study. The files are intended to be run in the following order:  
1. runsim.R: Generates simulated data from partially clustered trials. Analyses each dataset with GEEs and mixed models.
2. process_results.R: Prepares the analysis results for further analysis and generates the simsum objects that calculate the simulation study performance measures.
3. exploratory_analyses.R: Prepares the analysis results for three exploratory analyses that were included in this project.
4. small_sample_analyses.R: A sub-study that analyses datasets from selected scenarios with small sample bias correction methods.
5. small_sample_processing.R: Prepares the small sample sub-study results for further analysis.
6. manuscript_figures.R: Generates the figures included in the manuscript.
7. manuscript_tables.R. Generates tables included in the manuscript.
