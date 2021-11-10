Project overview

Aim: we aim to develop site-specific scenarios to be used as input files for the BEEHAVE (Becher et al., 2014) bee colony simulation model. 
We want to utilize the MUST-B data collected by the European Food Safety Authority (EFSA). 

Project structure:

data folder: contains downloaded data and derived data in sub-folders

analysis folder:
00_data_downloads: where did we get the data from?

explore_coordinates: get MUST-B site locations

tmy_foraging_hours: derive potential foraging hours on a daily basis, to be used as BEEHAVE input file

R folder:
contains all functions written for this project

shiny_app:
contains a shiny app to explore data availability for all MUST-B sites
