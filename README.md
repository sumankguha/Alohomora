# Alohomora

Welcome to the Alohomora repository, a toolbox for 

## Overview

A **typical** use case is: 
- generate a dataframe for raw variables
	- Hour-wise per session
	- First hour data
	- Minute-wise first hour per session

- Transform variables for
	- 

- UtilityScripts: 
	- preprocessIVSAFiles: bash script to parse MedPC log file to csv input files
	- collateIVSAData: R function to combine csv input files to single dataframe

## Installation guide
- Technical considerations
	- Hardware requirements
	- Software requirements
- R environment
## Demo

### User guide (detailed walkthrough with labeled example data)
####  Using the toolbox code — generating dataframes instructions
0. Preprocessing 
	- preprocessIVSAFiles:	This bash script parses MedPC log files to csv files for individual animals
	- collateIVSAData: 	This R function combines individual files into a single data file

1. Generate session-wise metrics
	The R notebook (makeDataFrame_IVSA_sessionWide.Rmd) generates session-wide metrics from individual data files (.csv). The metrics generated are 
	- Total infusions                                               			-
	- Total active lever
	- Total inactive lever
	- Preference score
	- Ratio = active / infusions
	- active lever interactions during infusion and time-out periods
	- Cumulative infusions
	- Cumulative active lever

2. Generate hour-wise metrics
		- 1-h binned infusions
		- 1-h binned active lever
		- 1-h binned inactive lever
		- 1-h binned ratio
		- 1-h binned active lever interactions during infusion and time-out periods
		- 1-h binned cumulative infusions
		- 1-h binned active lever 

3. Generate first hour metrics
		- First hour infusions
		- First hour active lever
		- First hour inactive lever
		- First hour ratio
		- First hour active lever interactions during infusion and time-out periods
		- First hour cumulative infusions
		- First hour cumulative active lever

4. Generate minute-wise metrics of first hour data
		- 10-min binned infusions
		- 10-min binned active lever
		- 10-min binned inactive lever
		- 10-min binned ratio
		- 10-min binned active lever interactions during infusions and time-out periods
		- 10-min binned cumulative infusions
		- 10-min binned cumulative active lever 

- Quick guide for training a tailored feature detector network
- Quick guide for evaluation of feature detectors

## User instructions for analyzing data

- How to use a trained network to analyze datasets?

## Support

## Contribute

## References

## License
