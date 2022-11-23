README - Francesco Maura

Household risk preferences and portfolio allocation: a collective approach

This file gives a brief description of how to run the code to obtain the results and the
tables used in the file: FrancescoMaura_JMPaper.pdf available in this directory.

Note: this paper uses ELSA data, in particular ELSA Wave 8 data (2016)
This data are publicly available, under simple conditions. 
Please see https://www.elsa-project.ac.uk/accessing-elsa-data for more information.

The code of this paper uses both R-studio and Stata

a) 00_health_index.do: generates the health index used in the paper.
					the outcome is the "wave8_Hindex.dta" file
b) 01_datapreparation.R: combines "wave8_Hindex.dta" and ELSA dataset to generate
					 "wave_8_empirical_analysis.dta"
c) 02_empirical_estimates.dta: provides the main estimates of the paper, visible in 
						  Table 11, 12, 13, 14, 15, 16 and 17, and the Appendix Tables.
						  Generates "wave8_couple_v2.dta"
d) 03_descriptive_statistics_and_figures.R: uses "wave8_couple_v2.dta". The output are Table 1, 2, 3, 4
						  and Figure 1, 2, 3

For additional information please contact: francesco.maura@unipd.it