# Spatial variability of election turnout in the Netherlands
Term project for the MSc Applied Data Science course Spatial Statistics &amp; Machine Learning at Utrecht University.
Performed by Max van den Elsen (2590611), Sander Engelberts (1422138) and Thom Venema (1157485) in March-April 2022

This term project created among which a geographically weighted regression model to check for the spatial variability of election turnout among neighbourhoods in the Netherlands. For this social-economic, socio-demographics, and location variables are used to check if these can explain differences in election turnout. Using these findings, policies can be made more focussed to attract more people to the polling stations.

## Repository
Below the paths to code-, data, full results, and other files in the repository can be found together with a short description of what these are about.  

| Path | Description
| :--- | :----------
| [spatialstatistics](https://github.com/thomvenema/spatialstatistics) | Main repository
| &ensp;&ensp;&boxvr;&nbsp; [LICENSE](https://github.com/thomvenema/spatialstatistics/blob/main/LICENSE) | GPL-3.0 License with copyright to autors
| &ensp;&ensp;&boxvr;&nbsp; [README.md](https://github.com/thomvenema/spatialstatistics/blob/main/README.md) | This ReadMe giving information about the repository
| &ensp;&ensp;&boxvr;&nbsp; [SSML_term_project.pdf](https://github.com/thomvenema/spatialstatistics/blob/main/SSML_term_project.pdf) | The report corresponding to this research
| &ensp;&ensp;&boxvr;&nbsp; [poster.pdf](https://github.com/thomvenema/spatialstatistics/blob/main/poster.pdf) | The research poster corresponding to this study
| &ensp;&ensp;&boxvr;&nbsp; [turnout_per_station.R](https://github.com/thomvenema/spatialstatistics/blob/main/turnout_per_station.R) | The code created for combining and cleaning the data of polling stations
| &ensp;&ensp;&boxvr;&nbsp; [turnout_per_station_XY.R](https://github.com/thomvenema/spatialstatistics/blob/main/turnout_per_station.R) | The code created for merging the turnout data of polling stations with their location coordinates
| &ensp;&ensp;&boxvr;&nbsp; [Merge_turnout_CBS.R](https://github.com/thomvenema/spatialstatistics/blob/main/Merge_turnout_CBS.R) | The code created for merging the turnout and coordinate data of polling stations with CBS population statistics at neighbourhood level to create the final data
| &ensp;&ensp;&boxvr;&nbsp; [modelling_turnout.R](https://github.com/thomvenema/spatialstatistics/blob/main/modelling_turnout.R) | The code that utilizes the final data from [Merge_turnout_CBS.R](https://github.com/thomvenema/spatialstatistics/blob/main/Merge_turnout_CBS.R) to explore the spatial data distribution, and train, inspect and compare multiple model types
| &ensp;&ensp;&boxur;&nbsp; [modelling_results](https://github.com/thomvenema/spatialstatistics/blob/main/modelling_results) | All results generated by [modelling_turnout.R](https://github.com/thomvenema/spatialstatistics/blob/main/modelling_turnout.R), ordered in maps representing their model type, variable stratification, and possible other factors like the type of cross-validation or weight matrix
