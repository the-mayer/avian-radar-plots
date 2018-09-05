# avian-radar-plots
Development of metrics and plots (ggplot2) to display summarized avian radar data to wildlife managers at SEA and SLC. 

#ARTI_Analysis_v3.0: This function was designed to summarize large stretches of radar data into succinct time periods, making the information less dense and interpretable by wildlife managers. It incorporates all of the best parts of other .R files, requiring less user interaction and having some diagnostic and "error handling" incorporated so that if the function fails, you have some feedback as to why. Often, the source radar data would be corrupted, or headers modified, depending on location. 

Care was taken to make this function location agnostic, calculating locations and local time zones based on the source data -- requiring less user intervention when generating plots. 
