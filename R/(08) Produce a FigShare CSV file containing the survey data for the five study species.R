###############################################################################################################################
##
##  (08) This script produces a FigShare CSV file containing the survey data for the five study species
##  
###############################################################################################################################

######## Load data for Amblyraja radiata and include them in a new data frame called "Survey_data"
species <- "Amblyraja radiata"
load( make.filename( file = paste0( "Data_", species, ".RData" ), DIR$Input ) )
Survey_data <- Species_data[,c( "haul_id", "longitude", "latitude", "year", "wgt_cpue" )]

######## Load data for Squalus acanthias and include them in the "Survey_data" data frame
species <- "Squalus acanthias"
load( make.filename( file = paste0( "Data_", species, ".RData" ), DIR$Input ) )
Survey_data <- rbind( Survey_data, Species_data[,c( "haul_id", "longitude", "latitude", "year", "wgt_cpue" )] )

######## Load data for Gadus morhua and include them in the "Survey_data" data frame
species <- "Gadus morhua"
load( make.filename( file = paste0( "Data_", species, ".RData" ), DIR$Input ) )
Survey_data <- rbind( Survey_data, Species_data[,c( "haul_id", "longitude", "latitude", "year", "wgt_cpue" )] )

######## Load data for Scyliorhinus canicula and include them in the "Survey_data" data frame
species <- "Scyliorhinus canicula"
load( make.filename( file = paste0( "Data_", species, ".RData" ), DIR$Input ) )
Survey_data <- rbind( Survey_data, Species_data[,c( "haul_id", "longitude", "latitude", "year", "wgt_cpue" )] )

######## Load data for Pleuronectes platessa and include them in the "Survey_data" data frame
species <- "Pleuronectes platessa"
load( make.filename( file = paste0( "Data_", species, ".RData" ), DIR$Input ) )
Survey_data <- rbind( Survey_data, Species_data[,c( "haul_id", "longitude", "latitude", "year", "wgt_cpue" )] )

######## Save the "Survey_data" data frame in a .csv file
dim( Survey_data )
write.table( Survey_data, make.filename( file = "Survey_data_VAST_JARA_coupling_demonstration.csv", DIR$Output ),  
	sep = ",", row.names = F, append = F ) 

