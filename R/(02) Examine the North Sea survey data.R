##############################################################################################################################
## 
##  (02) This script examines the North Sea survey data
## 
##############################################################################################################################

######## Load necessary packages
library( rnaturalearth )
library( rnaturalearthhires )
library( geosphere )
library( GISTools )
library( rgdal )
library( raster )
library( maptools )
library( rgeos )
library( inlmisc )
require( ggplot2 )
require( RColorBrewer )
require( spex )
library( FRK )
library( data.table )
library( ncdf4 )
library( sf )
library( sp )

######## Load utility functions
source( make.filename( "utilities.R", DIR$Functions ) )

#### Load mapping parameters
load( file = make.filename( "Mapping_parameters.RData", DIR$Input ) )

######## Load the survey data and explore them
Survey_data <- read.csv( make.filename( "ns-ibts_fishglob_2020.csv", DIR$Input ), dec = ".", sep = "," )
nrow( Survey_data ) #### 211 844 records
sort( names( Survey_data ) )
####  [1] "accepted_name"     "aphia_id"          "area_swept"       
####  [4] "class"             "continent"         "country"          
####  [7] "day"               "depth"             "family"           
#### [10] "gear"              "genus"             "haul_dur"         
#### [13] "haul_id"           "kingdom"           "latitude"         
#### [16] "longitude"         "month"             "num"              
#### [19] "num_cpue"          "num_h"             "order"            
#### [22] "phylum"            "quarter"           "rank"             
#### [25] "sbt"               "season"            "SpecCode"         
#### [28] "sst"               "stat_rec"          "station"          
#### [31] "stratum"           "sub_area"          "survey"           
#### [34] "verbatim_aphia_id" "verbatim_name"     "wgt"              
#### [37] "wgt_cpue"          "wgt_h"             "year"  

length( unique( Survey_data$haul_id ) ) #### 14 577 unique haul IDs
sort( table( Survey_data$accepted_name ), decreasing = T )[1 : 30] 
####         Merlangius merlangus              Limanda limanda 
####                        14130                        13241 
####              Clupea harengus                 Gadus morhua 
####                        12975                        11860 
####        Pleuronectes platessa Hippoglossoides platessoides 
####                        10525                        10256 
####           Eutrigla gurnardus     Melanogrammus aeglefinus 
####                        10238                         9056 
####            Sprattus sprattus             Microstomus kitt 
####                         8764                         8132 
####         Trisopterus esmarkii          Trisopterus minutus 
####                         7508                         5589 
####             Callionymus lyra            Amblyraja radiata 
####                         4705                         4629 
####          Trachurus trachurus                    Argentina 
####                         4223                         3344 
####           Platichthys flesus          Agonus cataphractus 
####                         3155                         2840 
####             Scomber scombrus                  Callionymus 
####                         2808                         2746 
####        Merluccius merluccius            Pollachius virens 
####                         2666                         2586 
####   Glyptocephalus cynoglossus          Lophius piscatorius 
####                         2521                         2468 
####         Enchelyopus cimbrius          Buglossidium luteum 
####                         2384                         2207 
####        Scyliorhinus canicula                  Solea solea 
####                         2130                         2025 
####       Engraulis encrasicolus       Myoxocephalus scorpius 
####                         2014                         1978 

sort( table( Survey_data$order ), decreasing = T )
Survey_data_Rajiformes <- Survey_data[Survey_data$order == "Rajiformes",]
nrow( Survey_data_Rajiformes ) #### 7762 data points
sort( table( Survey_data_Rajiformes$accepted_name ), decreasing = T )
####     Amblyraja radiata      Leucoraja naevus          Raja clavata 
####                  4629                  1192                   798 
####         Raja montagui        Raja brachyura              Dipturus 
####                   767                   133                   104 
####                  Raja   Leucoraja fullonica         Raja undulata 
####                    53                    33                    24 
####               Rajidae  Leucoraja circularis    Raja microocellata 
####                    10                     7                     7 
#### Bathyraja brachyurops Leucoraja lentiginosa        Rajella lintea 
####                     3                     1                     1 

Survey_data_Carcharhiniformes <- Survey_data[Survey_data$order == "Carcharhiniformes",]
nrow( Survey_data_Carcharhiniformes ) #### 2856 data points
sort( table( Survey_data_Carcharhiniformes$accepted_name ), decreasing = T )
####   Scyliorhinus canicula               Mustelus     Galeorhinus galeus 
####                    2130                    671                     30 
####  Scyliorhinus stellaris      Galeus melastomus 
####                      20                      5 

#### Map the spatial distribution of the survey stations in each year of the study period 
years <- sort( unique( Survey_data$year ) )
for ( i in 1 : length ( years ) ) {
	year_i <- years[i]
	png( file = make.filename( paste0( "Survey_stations_", year_i, ".png") , DIR$Maps ), 
		width = 6, height = 7, units = "in", res = 600 ) 
			par( mfrow = c( 1, 1 ) )
			par( mar = c( 4, 4, 1, 1 ) )
			plot( Mapping_parameters[["xlim_map"]], Mapping_parameters[["ylim_map"]], 
				xlim = Mapping_parameters[["xlim_map"]], ylim = Mapping_parameters[["ylim_map"]], 
				type = "n", ylab = "", xlab = "" )
			Survey_stations <- Survey_data[Survey_data$year == year_i,]
			points( Survey_stations$longitude, 
				Survey_stations$latitude, pch = 16, col = "blue" )	
			sp::plot( Mapping_parameters[["map_data"]], col = Mapping_parameters[["land_color"]], add = TRUE )
			mtext( side = 1, outer = TRUE, Mapping_parameters[["outermargintext"]][1], 
				cex = 1.25, line = par()$oma[1]/2, padj = -2 )
			mtext( side = 2, outer = TRUE, Mapping_parameters[["outermargintext"]][2], 
				cex = 1.25, line = par()$oma[2]/2, padj = 2 )
	dev.off()
}

#### Save the data in a .RData file
save( Survey_data, file = make.filename( "Survey_data.RData", DIR$Input ) )

######## Do some cleaning
graphics.off()

