##############################################################################################################################
## 
##  (03e) This script prepares data for Pleuronectes platessa for VAST modelling
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

######## Load mapping parameters
load( file = make.filename( "Mapping_parameters.RData", DIR$Input ) )

######## Define some settings
species <- "Pleuronectes platessa"

####### Set a new subfolder where maps will be saved 
if ( !dir.exists( paste0( DIR$Maps, species ) ) ) { dir.create( paste0( DIR$Maps, species ) ) } 
MapsDir <- paste0( DIR$Maps, species )

####### Load survey data 
load( file = make.filename( "Survey_data.RData", DIR$Input ) )

######## Examine the data for the species of interest, possibly do some secondary data cleaning, 
######## and save the data in a .Rdata file
Species_data <- Survey_data[Survey_data$accepted_name == species,]
nrow( Species_data ) #### 10 525 records
Haul_IDs <- unique( Species_data$haul_id )
Species_data <- Species_data[,c( "haul_id", "longitude", "latitude", "year", "wgt_cpue" )] 
Additional_data <- Survey_data[!Survey_data$haul_id %in% Haul_IDs,]
Additional_data <- Additional_data[!duplicated( Additional_data$haul_id ), ]
Additional_data$wgt_cpue <- 0 
Species_data <- rbind( Species_data, Additional_data[,c( "haul_id", "longitude", "latitude", "year", "wgt_cpue" )])
nrow( Species_data ) #### 14 577 records
summary( Species_data$year ) #### Nothing problematic
table( Species_data$year )
#### 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 
####  288  379  437  454  456  404  427  379  424  375  374  363  340  329  363  404 
#### 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
####  358  386  430  420  417  376  390  381  373  392  398  374  397  389  393  340 
#### 2015 2016 2017 2018 2019 2020 
####  374  360  377  364  352  340 

summary( Species_data$longitude ) #### Nothing problematic
summary( Species_data$latitude ) #### Nothing problematic
summary( Species_data$wgt_cpue ) #### 43 NA's
Species_data <- Species_data[!is.na( Species_data$wgt_cpue ),] 
nrow( Species_data ) #### 14 534 records 
Species_data$encounter <- ifelse( Species_data$wgt_cpue > 0, 1, 0 )
table( Species_data$encounter, Species_data$year ) 
####     1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
####   0   87  106  159  144  141  133  110  121  151  115  104  117  113  123  129
####   1  201  273  278  309  314  271  317  257  235  260  270  246  227  205  234
####    
####     1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
####   0  114  118  111  124  104  122  109  110  103   97   99   94   95   88   60
####   1  290  240  275  306  316  295  267  280  278  276  293  304  278  309  329
####    
####     2013 2014 2015 2016 2017 2018 2019 2020
####   0   88   52   76   62   88  101   90   94
####   1  305  288  298  298  289  263  262  246

summary( Species_data$wgt_cpue )
hist( Species_data$wgt_cpue )
Non_zero_species_data <- Species_data[Species_data$wgt_cpue > 0,]
summary( Non_zero_species_data$wgt_cpue )
hist( Non_zero_species_data$wgt_cpue )
sort( unique( Non_zero_species_data$wgt_cpue ) ) #### Values above 3000 kg/km2 identified as outliers (12 records)
Species_data <- Species_data[Species_data$wgt_cpue <= 3000,]
Non_zero_species_data <- Species_data[Species_data$wgt_cpue > 0,]
summary( Non_zero_species_data$wgt_cpue )
hist( Non_zero_species_data$wgt_cpue )
nrow( Species_data ) #### 14 522 records 
table( Species_data$encounter, Species_data$year )
####     1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
####   0   87  106  159  144  141  133  110  121  151  115  104  117  113  123  129
####   1  201  273  277  309  314  271  317  257  235  260  270  246  227  205  234
####    
####     1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
####   0  114  118  111  124  104  122  109  110  103   97   99   94   95   88   60
####   1  290  240  275  306  316  294  266  279  277  275  293  304  278  309  329
####    
####     2013 2014 2015 2016 2017 2018 2019 2020
####   0   88   52   76   62   88  101   90   94
####   1  305  287  295  296  289  263  262  246

#### Produce maps showing the spatial distribution of the encounter/non-encounter data  
png( file = make.filename( paste0( "Survey data_all years_", species, ".png" ), MapsDir ), 
	width = 6, height = 7, units = "in", res = 600 ) 
		par( mfrow = c( 1, 1 ) )
		par( mar = c( 4, 4, 1, 1 ) )
		plot( Mapping_parameters[["xlim_map"]], Mapping_parameters[["ylim_map"]], 
			xlim = Mapping_parameters[["xlim_map"]], ylim = Mapping_parameters[["ylim_map"]], 
			type = "n", ylab = "", xlab = "" )
		points( Species_data$longitude[Species_data$encounter == 0], 
			Species_data$latitude[Species_data$encounter == 0], pch = 16, col = "cyan" )
		points( Species_data$longitude[Species_data$encounter == 1], 
			Species_data$latitude[Species_data$encounter == 1], pch = 16, col = "blue" )
		sp::plot( Mapping_parameters[["map_data"]], col = Mapping_parameters[["land_color"]], add = TRUE )
		legend( 5, 51, legend = c( "Encounters", "Non-encounters" ), 
			col = c( "blue", "cyan" ), lwd = 3, lty = c( 0, 0 ), 
			pch = c( 16, 16 ), cex = 1.25, bg = "white" )
		mtext( side = 1, outer = TRUE, Mapping_parameters[["outermargintext"]][1], 
			cex = 1.25, line = par()$oma[1]/2, padj = -2 )
		mtext( side = 2, outer = TRUE, Mapping_parameters[["outermargintext"]][2], 
			cex = 1.25, line = par()$oma[2]/2, padj = 2 )
dev.off()
years <- sort( unique( Species_data$year ) )
for ( i in 1 : length ( years ) ) {
	year_i <- years[i]
	png( file = make.filename( paste0( "Survey data_", year_i, "_", species, ".png" ), MapsDir ), 
		width = 6, height = 7, units = "in", res = 600 ) 
			par( mfrow = c( 1, 1 ) )
			par( mar = c( 4, 4, 1, 1 ) )
			plot( Mapping_parameters[["xlim_map"]], Mapping_parameters[["ylim_map"]], 
				xlim = Mapping_parameters[["xlim_map"]], ylim = Mapping_parameters[["ylim_map"]], 
				type = "n", ylab = "", xlab = "" )
			Sp_data <- Species_data[Species_data$year == year_i,]
			points( Sp_data$longitude[Sp_data$encounter == 0], 
				Sp_data$latitude[Sp_data$encounter == 0], pch = 16, col = "cyan" )
			points( Sp_data$longitude[Sp_data$encounter == 1], 
				Sp_data$latitude[Sp_data$encounter == 1], pch = 16, col = "blue" )
			sp::plot( Mapping_parameters[["map_data"]], col = Mapping_parameters[["land_color"]], add = TRUE )
			legend( 5, 51, legend = c( "Encounters", "Non-encounters" ), 
				col = c( "blue", "cyan" ), lwd = 3, lty = c( 0, 0 ), 
				pch = c( 16, 16 ), cex = 1.25, bg = "white" )
			mtext( side = 1, outer = TRUE, Mapping_parameters[["outermargintext"]][1], 
				cex = 1.25, line = par()$oma[1]/2, padj = -2 )
			mtext( side = 2, outer = TRUE, Mapping_parameters[["outermargintext"]][2], 
				cex = 1.25, line = par()$oma[2]/2, padj = 2 )
	dev.off()
}

#### Save the data in a .RData file
save( Species_data, file = make.filename( paste0( "Data_", species, ".RData" ), DIR$Input ) )

######## Do some cleaning
graphics.off()

