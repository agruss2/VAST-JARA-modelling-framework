###############################################################################################################################
##
##  (01) This script explores the spatial grid for the North Sea and defines mapping parameters
##  
###############################################################################################################################

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
library( VAST )

######## Load utility functions
source( make.filename( "utilities.R", DIR$Functions ) )

######## Explore the spatial grid for the North Sea 
Grid <- make_extrapolation_info( Region = "NS_IBTS", 
	projargs = '+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=km', quiet = FALSE )
plot( Grid )
names( Grid )
dim( Grid$Data_Extrap )
head( Grid$Data_Extrap )
range( Grid$Data_Extrap$Lon ) #### -3.999745°E 12.919985°E
range( Grid$Data_Extrap$Lat ) #### 49.30414°N 62.05755°N

######## Define mapping parameters, produce a map of the spatial grid for the North Sea, 
######## and save the map and the mapping parameters
map_data = rnaturalearth::ne_countries( scale = switch( "medium", "low" = 110, "medium" = 50, 
	"high" = 10, 50 ), country = NULL )
map_data <- sp::spTransform( map_data, CRSobj = sp::CRS( '+proj=longlat' ) )
xlim_map <- c( -4.05, 12.95 )
ylim_map <- c( 49.25, 62.1 )
land_color <- "grey48"
outermargintext = c( "          Longitude (°E)", "          Latitude (°N)" )
legend_x <- c( 0.8, 0.85 )
legend_y <- c( 0.1, 0.3 )
xl <- ( 1 - legend_x[1] ) * par( 'usr' )[1] + ( legend_x[1] ) * par( 'usr' )[2]
xr <- ( 1 - legend_x[2] ) * par( 'usr' )[1] + ( legend_x[2] ) * par( 'usr' )[2]
yb <- ( 1 - legend_y[1] ) * par( 'usr' )[3] + ( legend_y[1] ) * par( 'usr' )[4]
yt <- ( 1 - legend_y[2] ) * par( 'usr' )[3] + ( legend_y[2] ) * par( 'usr' )[4]
align <- c( "lt","rb" )[2]
gradient <- c( "x", "y" )[2]
cex_legend <- 1.25 
Mapping_parameters  = list( "xlim_map" = xlim_map, "ylim_map" = ylim_map, "map_data" = map_data, "land_color" = land_color, 
	"outermargintext" = outermargintext, "xl" = xl, "xr" = xr, "yb" = yb, "yt" = yt, 
	"align" = align, "gradient" = gradient, "cex_legend" = cex_legend )
png( file = make.filename( "North_Sea_spatial_grid.png", DIR$Maps ), width = 6, height = 7, units = "in", res = 600 ) 
	par( mfrow = c( 1, 1 ) )
	par( mar = c( 4, 4, 1, 1 ) )
	plot( Mapping_parameters[["xlim_map"]], Mapping_parameters[["ylim_map"]], 
		xlim = Mapping_parameters[["xlim_map"]], ylim = Mapping_parameters[["ylim_map"]], 
		type = "n", ylab = "", xlab = "" )
	points( Grid$Data_Extrap$Lon, Grid$Data_Extrap$Lat, pch = 16, col = "black" )
	sp::plot( Mapping_parameters[["map_data"]], col = Mapping_parameters[["land_color"]], add = TRUE )
	mtext( side = 1, outer = TRUE, Mapping_parameters[["outermargintext"]][1], 
		cex = 1.25, line = par()$oma[1]/2, padj = -2 )
	mtext( side = 2, outer = TRUE, Mapping_parameters[["outermargintext"]][2], 
		cex = 1.25, line = par()$oma[2]/2, padj = 2 )
dev.off()
save( Mapping_parameters, file = make.filename( "Mapping_parameters.RData", DIR$Input ) )

######## Do some cleaning
graphics.off()

