##############################################################################################################################
## 
##  (05c) This script plots VAST results for Gadus morhua
##  
##############################################################################################################################

######## Load necessary libraries
library( raster )
library( rgdal ) 
library( maptools )
library( rgeos )
library( dplyr )
library( GISTools )
library( VAST )
library( Kendall )

######## Load utility functions
source( file = make.filename( "utilities.R", DIR$Functions ) ) 
source( file = make.filename( "NewGraph.R", DIR$Functions ) ) 

######## Load mapping parameters 
load( file = make.filename( "Mapping_parameters.RData", DIR$Input ) ) 

######## Define some settings
species <- "Gadus morhua"

######## Define subfolders for the species of interest
OutputDir <- paste0( DIR$Output, "\\", species )
MapsDir <- paste0( DIR$Maps, species )
if ( !dir.exists( paste0( DIR$Figures, species ) ) ) { dir.create( paste0( DIR$Figures, species ) ) } 
FiguresDir <- paste0( DIR$Figures, species )

######## Load the spatial grid for the North Sea 
Grid <- make_extrapolation_info( Region = "NS_IBTS", 
	projargs = '+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=km', quiet = FALSE )

######## Load data
load( make.filename( file = paste0( "Data_", species, ".RData" ), DIR$Input ) )
years <- sort( unique( Species_data$year ) )

######## Load VAST density predictions and start processing them
load( make.filename( "Spatial_density_estimates.RData", OutputDir ) )
Log_Density <- log( Density )
Y_gs <- Y_gs2 <- drop_units( Log_Density )
Max_Y_gs <- ( max( Y_gs ) / 1e3 )
for ( i in 1 : length( years ) ) { 
	Y_gs[,i] <- ifelse( Y_gs[,i] < Max_Y_gs, NA, Y_gs[,i] ) 
	Y_gs2[,i] <- ifelse( Y_gs2[,i] < Max_Y_gs, 1, NA )
}
load( make.filename( "MapDetails_List.RData", DIR$Output ) )
PlotDF <- MapDetails_List[["PlotDF"]]
NN <- RANN::nn2( data = PlotDF[,c( "Lon", "Lat" )], query = Grid$Data_Extrap[,c( "Lon", "Lat" )], k = 1)$nn.idx[,1]

######## Produce log-density maps for each year of the study period 
Data_Extrap <- as.data.frame( Grid$Data_Extrap )
zlim = range( Log_Density, na.rm = TRUE )
col = viridisLite::viridis( 10 ) 
legend_x = c( 0.8, 0.85 )
legend_y = c( 0.1, 0.3 )
for ( i in 1 : length( years ) ) {
	Year <- years[i]
	Log_Densities_1 <- Log_Density[,i]
 	Log_Densities_spatialgrid <- Log_Densities_1[match( NN, PlotDF[,'x2i'] )]
	Points_proj = sp::SpatialPointsDataFrame( coords = cbind( Data_Extrap$Lon, Data_Extrap$Lat ),
		data = data.frame( "y" = Log_Densities_spatialgrid ), 
		proj4string = sp::CRS( '+proj=longlat' ) )
	n_cells = length( Log_Densities_spatialgrid )
	cell.size = mean( diff( Points_proj@bbox[1,] ), diff( Points_proj@bbox[2,] ) ) / floor( sqrt( n_cells ) )
	Raster_proj = plotKML::vect2rast( Points_proj, cell.size = cell.size )
	png( file = make.filename( paste0( species, "_Log Density_", Year, "_with legend.png" ), MapsDir ), 
		width = 6, height = 7, units = "in", res = 600 ) 	
			par( mfrow = c( 1, 1 ) )
			par( mar = c( 4, 4, 1, 1 ) )
			plot( Mapping_parameters[["xlim_map"]], Mapping_parameters[["ylim_map"]], 
				xlim = Mapping_parameters[["xlim_map"]], ylim = Mapping_parameters[["ylim_map"]], 
				type = "n", ylab = "", xlab = "" )
			image( Raster_proj, col = col, zlim = zlim, xlim = Mapping_parameters[["xlim"]], 
				ylim = Mapping_parameters[["ylim"]], add = T )
			sp::plot( Mapping_parameters[["map_data"]], col = Mapping_parameters[["land_color"]], add = TRUE )
			xl = ( 1 - legend_x[1] ) * par( 'usr' )[1] + ( legend_x[1] ) * par( 'usr' )[2]
			xr = ( 1 - legend_x[2] ) * par( 'usr' )[1] + ( legend_x[2] ) * par( 'usr' )[2]
			yb = ( 1 - legend_y[1] ) * par( 'usr' )[3] + ( legend_y[1] ) * par( 'usr' )[4]
			yt = ( 1 - legend_y[2] ) * par( 'usr' )[3] + ( legend_y[2] ) * par( 'usr' )[4]
			align = c( "lt","rb" )[2]
			gradient = c( "x", "y" )[2]
			plotrix::color.legend( xl = xl, yb = yb, xr = xr, yt = yt, 
				legend = round( seq(zlim[1], zlim[2], length = 4 ), 1 ), col = "white", 
				rect.col = col, cex = 1.25, align = align, gradient = gradient )
			mtext( side = 1, outer = TRUE, Mapping_parameters[["outermargintext"]][1], 
				cex = 1.25, line = par()$oma[1]/2, padj = -2 )
			mtext( side = 2, outer = TRUE, Mapping_parameters[["outermargintext"]][2], 
				cex = 1.25, line = par()$oma[2]/2, padj = 2 )
	dev.off()
	png( file = make.filename( paste0( species, "_Log Density_", Year, "_without legend.png" ), MapsDir ), 
		width = 6, height = 7, units = "in", res = 600 ) 	
			par( mfrow = c( 1, 1 ) )
			par( mar = c( 4, 4, 1, 1 ) )
			plot( Mapping_parameters[["xlim_map"]], Mapping_parameters[["ylim_map"]], 
				xlim = Mapping_parameters[["xlim_map"]], ylim = Mapping_parameters[["ylim_map"]], 
				type = "n", ylab = "", xlab = "" )
			image( Raster_proj, col = col, zlim = zlim, xlim = Mapping_parameters[["xlim"]], 
				ylim = Mapping_parameters[["ylim"]], add = T )
			sp::plot( Mapping_parameters[["map_data"]], col = Mapping_parameters[["land_color"]], add = TRUE )
			mtext( side = 1, outer = TRUE, Mapping_parameters[["outermargintext"]][1], 
				cex = 1.25, line = par()$oma[1]/2, padj = -2 )
			mtext( side = 2, outer = TRUE, Mapping_parameters[["outermargintext"]][2], 
				cex = 1.25, line = par()$oma[2]/2, padj = 2 )
	dev.off()
}

######## Produce log-density maps for each year of the study period - Version 2 (V2)
######## These maps are similar to the maps generated above, except that spatial patterns of log-density in
######## each year of the study period are shown only for those areas where log-density is >1% of the maximum 
######## expected log-density over the entire study period. For each year of the study period, the areas where 
######## log-density is <1% of the maximum expected log-density over the entire study period are highlighted 
######## in light gray 
Data_Extrap <- as.data.frame( Grid$Data_Extrap )
zlim = range( Y_gs, na.rm = TRUE )
zlim_2 = range( Y_gs2, na.rm = TRUE )
col = viridisLite::viridis( 10 ) 
col_2 = "grey78"
legend_x = c( 0.8, 0.85 )
legend_y = c( 0.1, 0.3 )
for ( i in 1 : length( years ) ) {
	Year <- years[i]
	Y_gs_1 <- Y_gs[,i]
 	Y_gs_spatialgrid <- Y_gs_1[match( NN, PlotDF[,'x2i'] )]
	Y_gs_2 <- Y_gs2[,i]
 	Y_gs2_spatialgrid <- Y_gs_2[match( NN, PlotDF[,'x2i'] )]
	Points_proj_1 = sp::SpatialPointsDataFrame( coords = cbind( Data_Extrap$Lon, Data_Extrap$Lat ),
		data = data.frame( "y" = Y_gs_spatialgrid ), 
		proj4string = sp::CRS( '+proj=longlat' ) )
	Points_proj_2 = sp::SpatialPointsDataFrame( coords = cbind( Data_Extrap$Lon, Data_Extrap$Lat ),
		data = data.frame( "y" = Y_gs2_spatialgrid ), 
		proj4string = sp::CRS( '+proj=longlat' ) )
	n_cells = length( Y_gs_spatialgrid )
	cell.size = mean( diff( Points_proj_1@bbox[1,] ), diff( Points_proj_1@bbox[2,] ) ) / floor( sqrt( n_cells ) )
	Raster_proj_1 = plotKML::vect2rast( Points_proj_1, cell.size = cell.size )
	Raster_proj_2 = plotKML::vect2rast( Points_proj_2, cell.size = cell.size )
	png( file = make.filename( paste0( species, "_Log Density_", Year, "_with legend_V2.png" ), MapsDir ), 
		width = 6, height = 7, units = "in", res = 600 ) 	
			par( mfrow = c( 1, 1 ) )
			par( mar = c( 4, 4, 1, 1 ) )
			plot( Mapping_parameters[["xlim_map"]], Mapping_parameters[["ylim_map"]], 
				xlim = Mapping_parameters[["xlim_map"]], ylim = Mapping_parameters[["ylim_map"]], 
				type = "n", ylab = "", xlab = "" )
			image( Raster_proj_2, col = col_2, zlim = zlim_2, xlim = Mapping_parameters[["xlim"]], 
				ylim = Mapping_parameters[["ylim"]], add = T )
			image( Raster_proj_1, col = col, zlim = zlim, xlim = Mapping_parameters[["xlim"]], 
				ylim = Mapping_parameters[["ylim"]], add = T )
			sp::plot( Mapping_parameters[["map_data"]], col = Mapping_parameters[["land_color"]], add = TRUE )
			xl = ( 1 - legend_x[1] ) * par( 'usr' )[1] + ( legend_x[1] ) * par( 'usr' )[2]
			xr = ( 1 - legend_x[2] ) * par( 'usr' )[1] + ( legend_x[2] ) * par( 'usr' )[2]
			yb = ( 1 - legend_y[1] ) * par( 'usr' )[3] + ( legend_y[1] ) * par( 'usr' )[4]
			yt = ( 1 - legend_y[2] ) * par( 'usr' )[3] + ( legend_y[2] ) * par( 'usr' )[4]
			align = c( "lt","rb" )[2]
			gradient = c( "x", "y" )[2]
			plotrix::color.legend( xl = xl, yb = yb, xr = xr, yt = yt, 
				legend = round( seq(zlim[1], zlim[2], length = 4 ), 1 ), col = "white", 
				rect.col = col, cex = 1.25, align = align, gradient = gradient )
			mtext( side = 1, outer = TRUE, Mapping_parameters[["outermargintext"]][1], 
				cex = 1.25, line = par()$oma[1]/2, padj = -2 )
			mtext( side = 2, outer = TRUE, Mapping_parameters[["outermargintext"]][2], 
				cex = 1.25, line = par()$oma[2]/2, padj = 2 )
	dev.off()
	png( file = make.filename( paste0( species, "_Log Density_", Year, "_without legend_V2.png" ), MapsDir ), 
		width = 6, height = 7, units = "in", res = 600 ) 	
			par( mfrow = c( 1, 1 ) )
			par( mar = c( 4, 4, 1, 1 ) )
			plot( Mapping_parameters[["xlim_map"]], Mapping_parameters[["ylim_map"]], 
				xlim = Mapping_parameters[["xlim_map"]], ylim = Mapping_parameters[["ylim_map"]], 
				type = "n", ylab = "", xlab = "" )
			image( Raster_proj_2, col = col_2, zlim = zlim_2, xlim = Mapping_parameters[["xlim"]], 
				ylim = Mapping_parameters[["ylim"]], add = T )
			image( Raster_proj_1, col = col, zlim = zlim, xlim = Mapping_parameters[["xlim"]], 
				ylim = Mapping_parameters[["ylim"]], add = T )
			sp::plot( Mapping_parameters[["map_data"]], col = Mapping_parameters[["land_color"]], add = TRUE )
			mtext( side = 1, outer = TRUE, Mapping_parameters[["outermargintext"]][1], 
				cex = 1.25, line = par()$oma[1]/2, padj = -2 )
			mtext( side = 2, outer = TRUE, Mapping_parameters[["outermargintext"]][2], 
				cex = 1.25, line = par()$oma[2]/2, padj = 2 )
	dev.off()
}

######## Load and process the index of relative biomass predicted by VAST
Nindex = read.csv( make.filename( file = "Index.csv", OutputDir ), as.is = TRUE )
Index = Nindex$Estimate
SE.Index = Nindex$Std..Error.for.Estimate
Relative_Biomass <- Index / mean( Index )
Relative_SE.Biomass <- ( Index %o% c( 1, 1 ) + SE.Index %o% c( -1.96, 1.96 ) ) / mean( Index )

######## Plot the index of relative biomass predicted by VAST
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
plot( years, Relative_Biomass, type = "n", xlab = "Year", ylab = "Relative biomass", cex.axis = 1, 
	xlim = c( min( years ) - 1, max( years ) + 1 ), 
	ylim = c( min( Relative_SE.Biomass ) - 0.11, max( Relative_SE.Biomass ) + 0.01 ) )
polygon( x = c( years, rev( years ) ), 
	y = c( Relative_SE.Biomass[,1], rev( Relative_SE.Biomass[,2] ) ), 
	col = "gray88", border = NA, lty = "solid" )
abline( h = 1, col = "black", lwd = 1.5, lty = 2 )
lines( years, Relative_Biomass, col = "gray48", type = "l", lwd = 4.5, 
	ylab = "", xlab = "", cex.axis = 0.7 )
points( years, Relative_Biomass, col = "gray48", lwd = 3.5, cex = 1, pch = 19, 
	ylab = "", xlab = "", cex.axis = 0.7 )
minor.tick( nx = 5 )
SavePlot( paste0( "\\", species, "\\", species, "_Index of relative biomass" ) )
graphics.off()
MannKendall( Relative_Biomass ) #### tau = -0.531, 2-sided pvalue =2.9146e-06

######## Load and process the centres of gravity (COGs) and the log-effective areas occupied 
######## predicted by VAST
load( file = make.filename( file = "Save.RData", OutputDir ) )
TmbData <- Save$Opt$data_list
Sdreport <- Save$Opt$parameter_estimates$SD
SD <- TMB::summary.sdreport( Sdreport )
SD_mean_Z_ctm <- array( NA, dim = c( unlist( TmbData[c( 'n_c', 'n_t', 'n_m' )] ), 2 ), 
	dimnames = list( NULL, NULL, NULL, c( 'Estimate', 'Std. Error' ) ) )
SD_mean_Z_ctm[] <- SD[which( rownames( SD ) == "mean_Z_ctm" ), c( 'Estimate', 'Std. Error' )]
SD_log_effective_area_ctl = array( TMB::summary.sdreport( Sdreport )[which(
	rownames( TMB::summary.sdreport( Sdreport ) ) == paste0( "log_", "effective_area_ctl" ) ),], 
	dim = c( unlist( TmbData[c( 'n_c', 'n_t', 'n_l' )] ), 2 ), 
	dimnames = list( NULL, NULL, NULL, c( 'Estimate', 'Std. Error' ) ) )

######## Plot the eastward COG predicted by VAST
Ybounds <- ( SD_mean_Z_ctm[1,,1,'Estimate'] %o% rep( 1, 2 ) + 
	SD_mean_Z_ctm[1,,1,'Std. Error'] %o% c( -1, 1 ) )
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
plot( years, SD_mean_Z_ctm[1,,1,'Estimate'], type = "n", xlab = "Year", ylab = "Eastings (km)", cex.axis = 1, 
	xlim = c( min( years ) - 1, max( years ) + 1 ), 
	ylim = range( c( Ybounds * 0.999, Ybounds * 1.002 ), na.rm = TRUE ) )
polygon( x = c( years, rev( years ) ), y = c( Ybounds[,1], rev( Ybounds[,2] ) ), 
	col = "gray88", border = NA, lty = "solid" )
lines( years, SD_mean_Z_ctm[1,,1,'Estimate'], col = "gray48", type = "l", lwd = 4.5, 
	ylab = "", xlab = "", cex.axis = 0.7 )
points( years, SD_mean_Z_ctm[1,,1,'Estimate'], col = "gray48", lwd = 3.5, cex = 1, pch = 19, 
	ylab = "", xlab = "", cex.axis = 0.7 )
minor.tick( nx = 5 )
SavePlot( paste0( "\\", species, "\\", species, "_Eastward COG" ) )
graphics.off()
MannKendall( SD_mean_Z_ctm[1,,1,'Estimate'] ) #### tau = -0.235, 2-sided pvalue =0.039228

######## Plot the northward COG predicted by VAST
Ybounds <- ( SD_mean_Z_ctm[1,,2,'Estimate'] %o% rep( 1, 2 ) + SD_mean_Z_ctm[1,,2,'Std. Error'] %o% c( -1, 1 ) )
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
plot( years, SD_mean_Z_ctm[1,,2,'Estimate'], type = "n", xlab = "Year", ylab = "Northings (km)", cex.axis = 1, 
	xlim = c( min( years ) - 1, max( years ) + 1 ), 
	ylim = range( c( Ybounds * 0.999, Ybounds * 1.002 ), na.rm = TRUE ) )
polygon( x = c( years, rev( years ) ), y = c( Ybounds[,1], rev( Ybounds[,2] ) ), 
	col = "gray88", border = NA, lty = "solid" )
lines( years, SD_mean_Z_ctm[1,,2,'Estimate'], col = "gray48", type = "l", lwd = 4.5, 
	ylab = "", xlab = "", cex.axis = 0.7 )
points( years, SD_mean_Z_ctm[1,,2,'Estimate'], col = "gray48", lwd = 3.5, cex = 1, pch = 19, 
	ylab = "", xlab = "", cex.axis = 0.7 )
minor.tick( nx = 5 )
SavePlot( paste0( "\\", species, "\\", species, "_Northward COG" ) )
graphics.off()
MannKendall( SD_mean_Z_ctm[1,,2,'Estimate'] ) #### tau = 0.576, 2-sided pvalue =3.5763e-07

######## Plot the log-effective area occupied predicted by VAST
Ybounds = ( SD_log_effective_area_ctl[1,,1,1] %o% rep( 1, 2 ) + SD_log_effective_area_ctl[1,,1,2] %o% c( -1, 1 ) )
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
plot( years, SD_log_effective_area_ctl[1,,1,1], type = "n", xlab = "Year", 
	ylab = expression( 'Effective area occupied ln(km'^2*')' ), 
	xlim = c( min( years ) - 1, max( years ) + 1 ), 
	ylim = range( c( Ybounds * 0.999, Ybounds * 1.002 ), na.rm = TRUE ) )
polygon( x = c( years, rev( years ) ), y = c( Ybounds[,1], rev( Ybounds[,2] ) ), 
	col = "gray88", border = NA, lty = "solid" )
lines( years, SD_log_effective_area_ctl[1,,1,1], col = "gray48", type = "l", lwd = 4.5, 
	ylab = "", xlab = "", cex.axis = 0.7 )
points( years, SD_log_effective_area_ctl[1,,1,1], col = "gray48", lwd = 3.5, cex = 1, pch = 19, 
	ylab = "", xlab = "", cex.axis = 0.7 )
minor.tick( nx = 5 )
SavePlot( paste0( "\\", species, "\\", species, "_Log-effective area occupied" ) )
graphics.off()
MannKendall( SD_log_effective_area_ctl[1,,1,1] ) #### tau = -0.494, 2-sided pvalue =1.3621e-05

