###############################################################################################################################
##
##  (04e) This script runs and evaluates a VAST model for Pleuronectes platessa
##  Here, n_x = 200 knots are being used
##  
###############################################################################################################################

######## Load required R packages
library( VAST )
library( DHARMa )
library( raster )

######## Load required functions
source( file = make.filename( file = "DHARMa utilities.R", DIR$Functions ) )
source( file = make.filename( file = "plot utility functions.R", DIR$Functions ) )
source( file = make.filename( file = "utilities.R", DIR$Functions ) )
source( file = make.filename( file = "plot_population_index.R", DIR$Functions ) )

######## Define some settings
species <- "Pleuronectes platessa"

######## Set a new subfolder where outputs will be saved
if ( !dir.exists( paste0( DIR$Output, "\\", species ) ) ) { dir.create( paste0( DIR$Output, "\\", species ) ) } 
OutputDir <- paste0( DIR$Output, "\\", species )

######## Load data
load( make.filename( file = paste0( "Data_", species, ".RData" ), DIR$Input ) )
years <- sort( unique( Species_data$year ) )
Year_i <- as.numeric( as.character( Species_data$year ) )
Lon_i <- Species_data$long
Lat_i <- Species_data$lat

######## Define VAST settings 
settings <- make_settings( n_x = 200, Region = "NS_IBTS", purpose = "index2", bias.correct = TRUE )
settings$Options <- c( Calculate_Range = TRUE, Calculate_effective_area = TRUE, 
	treat_nonencounter_as_zero = TRUE, Range_fraction = 0.2 )
settings$Method <- "Barrier"

######## Estimate fixed effects and predict random effects
setwd( OutputDir )
fit = fit_model( settings = settings, Lat_i = Lat_i, Lon_i = Lon_i, 
	t_i = as.numeric( as.character( Species_data[, "year"] ) ), c_i = rep( 0, nrow( Species_data ) ), 
	b_i = as.numeric( as.character( Species_data[, "wgt_cpue"] ) ), a_i = rep( 1, nrow( Species_data ) ), 
	v_i = rep( 0, nrow( Species_data ) ), Q_ik = NULL, newtonsteps = 3 )
Save = list( "Opt" = fit, "Report" = fit$Report, "ParHat" = fit$ParHat, "DataFrame" = fit$data_frame )
save( Save, file = "Save.RData" )

######## Define some settings                                                  
Year_Set = seq( min( Species_data[,'year'] ), max( Species_data[,'year'] ) )
Years2Include = which( Year_Set %in% sort( unique( Species_data[,'year'] ) ) )

######## Extract estimated densities and save them in a .RData file
Density = as.data.frame( fit$Report$D_gct[,1,,drop=TRUE] )
colnames( Density ) <- as.character( Year_Set[Years2Include] )
save( Density, file = "Spatial_density_estimates.RData" ) 

######## Produce an abundance index and save it in a .RData file
Index = plot_biomass_index( fit = fit, years_to_plot = Years2Include, year_labels = Year_Set,
	working_dir = OutputDir )
save( Index, file = "Index.RData" ) 

######## Evaluate the model using DHARMa residuals
n_samples <- 1000
Obj = fit$tmb_list$Obj
n_g_orig = Obj$env$data$n_g
Obj$env$data$n_g = 0
b_iz = matrix( NA, nrow = length( fit$data_frame$b_i ), ncol = n_samples )
for ( zI in 1 : n_samples ) {
	if ( zI %% max( 1, floor( n_samples / 10 ) ) == 0 ) {
		message( "  Finished sample ", zI, " of ", n_samples )
	}
	b_iz[,zI] = simulate_data( fit = list( tmb_list = list( Obj = Obj ) ), type = 1 )$b_i
}
if ( any( is.na( b_iz ) ) ) {
	stop( "Check simulated residuals for NA values" )
}
b_iz <- as_units( b_iz, 'kg' )
dharmaRes = create_DHARMa( simulatedResponse = b_iz, observedResponse = as_units( fit$data_frame$b_i, 'kg' ),
	fittedPredictedResponse = fit$Report, integer = FALSE )
prop_lessthan_i = apply( as.numeric( b_iz ) < outer( as.numeric( fit$data_frame$b_i ), 
	rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
prop_lessthanorequalto_i = apply( as.numeric( b_iz ) <= outer( as.numeric( fit$data_frame$b_i ), 
	rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
PIT_i = runif( min = prop_lessthan_i, max = prop_lessthanorequalto_i, n = length( prop_lessthan_i ) )
dharmaRes$scaledResiduals = PIT_i

#### Save the DHARMa residuals 
save( dharmaRes, file = "dharmaRes.RData" )

#### Produce and save an histogram of DHARMa residuals
val = dharmaRes$scaledResiduals
val[val == 0] = -0.01
val[val == 1] = 1.01
jpeg( "Histogram_of_DHARMa_residuals.png", width = 6, height = 7, units = "in", res = 600 )
	hist( val, breaks = seq( -0.02, 1.02, len = 53 ), col = c( "red", rep( "lightgrey", 50 ), "red" ),
		main = "", xlab = "Residuals", cex.main = 2.5 )
dev.off()

#### Produce and save a QQ-plot of DHARMa residuals
jpeg( "QQplot_of_DHARMa_residuals.png", width = 6, height = 7, units = "in", res = 600 )
	gap::qqunif( dharmaRes$scaledResiduals, pch = 2, bty = "n", logscale = F, col = "black", cex = 0.6,
		main = "", cex.main = 2.5 )
dev.off()

#### Map DHARMa residuals in each year of the study period 
load( file = make.filename( "Mapping_parameters.RData", DIR$Input ) )
Grid <- make_extrapolation_info( Region = "NS_IBTS", 
	projargs = '+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=km', quiet = FALSE )
years <- sort( unique( Species_data$year ) )
Q1 <- matrix( NA, nrow = length( dharmaRes$scaledResiduals ), ncol = length( years ) )
for ( i in 1 : length( years ) ) { Q1[,i] <- ifelse( Year_i == years[i], dharmaRes$scaledResiduals, NA ) }
loc_data <- data.frame( "long" = Species_data$long, "lat" = Species_data$lat )
NN <- RANN::nn2( data = loc_data, query = Grid$Data_Extrap[,c( "Lon", "Lat" )], k = 1)$nn.idx[,1]
Data_Extrap <- as.data.frame( Grid$Data_Extrap )
zlim <- range( Q1, na.rm = TRUE )
col <- colorRampPalette( colors = c( "darkblue", "blue", "lightblue", "lightgreen", "yellow", "orange", "red" ) )
col <- col( 10 )
legend_x <- c( 0.8, 0.85 )
legend_y <- c( 0.1, 0.3 )
for ( i in 1 : length( years ) ) {
	Year <- years[i]
	Q1_1 <- Q1[,i]
	Q1_1_spatialgrid <- Q1_1[match( NN, 1 : length( Q1_1 ) )]
	Points_proj <- sp::SpatialPointsDataFrame( coords = cbind( Data_Extrap$Lon, Data_Extrap$Lat ),
		data = data.frame( "y" = Q1_1_spatialgrid ), 
		proj4string = sp::CRS( '+proj=longlat' ) )
	n_cells <- length( Q1_1_spatialgrid )
	cell.size = mean( diff( Points_proj@bbox[1,] ), diff( Points_proj@bbox[2,] ) ) / floor( sqrt( n_cells ) )
	Raster_proj = plotKML::vect2rast( Points_proj, cell.size = cell.size )
	png( file = make.filename( paste0( species, "_DHARMa residuals_", Year, "_with legend.png" ), OutputDir ), 
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
	png( file = make.filename( paste0( species, "_DHARMa residuals_", Year, "_without legend.png" ), OutputDir ), 
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

######## Create a "MapDetails_List" object and save it in a .RData file
setwd( DIR$Output )
Grid <- make_extrapolation_info( Region = "NS_IBTS", 
	projargs = '+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=km', quiet = FALSE, max_cells = 2000 )
Spatial_List <- make_spatial_info( grid_size_km = settings$grid_size_km, n_x = settings$n_x, 
	fine_scale = settings$fine_scale, Method = settings$Method, 
	Lon = as.numeric( as.character( Species_data[,"longitude"] ) ),
	Lat = as.numeric( as.character( Species_data[, "latitude"] ) ), 
	Extrapolation_List = Grid, Save_Results = TRUE, "knot_method" = "grid" )
MapDetails_List <- make_map_info( "Region" = settings$Region, "NN_Extrap" = Spatial_List$PolygonList$NN_Extrap, 
	"spatial_list" = Spatial_List, "Extrapolation_List" = Grid )
save( MapDetails_List, file = "MapDetails_List.RData" )

