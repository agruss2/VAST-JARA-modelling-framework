###############################################################################################################################
##
##  (06e) This script runs FishLife and JARA for Pleuronectes platessa
##  
###############################################################################################################################

######## Load necessary packages
library( FishLife ) 
library( JARA ) 

######## Define some settings 
species <- "Pleuronectes platessa"

######## Define subfolders for the species of interest
OutputDir <- paste0( DIR$Output, "\\", species )

######## Extract a generation time value from FishLife
sp <- unlist( strsplit( as.character( species )," " ) )
taxa <- Search_species( Genus = sp[1], Species = sp[2], add_ancestors = TRUE )$match_taxonomy
predfl <- Plot_taxa( taxa, mfrow = c( 3, 2 ) )
GL <- predfl[[1]]$Mean_pred["G"] #### 15.73761 years

######## Prepare input data for JARA
jd <- read.csv( make.filename( file = "Index.csv", OutputDir ), as.is = TRUE ) 
Year <- min( jd$Time ) : max( jd$Time )
index  <- data.frame( Year, Index = NA )
se <- data.frame( Year, Index = 0.1 )
index[Year %in% jd$Time,2] <- jd$Estimate 
se[Year %in% jd$Time,2] <- jd$Std..Error.for.ln.Estimate.

######## Create a data list with JARA input and settings to be passed to fit_jara() 
jarainput <- build_jara( I = index, se = se, GL = GL[[1]], assessment = species, 
 	pk.prior = c( 0.5, 0.1 ), fixed.obsE = 0.001, scenario = "" ) #### Limit exponentially increasing projections 

######## Run JARA
setwd( OutputDir ) 
jara <- fit_jara( jarainput, save.jara = T, save.all = F, save.csvs = F, 
	save.jarafile = F, do.ppc = T, nc = 3, ni = 100000, nt = 6, nb = 40000 )

######## Plot observed and fitted indices with expected CIs (dark grey) and then 
######## fitted CPUE indices on log-scale (r4ss-style), and then plot all routine JARA plots and save them  
jrplot_fits( jara )
jrplot_logfits( jara )
jara_plots( jara )

######## Produce a summary multi-plot figure  
adj <- -.15
plname <- paste0( "jara.", sp[1], ".", sp[2] )
pwidth <- 8
pheight <- 8.5
res <- 300
windows( width = pwidth, height = pheight ) 
	jrpar( mfrow = c( 3, 2 ), labs = T, plot.cex = 1 )
 	jrplot_indices( jarainput, add = T )
 	mtext( text = paste0( letters[1], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
 	jrplot_trjfit( jara, add = T )
 	mtext( text = paste0( letters[2], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
 	jrplot_poptrj( jara, add = T )
 	mtext( text = paste0( letters[3], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
 	jrplot_changes( jara, add = T )
 	mtext( text = paste0( letters[4], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
 	jrplot_state( jara,add = T )
 	mtext( text = paste0( letters[5], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
 	jrplot_iucn( jara, add = T )
 	mtext( text = paste0( letters[6], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
 	mtext( text = paste0( letters[6], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
 	mtext( species, side = 3, outer = T, line = .5, cex = 1., c( 0.5 ) )
dev.print( jpeg, paste0( getwd(), "/", plname, ".jpg" ), width = pwidth, height = pheight, res = res, units = "in" )
 	
######## Do some extra hindcasting
hc <- jara_hindcast( jarainput, peels = 0 : 10, save.jarafile = F )
 	
######## Plot hindcasting results
plname <- paste0( "jarahindcast", sp[1], ".", sp[2] )
pwidth <- 8
pheight <- 9
res <- 300
windows( width = pwidth, height = pheight ) 
	jrpar( mfrow = c( 2, 1 ), labs = T, plot.cex = 1, mai = c( 0.6, 0.6, 0.1, 0.5 ) )
  	jrplot_retrobias( hc, add = T )
  	mtext( text = paste0( letters[1], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
  	jrplot_retroiucn( hc, add = T )
  	mtext( text = paste0( letters[2], ")" ), xpd = NA, side = 3, adj = adj, cex = 1 )
  	mtext( paste0( "Retrospective Hindcast ", species ), side = 3, outer = T, line = .5, cex = 1., c( 0.5 ) )
dev.print( jpeg, paste0( getwd(), "/", plname, ".jpg" ), width = pwidth, height = pheight, res = res, units = "in" )
   	
######## Do some cleaning 
graphics.off()

