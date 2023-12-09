###############################################################################################################################
##
##  (07d) This script produces additional JARA plots for Scyliorhinus canicula
##  
###############################################################################################################################

######## Load necessary packages
library( FishLife ) 
library( JARA ) 

######## Load utility functions
source( file = make.filename( "utilities.R", DIR$Functions ) ) 
source( file = make.filename( "NewGraph.R", DIR$Functions ) ) 

######## Define some settings 
species <- "Scyliorhinus canicula"

######## Define subfolders for the species of interest
OutputDir <- paste0( DIR$Output, "\\", species )

######## Extract a generation time value from FishLife
sp <- unlist( strsplit( as.character( species )," " ) )
taxa <- Search_species( Genus = sp[1], Species = sp[2], add_ancestors = TRUE )$match_taxonomy
predfl <- Plot_taxa( taxa, mfrow = c( 3, 2 ) )
GL <- predfl[[1]]$Mean_pred["G"] #### 9.039135 years
GL <- 13 #### We decided to use 13 years instead - value from the latest ICUN Red List assessment

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

######## Plot the observed (VAST) and JARA population trajectories on the same scale  
years_data <- jarainput$data$yr  
se <- as.matrix( sqrt( jarainput$jagsdata$SE2 )[1 : length( years_data ),])
y <- as.matrix( jarainput$jagsdata$y[1 : length( years_data ),])
dat <- jarainput$data$I
Nt <- jara$trj[jara$trj$name == "global" & jara$trj$estimation == "fit",]
year <- Nt$yr
end.yr <- which( year == max( Nt$yr[Nt$estimation == "fit"] ) ) 
nT <- length( year )
years <- year[1 : end.yr]
n.years <- length( years )
abundance <- jara$settings$model.type
GL <- jara$settings$GL
years <- jara$yr
indices <- unique( jara$fits$name )
n.indices <- jara$settings$nI
cs <- sample( seq( 80, 90, 1 ) )
colci <- paste0( "gray", cs )
cols <- jara$settings$cols
q.adj <- jara$pars$median[-c( 1 : 2 )]
fits <- jara$fits[jara$fits$name%in%indices[1],]
Mean_obs <- Mean( fits$obs / q.adj[1] )
m1 <- 0
m2 <- 6.45 #### Needs to be adjusted for each species individually
xlim <- c( min( years - 1 ), max( years + 2 ) )
ylim <- c( m1, m2 )
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
plot( 0, 0, ylim = ylim, xlim = xlim, ylab = "Relative biomass", xlab = "Year", col = "black", 
	type = "n", lwd = 2, frame = TRUE, yaxs = "i" )
polygon( x = c( years, rev( years ) ), y = c( Nt$lci / Mean_obs, rev( Nt$uci / Mean_obs ) ), 
	col = "gray", border = "gray90" )
for ( i in 1 : n.indices ) {
	iv <- c( -0.25, 0.25 )
 	ds <- runif( 1, -0.2, 0.2 )
  	for ( t in 1 : length( years_data ) ) {
    		lines( rep( years_data[t] + ds, 2 ), 
			c( exp( y[t,1] - 1.96 * se[t,1] ) / Mean_obs, 
			exp( y[t,1] + 1.96*se[t,1] ) / Mean_obs ), col = cols[1], lwd = 1.2 )
    		lines( years_data[t] + ds + iv, 
			rep( exp( y[t,1] - 1.96 * se[t,1] ), 2 ) / Mean_obs, col = cols[1], lwd = 1.2 )  
    		lines( years_data[t] + ds + iv,
			rep( exp( y[t,1] + 1.96 * se[t,1] ), 2 ) / Mean_obs,col = cols[1], lwd = 1.2 )
    	}  
	points( years_data + ds,dat[,2] / Mean_obs, type = "p", pch = 21, col = cols[i], bg = cols[i], lty = 2) 

}
lines( years, Nt$mu / Mean_obs, type = "l", col = 1, lwd = 2 )
minor.tick( nx = 5 )
SavePlot( paste0( "\\", species, "\\", species, "_JARA and VAST population trajectories" ) )
graphics.off()

######## Plot the JARA estimated population trajectory relative to generation length (GL) 
Nt <- jara$trj[jara$trj$name == "global",]
year <- Nt$yr
end.yr <- which( year == max( Nt$yr[Nt$estimation == "fit"] ) ) 
nT <- length( year )
years <- year[1 : end.yr]
n.years <- length( years )
abundance <- jara$settings$model.type
GL <- jara$settings$GL
Mean_Nt <- Mean( Nt$mu[1 : end.yr] )  
m1 <- 0
m2 <- Max( Nt$uci / Mean_Nt )
xlim <- c( min( year - 1 ), max( year + 2 ) )
ylim <- c( m1, m2 )
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
plot( 0, 0, ylim = ylim, xlim = xlim, ylab = "Relative biomass", xlab = "Year", col = "black", 
	type = "n", lwd = 2, frame = TRUE, yaxs = "i" )
polygon( x = c( year, rev( year ) ), y = c( Nt$lci / Mean_Nt, rev( Nt$uci / Mean_Nt ) ), 
	col = gray( 0.6, 0.3 ), border = "gray90" )
polygon( x = c( years, rev( years ) ), y = c( Nt$lci[1 : end.yr] / Mean_Nt, Nt$uci[end.yr : 1] / Mean_Nt ), 
	col = gray( 0.7, 0.5 ), border = "gray90" )
lines( year[end.yr : nT], Nt$mu[end.yr : nT] / Mean_Nt, type = "l", col = 2, lwd = 2, lty = 5 )
lines( years, Nt$mu[1 : end.yr] / Mean_Nt, type = "l", col = 1, lwd = 2 )
if ( n.years - 3 * GL - 1 > 0) lines( rep( year[n.years] - 3 * GL, 2 ), c( 0, m2 * .93 ),lty = 2, col = 2 )
lines( rep(year[n.years] - 1 * GL, 2 ), c( 0, m2 * .93 ), lty = 2, col = 4, lwd = 2 )
lines( rep( year[n.years] - 2 * GL, 2 ), c( 0, m2 * .93 ), lty = 2, col = 3, lwd = 2 )
if ( n.years - 3 * GL - 1 > 0 ) text( year[n.years] - 3 * GL, m2 * .96, "3GL", lwd = 2 )
text( year[n.years] - 2 * GL, m2 * .96, "2GL" )
text( year[n.years] - GL, m2 * .96, "1GL" )
lines( rep( year[n.years], 2 ), c( 0, m2 ) * .93, lty = 2, col = 1, lwd = 2 )
text( year[n.years], m2 * .96, paste0( year[n.years] ) )
minor.tick( nx = 5 )
SavePlot( paste0( "\\", species, "\\", species, "_JARA estimated population trajectory relative to GL" ) )
graphics.off()

######## Plot mean rates of change (%) over 1, 2 and 3 GLs 
rs <- jara$posteriors[,-1]
lamdas <- ( exp( rs ) - 1 ) * 100
lymax <- rymax <- lxrange <- rxrange <- NULL #### Maximum and range for plotting
for ( i in 1 : ncol( rs ) ) {
	den <- stats::density( lamdas[,i], adjust = 2 )
    	assign( paste0( "xl", i ), den$x ) 
    	assign( paste0( "yl", i ), den$y )
    	lymax <- c( lymax, max( den$y ) )
    	lxrange <- c( lxrange, range( den$x ) )
    	den <- stats::density( rs[,i], adjust = 2 )
    	assign( paste0( "xr", i ), den$x )
    	assign( paste0( "yr", i ), den$y )
    	rymax <- c( rymax, max( den$y ) )
    	rxrange <- c( rxrange, range( den$x ) )
}
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
cnam <- c( "All years", "1 GL", "2 GL", "3 GL" )  
xlim <- quantile( as.matrix( lamdas ), c( 0.001, 0.999 ) )
jcol <- c( grey( 0.5, 0.6 ), rgb( 0, 0,1, 0.3 ), rgb( 0, 1, 0, 0.3 ), rgb( 1, 0, 0, 0.3 ) )
plot( 0, 0, type = "n", ylab =  "Density", xlab = "Annual rate of change (%)", cex.main = 0.9, 
	ylim = c( 0, 1.1 * max( lymax ) ), xlim = xlim, yaxs = "i" ) 
for ( i in 1 : ncol( rs ) ) {
	x = get( paste0( "xl", i ) )
    	y = get( paste0( "yl", i ) )
    	xp = x[x > xlim[1] & x < xlim[2]]
    	yp = y[x > xlim[1] & x < xlim[2]]
    	polygon( c( xp, rev( xp ) ), c( yp, rep( 0, length( yp ) ) ), col = jcol[i], border = NA )
    	mu.lamda <- round( median( lamdas[,i] ), 10 )
    	lines( rep( mu.lamda, 2 ), c( 0, max( y ) ), col = c( 1, 4, 3, 2 )[i], lwd = 1, lty = 1 )
}
abline( v = 0, lty = 2 )
legend( "topright", paste0( cnam[1 : ncol( rs )], ": ", 
	ifelse( round( apply( lamdas, 2, median ), 2 ) > 0, "+", "" ),
	round( apply( lamdas, 2, median ), 2 ), "%" ), 
	pch = 15, col = c( jcol ), bty = "n", cex = 1.2, y.intersp = 0.8, x.intersp = 0.8 )
minor.tick( nx = 5 )
SavePlot( paste0( "\\", species, "\\", species, "_JARA mean rates of change over 1-3 GLs" ) )
graphics.off()

######## Plot current or projected population change relative to the first year  
extinction <- 0.01
credibility <- 0.95
ylab <- "Density"
xlab <- "Relative state"
legend.cex <- 0.9
legend.pos <- "right"
pdyn <- jara$pop.posterior
yrs <- 1 : ncol( pdyn )
nyrs <- length( yrs )
yr <- as.numeric( names( pdyn ) )
ref.yr <- yr[1 : 3]
end.yr <- max( jara$yr ) 
prj.yr <- max( jara$pyr )
pop.ref <- apply( pdyn[,which( yr %in% ref.yr )], 1, mean )
states <- cbind( pdyn[,which( yr %in% end.yr )] / pop.ref,pdyn[,which( yr %in% prj.yr )] / pop.ref )
type <- ifelse( prj.yr - end.yr < 3, "current", "both" )  
lymax <- rymax <- lxrange <- lxmax <- NULL #### Maximum and range for plotting
for ( i in 1 : 2 ) {
	if ( i == 1 & type == "current" | type == "both" |i == 2 & type =="projected" ) {
		den <- stats::density( states[,i], adjust = 2 )
    		assign( paste0( "xl", i ), den$x )
    		assign( paste0( "yl", i ), den$y )
    		lymax <- c( lymax, max( den$y ) )
    		lxmax <- c( lxmax, quantile( states[,i], 0.99 ) )
    }
}
lxrange <- ifelse( lxrange < 0, 0, lxrange )
xlim <- c( 0, max( lxmax, 1.1 ) )
jcol <- c( grey( 0.4, 0.6 ), rgb( 1, 0, 0, 0.6 ) )
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
plot( 0, 0, type = "n", ylab = ylab, xlab = xlab, xaxt = "n", yaxt = "n", cex.main = 0.9, 
	ylim = c( 0, 1.22 * max( lymax ) ), xlim = xlim, xaxs = "i", yaxs = "i", frame = FALSE ) 
for ( i in 2 : 1 ) {
    	if ( i == 1 & type == "current" | type == "both" | i == 2 & type == "projected" ) {
    		x <- get( paste0( "xl", i ) )
    		y <- get( paste0( "yl", i ) )
    		xp <- x[x > xlim[1] & x < xlim[2]]
    		yp <- y[x > xlim[1] & x < xlim[2]]
    		polygon( c( xp, rev( xp ) ), c( yp, rep( 0, length( yp ) ) ), 
			col = jcol[i], border = NA )
    		mu <- round( median( states[,i] ), 10 )
    		lines( rep( mu, 2 ), c( 0, max( lymax * c( 1.05, 1.0 )[i] ) ), 
			col = c( 1,2 )[i], lwd = 1, lty = c( 1 ) )
    		text( max( mu, 0.05 ), max( lymax * c( 1.11, 1.05 )[i] ), c( end.yr, prj.yr )[i], cex = 0.9 )
  	}
}
axis( 1, at = seq( 0, ceiling( max( states ) ), 2 ), cex.axis = 0.9 )
axis( 2, cex.axis = 0.9 )
lines( rep( 1, 2 ), c( 0, max( lymax * c( 1.13, 1.0 )[1] ) ), col = 1, lwd = 1, lty = 2 )
text( 3.5, max( lymax * c( 1.18 ) ), min( ( ref.yr ) ), cex = 0.9 )
cnam <- c( paste0( "Current: ", round( median( states[,1] ), 2 ) ), 
	paste0( "Projected: ", round( median( states[,2] ), 2 ) ) )
if ( type == "current" ) type.id = 1 
if ( type == "projected" ) type.id = 2 
if ( type == "both" ) type.id = 1 : 2 
legend( legend.pos, cnam[type.id], pch = 15, col = c( jcol ), box.col = "white",
	cex = legend.cex, y.intersp = 0.8, x.intersp = 0.8 )
mu <- apply( states, 2, quantile, c( 0.5 ) )
quants <- rbind( mu, HDInterval::hdi( states, credMass = credibility ) )
box()
SavePlot( paste0( "\\", species, "\\", species, "_Current or projected population change relative to the first year" ) )
graphics.off()

######## Produce an IUCN posterior plot under ICUN Criterion A2  
ylimadj <- 1.1
legend.cex <- 0.9
criteria <- c( "A2", "A1" )[1]
ylab <- "Density"
xlab <- "Change (%)"
change <- jara$posteriors$pop.change
den <- stats::density( change, adjust = 2 )
x1 <- den$x
y1 <- den$y
A1 <- ifelse( criteria == "A1", TRUE, FALSE )
mu.change <- round( median( change ), 1 )
sign = ""
if ( mu.change > 0 ) sign = "+"
CR <- sum( ifelse( change <= ifelse( A1, -90, -80 ), 1, 0 ) ) / length( change ) * 100
EN <- sum( ifelse( change> ifelse( A1, -90, -80 ) & change<= ifelse( A1, -70, -50 ), 1, 0 ) ) / length( change ) * 100
VU <- sum( ifelse( change> ifelse( A1, -70, -50 ) & change<= ifelse( A1, -50, -30 ), 1, 0 ) ) / length( change ) * 100
NT <- sum( ifelse( change> ifelse( A1, -50, -30 ) & change<= ifelse( A1, -40, -20 ), 1, 0 ) ) / length( change ) * 100
LC <- sum( ifelse( change > ifelse( A1, -40, -20 ), 1, 0 ) ) / length( change ) * 100
old_status <- c( CR, EN, VU, NT, LC )
Decline <- round( sum( ifelse( change < 0, 1, 0 ) ) / length( change ) * 100, 1 )
if ( sum( round( old_status, 0 ) ) != 100 ) {
	CR <- round_realsum( old_status, 0 )[1]
    	EN <- round_realsum( old_status, 0 )[2]
    	VU <- round_realsum( old_status, 0 )[3]
	NT <- round_realsum( old_status, 0 )[4]
	LC <- round_realsum( old_status, 0 )[5]
} else{
    	CR <- round( CR, 0 )
    	EN <- round( EN, 0 )
    	VU <- round( VU, 0 )
    	if ( NT_opt == TRUE ) { NT = round( NT, 0 ) }
    	LC <- round( LC, 0 )
}
xlim <- c( -100, min( max( 30, quantile( change, .99 ) ), 1000 ) )
ylim <- c( 0, max( y1 * ylimadj ) )
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 1 ) )
plot( x1, y1, type = "n", xlim = xlim, ylim = ylim, ylab = ylab, xlab = xlab,
	cex.main = 0.9, frame = TRUE, xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i" )
maxy <- max( ylim )
x2 <- c( ifelse( A1, -40, -20 ), 1500 ); y2 = c( 0, 5 )
polygon( c( x2, rev( x2 ) ), c( rep( maxy, 2 ), rev( rep( 0, 2 ) ) ), col = "#60C659", border = "#60C659" ) 
x3 <- c( ifelse( A1, -50, -30 ), x2[1] )
polygon( c( x3, rev( x3 ) ), c( rep( maxy, 2 ), rev( rep( 0, 2 ) ) ), col = "#CCE226", border="#CCE226" ) 
x4 <- c( ifelse( A1, -70, -50 ), x3[1] )
polygon( c( x4, rev( x4 ) ), c( rep( maxy, 2 ), rev( rep( 0, 2 ) ) ), col = "#F9E814", border = "#F9E814" )
x5 <- c( ifelse( A1, -90, -80 ), x4[1] )
polygon( c( x5, rev( x5 ) ), c( rep( maxy, 2 ), rep( 0, 2 ) ), col = "#FC7F3F", border = "#FC7F3F" )
x6 <- c( -100, x5[1] )
polygon( c( x6, rev( x6 ) ), c( rep( maxy, 2 ), rep( 0, 2 ) ), col = "#D81E05", border = "#D81E05" )
polygon( c( x1, rev( x1 ) ), c( y1, rep( 0, length( y1 ) ) ), col = "grey" )
box()    
axis( 1, at = seq( -100, max( x1, 30 ) + 50, ifelse( max( x1, 30 ) > 150, 50, 25 ) ),
	tick = seq( -100, max( x1, 30 ), ifelse( max( x1, 30 ) > 150, 50, 25 ) ) )
legend( "right", c( paste0( "CR (",CR,"%)" ), paste0( "EN (",EN,"%)" ),
	paste0( "VU (",VU,"%)" ), paste0( "NT (",NT,"%)" ), paste0( "LC (",LC,"%)" ) ),
	col = 1, pt.bg = c( "#D81E05", "#FC7F3F", "#F9E814", "#CCE226", "#60C659" ), pt.cex = 1.3, pch = 22,
	bg = "white",cex = legend.cex + .3, y.intersp = 0.8, x.intersp = 0.8 )
abline( v = mu.change, lty = 2, col = 'grey30', lwd = 2 ) 
text( ifelse( mean( change ) < -80, -80, mean( change ) ), max( y1 * 1.05 ),
	paste0( "  Change: ", sign, mu.change, "%" ), bg = "white", cex = legend.cex + 0.1 )
SavePlot( paste0( "\\", species, "\\", species, "_IUCN posterior plot under ICUN Criterion A2" ) )
graphics.off()      

######## Do some extra hindcasting
hc <- jara_hindcast( jarainput, peels = 0 : 10, save.jarafile = F )
 	
######## Plot retrospective pattern
xlab <- "Year"
ylab <- "Relative biomass"
legend.loc <- "topleft"
cols <- hc$settings$cols
Nt <- hc$trj[hc$trj$name == "global",]
Nt0 <- Nt[Nt$level == 0,]
year <- hc$y
end.yr <- which(year == max( Nt$yr[Nt$estimation == "fit"] ) ) 
nyrs <- length( year )
suby <- 1 : nyrs
peels <- hc$peels
Mean_pred <- Mean( Nt$mu[suby] )
Xlim <- c( min( year ) - 0.99, max( year ) + 0.99 ) 
Ylim <- c( 0, Max( Nt$mu[suby] / Mean_pred ) * 1.4 )
m1 <- 0
m2 <- max( c( Nt0$uci / Mean_pred, Nt$mu / Mean_pred ), na.rm = TRUE ) * 1.1
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 3 ) )
plot( 0, 0, ylim = Ylim, xlim = Xlim, ylab = ylab, xlab = xlab, 
	col = "black", type = "n", lwd = 2, frame = TRUE,  yaxs = "i" )
polygon( x = c( year, rev( year ) ), y = c( Nt0$lci[suby] / Mean_pred, rev( Nt0$uci[suby] / Mean_pred ) ), 	
	col = gray( 0.4, 0.5 ), border = gray( 0.4, 0.5 ) )
lines( year, Nt$mu[suby] / Mean_pred, type = "l", col = 1, lwd = 2 )
diags <- data.frame( rho = NULL, hcrho = NULL )
for ( i in 2 : length( hc$peels ) ) {
  	Nti <- Nt[Nt$level == peels[i],]
  	sub0 <- 1 : ( end.yr - peels[i] )
  	sub1 <- 1 : ( end.yr - peels[i] + 1 )
  	sub2 <- ( end.yr - peels[i] + 1 )
  	lines( year[sub0], Nti$mu[sub0] / Mean_pred, type = "l", col = cols[i - 1], lwd = 2 )
  	lines( year[sub1], Nti$mu[sub1] / Mean_pred, type = "l", col = cols[i - 1], lwd = 2, lty = 2 )
  	points( year[sub2], Nti$mu[sub2] / Mean_pred, bg = cols[i - 1], pch = 21, cex = 0.8 )
  	rho <- ( Nti$mu[sub2 - 1] - Nt0$mu[sub2 - 1] ) / Nt0$mu[sub2 - 1]
  	hcrho <- ( Nti$mu[sub2] - Nt0$mu[sub2] ) / Nt0$mu[sub2]
  	diags <- rbind( diags, data.frame( rho = rho, hcrho = hcrho ) )
}
lines( year, Nt$mu[suby] / Mean_pred, type = "l", col = 1, lwd = 1 )
legend( legend.loc, paste( year[nyrs - peels] ), col = c( 1, cols ),
	bty = "n", cex = 0.7, pt.cex = 0.7, lwd = c( 2, rep( 2, length( peels ) ) ) )
diags <- rbind( diags, data.frame( rho = mean( diags$rho ), hcrho = mean( diags$hcrho ) ) )
mrho <- round( diags[nrow( diags ),1], 2 )
mhcrho <- round( diags[nrow( diags ),2], 2 )
row.names( diags ) <- c( year[nyrs - peels[-1]], "mu" )   
#legend( "top", legend = paste0( "Bias: ", mrho, " (", mhcrho, ")" ), bty = "n", y.intersp = -0.2, cex = 1.1 )
minor.tick( nx = 5 )
SavePlot( paste0( "\\", species, "\\", species, "_Retrospective pattern" ) )
graphics.off()

######## Produce a retrospective IUCN posterior plot under IUCN Criterion A2  
criteria <- c( "A2", "A1" )[1]
ylab <- "Change (%)"
xlab <- "Year"
A1 <- ifelse( criteria == "A1", TRUE, FALSE )
runs <- hc$peels
d <- hc$posteriors
ymax1 <- quantile( hc$posteriors$pop.change[hc$posteriors$level == 0], 0.995 )
ymax2 <- quantile( hc$posteriors$pop.change[hc$posteriors$level > 0], 0.985 )
ymax <- max( ymax1, ymax2 )
ylim <- c( -100, min( max( 30, ymax ), 1000 ) )
xlim <- c( 0.5, length( hc$peels ) + 0.49 )
xall <- hc$posteriors$pop.change
cols <- rev( c( "#D81E05", "#FC7F3F", "#F9E814", "#CCE226", "#60C659" ) )
new.graph( 0.5 )
par( cex = 1.5 )
par( mar = c( 4.5, 4.5, 1, 3 ) )
plot( 0, 0, type = "n", xlab = xlab, xlim = xlim, ylim = ylim, axes = F, xaxs = "i", yaxs = "i", ylab = ylab )  
axis( 1, at = seq( 0, length( hc$peels ), 1 ), labels = max( hc$yr ) - rev( c( hc$peels, max(hc$peels+1 ) ) ),
	cex.axis = 0.8, mgp = c( 2, 0.5, 0 ) ) 
axis( 2, at = seq( -100, max( xall, 30 ) + 50, ifelse( max( xall, 30 ) > 150, 50, 25 ) ),
	tick = seq( -100, max( xall, 30 ) + 50, ifelse( max( xall, 30 ) > 150, 50, 25 ) ), 
	cex.axis = 0.8, mgp = c( 2, 0.5, 0 ) )
box()
out <- NULL
for ( j in 1 : length( runs ) ) {
	change <- d[d$level == rev( runs )[j],]$pop.change
      change <- ifelse( change > ymax, max( ymax + 50, 45 ), change )
      den <- stats::density( change, adjust = 1 )
	y1 <- den$x
      x1 <- den$y / max( den$y ) + j - 0.5
	CR <- sum( ifelse( change <= ifelse( A1, -90, -80 ), 1, 0 ) ) / length( change ) * 100
	EN <- sum( ifelse( change> ifelse( A1, -90, -80 ) & change<= ifelse( A1, -70, -50 ), 1, 0 ) ) / length( change ) * 100
	VU <- sum( ifelse( change> ifelse( A1, -70, -50 ) & change<= ifelse( A1, -50, -30 ), 1, 0 ) ) / length( change ) * 100
	NT <- sum( ifelse( change> ifelse( A1, -50, -30 ) & change<= ifelse( A1, -40, -20 ), 1, 0 ) ) / length( change ) * 100
	LC <- sum( ifelse( change > ifelse( A1, -40, -20 ), 1, 0 ) ) / length( change ) * 100
	old_status <- c( CR, EN, VU, NT, LC )
	Decline <- round( sum( ifelse( change < 0, 1, 0 ) ) / length( change ) * 100, 1 )
	if ( sum( round( old_status, 0 ) ) != 100 ) {
		CR <- round_realsum( old_status, 0 )[1]
    		EN <- round_realsum( old_status, 0 )[2]
    		VU <- round_realsum( old_status, 0 )[3]
		NT <- round_realsum( old_status, 0 )[4]
		LC <- round_realsum( old_status, 0 )[5]
	} else{
    		CR <- round( CR, 0 )
    		EN <- round( EN, 0 )
    		VU <- round( VU, 0 )
    		if ( NT_opt == TRUE ) { NT = round( NT, 0 ) }
    		LC <- round( LC, 0 )
	}
      Change3xGL <- round( median( change ), 3 )
      percentages <- c( CR, EN, VU, NT, LC )
      status <- ifelse( which( percentages == max( percentages ) ) == 4 & 
		max( percentages ) < 50, "NT", categories[3 : 6][which( percentages == max( percentages ) ) ] )
	out <- rbind( out, data.frame( Year = max( hc$yr ) - rev( runs )[j], Change3xGL, CR, EN, VU, LC, status ) )
	lc <- c( ifelse( A1, -40, -30 ) )
      polygon( c( x1[y1 >= lc], rep( min( x1 ), length( y1[y1 >= lc] ) ) ),
		c( y1[y1 >= lc], rev( y1[y1 >= lc] ) ), col = cols[1], border = cols[1] )
	nt <- c( ifelse( A1, -50, -30 ) )
      polygon( c( x1[y1 <= lc & y1 >= nt], rep( min( x1 ), length( y1[y1 <= lc & y1 >= nt] ) ) ), 
		c( y1[y1 < lc & y1 >= nt], rev( y1[y1 < lc & y1 >= nt] ) ), col = cols[2], border = cols[2] )
      vu <- c( ifelse( A1, -70, -50 ) )
      polygon( c( x1[y1 <= nt & y1 >= vu], rep( min( x1 ), length( y1[y1 <= nt & y1 >= vu] ) ) ), 
		c( y1[y1 < nt & y1 >= vu], rev( y1[y1 < nt & y1 >= vu] ) ), col = cols[3], border = cols[3] )
      en <- ifelse( A1, -90, -80 )
      polygon( c( x1[y1 < vu & y1 >= en], rep( min( x1 ), length( y1[y1 < vu & y1 >= en] ) ) ),
		c( y1[y1 < vu & y1 >= en], rev( y1[y1 < vu & y1 >= en] ) ), col = cols[4], border = cols[4] )
      polygon( c( x1[y1 < en], rep( min( x1 ), length( y1[y1 < en] ) ) ),
		c( y1[y1 < en], rev( y1[y1 < en] ) ), col = cols[5], border = cols[5] )
      polygon( c( x1, rep( min( x1 ), length( x1 ) ) ), c( y1, rev( y1 ) ) )
}
abline( h = 0, lty = 2 )
text( 1 : length( runs ), par( 'usr' )[4], ( out$status ), cex = 0.8, pos = 1, offset = 0.2 )
legend( par('usr' )[2] * 1.01, quantile( par( 'usr' )[3 : 4], 0.6 ), bty = 'n', xpd = NA,
	c( "LC", "NT", "VU", "EN", "CR" ), pch = 15, col = c( cols ), pt.cex = 2, cex = 0.9 )
SavePlot( paste0( "\\", species, "\\", species, "_Retrospective IUCN posterior plot under IUCN Criterion A2" ) )
graphics.off()

