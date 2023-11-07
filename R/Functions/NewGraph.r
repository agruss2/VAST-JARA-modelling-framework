###############################################################################################################################
##
##  NewGraph functions
##
###############################################################################################################################

######## Define the "new.graph" function
"new.graph" <- function ( size = 1, pointsize = 10, colour.type = "blue" ) {

	windows.options( reset = TRUE )
  	if ( !colour.type %in% c( "blue", "red", "brown" ) ) {
    		cat ( "Colour type not recognised, plotting in blue\n" )
    		colour.type <- "blue"
	}
    
  	#### Set up window size: Make sure this agrees with SavePlot
  	width <- ( 21 / 2.54 ) * 0.8
  	height <- ( 29.7 / 2.54 ) * 0.8
  	windows( width = width, height = height * size, pointsize = pointsize, bg = "white", rescale = "fixed" )
  
	#### Set sefault par parameters
 	par( xaxs = "i", yaxs = "i" )
  	par( las = 1 )
  	.par <<- par( no.readonly = TRUE )
  	.par$size <<- size
  	.par$pointsize <<- pointsize
  
	#### Mimic Splus default palette
  	if ( colour.type == "red" ) {
    		palette( c( "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
			"#CC79A7", "lightcyan", "lightpink", "mintcream", "moccasin", "lightskyblue1",
			"lightyellow", "lightpink2", "grey46", rev( rainbow( 21, start = 0, end = 0.35 ) ) ) )
  	} 
  	if ( colour.type == "blue" ) {
    	tmp <- c( rgb( 100, 255, 255, maxColorValue = 255 ), rgb( 100, 243, 255, maxColorValue = 255 ), 
		rgb( 100, 232, 255, maxColorValue = 255 ), rgb( 100, 220, 255, maxColorValue = 255 ), 
		rgb( 100, 208, 255, maxColorValue = 255 ), rgb( 100, 197, 255, maxColorValue = 255 ),
		rgb( 100, 185, 255, maxColorValue = 255 ), rgb( 100, 173, 255, maxColorValue = 255 ),
		rgb( 100, 162, 255, maxColorValue = 255 ), rgb( 100, 150, 255, maxColorValue = 255 ), 
		rgb( 100, 150, 255, maxColorValue = 255 ), rgb( 100, 150, 255, maxColorValue = 255 ), 	
		rgb( 89, 133, 255, maxColorValue = 255 ), rgb( 78, 117, 255, maxColorValue = 255 ), 
		rgb( 67, 100, 255, maxColorValue = 255 ), rgb( 55, 83, 255, maxColorValue = 255 ),
		rgb( 44, 67, 255, maxColorValue = 255 ), rgb( 33, 50, 255, maxColorValue = 255 ),
		rgb( 22, 33, 255, maxColorValue = 255 ), rgb( 11, 17, 255, maxColorValue = 255 ),
		rgb( 0, 0, 255, maxColorValue = 255 ) )
    	palette( c( "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
		"lightcyan", "lightpink", "mintcream", "moccasin", "lightskyblue1", "lightyellow", "lightpink2", 
	"grey46", tmp ) )
  	}
  	if ( colour.type == "brown" ) {
    		tmp <- c( "#663000", "#7C4519", "#935B30", "#AB744D", "#C18F6B", "#CF9F81", "#D5A88E", "#DCB59F", 
			"#E8C8B8", "#F0DBD0", "#E1EBE6", "#CEFBFC", "#B8FBFF", "#A1F8FF", "#8BF5FF", "#74F2FF", "#5EEDFF", 
			"#49E8FF", "#2FDEF9", "#1DC3E2", "#00AACC" )
    		palette( c( "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
			"lightcyan", "lightpink", "mintcream", "moccasin", "lightskyblue1", "lightyellow", 
			"lightpink2", "grey46", tmp ) )
	}
  	icol <<- palette()[17:37]
  	invisible()

}

######## Define the "reset.par" function
"reset.par" <- function ( x = .par ) {

	#### Resets par parameters to a set of default values, ignoring non-standard parameters
  	#### with a default of '.par'. ('.par' is created when new.graph is called)
  	PAR <- x
  	PAR <- PAR[names( PAR ) %in% names( par( no.readonly = T ) )]
  	par( PAR )

}

######## Define the "SavePlot" function
"SavePlot" <- function ( filename = NULL, path = DIR$Figures, size = .par$size, pointsize = .par$pointsize, res = 200,
	type = "jpg" ) {

  		#### Path set for ANT
  		#### Valid types are JPG (default), PNG, and PDF.
  		#### The type is automatically added as an extension
  		#### size represents a fraction of the height of an A4 page
  		#### res = the resolution in 'dpi'. Values above about 500 may cause errors
  		#### Dimensions (width & height) specify 80% of an A4 page (in inches)
  		width <- ( 21 / 2.54 ) * 0.8
  		height <- ( 29.7 / 2.54 ) * 0.8
  		if ( is.null( filename ) ) stop( "Please specify a filename" )
  		this.image <- recordPlot()
  		valid <- c( "jpg", "png", "pdf" )
  		Type <- valid[match( tolower( type ), valid )]
  		if ( is.na( Type ) ) {
    			stop( paste( type, "is an invalid file type" ) )
  		} else if ( Type == "png" ) {
    			filename <- paste( make.filename( filename, path ), ".png", sep = "" )
    			png( filename = filename, width = width, height = height * size, units = "in", 
				pointsize = pointsize, bg = "white", res = res, restoreConsole = TRUE )
    		replayPlot( this.image )
    		dev.off()
		} else if ( type == "jpg" ) {
    			filename <- paste( make.filename( filename, path ), ".jpg", sep = "" )
    			jpeg( filename = filename, width = width, height = height * size, units = "in", quality = 100,
				pointsize = pointsize, bg = "white", res = res, restoreConsole = TRUE )
    			replayPlot( this.image )
    			dev.off()
  		} else if ( type == "pdf" ) {
    			filename <- paste( make.filename( filename, path ), ".pdf", sep = "" )
    			pdf( file = filename, width = width, height = height * size, pointsize = pointsize, bg = "white" )
    			replayPlot( this.image )
    			dev.off()
  		}
  		return( filename )

}

######## Define the "demo.symbols" function
demo.symbols <- function () {

	plot( c( 0, 9 ), c( 1, 9 ), xlab = "", ylab = "", axes = F, type = "n", xaxs = "i", yaxs = "i" ) 
  	mtext( side = 3, text = "R lines and symbols" )
  	for ( i in 1 : 8 ) {
    		abline( h = i, lty = i )
    		text( 1, i + 0.2, as.character( i ) )
  	}
  	for ( i in 1 : 7 ) {
    		points( 2, i + 0.5, pch = i, cex = 3 )
    		text( 2.7, i + 0.5, as.character( i ) )
    		points( 4, i + 0.5, pch = i + 6, cex = 3 )
    		text( 4.7, i + 0.5, as.character( i + 8 ) )
    		points( 6, i + 0.5, pch = i + 12, cex = 3 )
    		text( 6.7, i + 0.5, as.character( i + 12 ) )
    		points( 8, i + 0.5, pch = ( i + 18 ), cex = 3 )
    		text( 8.7, i + 0.5, as.character( i + 18 ) )
	}
  	invisible()

}

######## Define the "minor.tick" function
minor.tick <- function ( nx = 2, ny = 2, tick.ratio = 0.5 ) {

	ax <- function( w, n, tick.ratio ) {

    		range <- par( "usr") [if ( w == "x" )
        		1 : 2
    			else 3:4]
    		tick.pos <- if ( w == "x" )
        		par( "xaxp" )
    			else par( "yaxp" )
    		distance.between.minor <- ( tick.pos[2] - tick.pos[1] ) / tick.pos[3] / n
    		possible.minors <- tick.pos[1] - ( 0 : 100 ) * distance.between.minor
    		low.minor <- min( possible.minors[possible.minors >= range[1]] )
    		if ( is.na( low.minor ) )
        		low.minor <- tick.pos[1]
    		possible.minors <- tick.pos[2] + ( 0 : 100 ) * distance.between.minor
    		hi.minor <- max( possible.minors[possible.minors <= range[2]] )
    		if ( is.na( hi.minor ) )
        		hi.minor <- tick.pos[2]
    		axis( if ( w == "x")
      		1
    			else 2, seq( low.minor, hi.minor, by = distance.between.minor ),
       			labels = FALSE, tcl = par( "tcl" ) * tick.ratio )
	}

  	if ( nx > 1 ) ax( "x", nx, tick.ratio = tick.ratio )
  	if ( ny > 1 ) ax( "y", ny, tick.ratio = tick.ratio )
	invisible()

}
