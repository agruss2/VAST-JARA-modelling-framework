###############################################################################################################################
##
##  This script gathers utility data and functions
##
###############################################################################################################################

######## Define some data objects
"Mon" <- c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" )
"Month" <- c( "January", "February", "March", "April", "May", "June",
	"July", "August", "September", "October", "November", "December" )
"MON" <- casefold( Mon, upper = T )
"mon" <- casefold( Mon, upper = F )
"MONTH" <- casefold( Month, upper = F )

######## Define the "calc.POSIXct" function
calc.POSIXct <- function ( x, format = "%d/%m/%Y %I:%M", tz = "GMT" ) {

	y <- as.POSIXct( strptime( y, format = format, tz = tz ), tz = tz )
    	return( y )

}

######## Define the "calc.yyyymmdd" function
calc.yyyymmdd <- function ( x, format = "y m d" ) 	{

	CLASS <- class(x)
    	if( any( CLASS %in% c( "POSIXlt", "POSIXct" ) ) ){
        	x <- as.POSIXlt( x )
    	} else {
       	x <- as.POSIXlt( strptime( x, format = format ) )
   	}
   	yyyy <- as.character( format( x, format = c( "%Y" ) ) )
   	mm <-  as.numeric( format( x, format = c( "%m" ) ) )
   	mm <- ifelse( is.na( mm ), NA, ifelse( mm >= 10, as.character( mm ), paste( "0", as.character( mm ), sep = "" ) ) )
   	dd <-  as.numeric( format( x, format = c( "%d" ) ) )
   	dd <- ifelse( is.na( dd ), NA, ifelse( dd >= 10, as.character( dd ), paste( "0", as.character( dd ), sep ="" ) ) )        
   	yyyymmdd <- ifelse( is.na( yyyy ) | is.na( mm ) | is.na( dd ), NA, paste( yyyy, mm, dd, sep = "" ) )
   	return ( as.numeric( yyyymmdd ) )    
   
}    

######## Define the "calc.fish.year" function
calc.fish.year <- function ( x, origin.m = 10, out.class = "numeric" ) {
    if( !( out.class %in% c( "factor", "numeric", "character" ) ) ) {
		stop( "Not a valid out.class" )
    }
    CLASS <- class( x )
    if( any( CLASS %in% c( "Date", "date", "dates", "chron", "POSIXlt", "POSIXct" ) ) ) {
       	OBJ <- as.POSIXlt( x )
       	MONTHS <- as.numeric( format( OBJ, format = c( "%m" ) ) )
       	YEARS <- as.numeric( format( OBJ, format = c( "%Y" ) ) )
       	OUT <- ifelse( MONTHS >= origin.m, YEARS + 1, YEARS )
       	LEVELS <- sort( unique( OUT ) )       
	} else if( CLASS == "numeric" ) {   
		
		#### The variable should be the name of the field which is numeric, i.e., 20010102  
       	DAYS <- x %% 100
       	x <- x %/% 100
       	MONTHS <- x %% 100
       	YEARS <- x %/% 100
       	OUT <- YEARS + ( MONTHS >= origin.m )
       	LEVELS <- sort( unique( OUT ) )                  
    	} else {  
        	stop( "x is not a valid object" )
    	}
    	OUT <- switch( out.class, "factor" = factor( as.character( OUT ), levels = LEVELS ),
		"numeric" = OUT,
		"character" = as.character( OUT ) )
	return( OUT )
    
}

######## Define the "calc.month" function
calc.month <- function ( x ) {

	#### The variable should be the name of the field which is numeric, i.e., 20010102  
	CLASS <- class( x )
    	if( any( CLASS %in% c( "Date", "date", "dates", "chron", "POSIXlt", "POSIXct" ) ) ) {
       	OBJ <- as.POSIXlt( x )
       	month <- as.numeric( format( OBJ, format = c( "%m" ) ) )
   	} else if( CLASS == "numeric" ) {
         	month <- ( x %/% 100 ) %% 100
    	} else {
        	stop( "x is not a valid object" )
    	}
   	return ( month )

}

######## Define the "calc.fish.day" function
calc.fish.day <- function ( x, origin.m = 10 ) {
    
	CLASS <- class( x )
    	if( any( CLASS %in% c( "Date", "date", "dates", "chron", "POSIXlt", "POSIXct" ) ) ) {
         	stop( "Expecting numeric format 20010102" )
    	} else if( CLASS == "numeric" ) {
        	day <- x %% 100
        	x <- x %/% 100
        	month <- x %% 100
        	year <- x %/% 100
        	fyear <- year + ( month >= origin.m )
        	fday <- julian.sp( month,day, year ) - julian.sp( origin.m, 1, fyear - 1 )
    	} else {
        	stop("x is not a valid object")
 	}    
    	return( fday )

}

######## Define the "julian.sp" function
julian.sp <- function ( m, d, y, origin.m = 1, origin.d = 1, origin.y = 1960 ) {
	
	only.origin <- all( missing( m ), missing( d ), missing( y ) )
	if( only.origin ) m <- d <- y <- NULL #### Returns days since origin
	origin. <- c( origin.m , origin.d , origin.y )
	nms <- names( d )
	max.len <- max( length( m ), length( d ), length( y ) )	
	m <- c( origin.[1], rep( m, length = max.len ) )
	d <- c( origin.[2], rep( d, length = max.len ) )
	y <- c( origin.[3], rep( y, length = max.len ) )	
	y <- y + ifelse( m > 2, 0, -1 )
	m <- m + ifelse( m > 2, -3, 9 )
	c <- y %/% 100
	ya <- y - 100 * c
	out <- ( 146097 * c ) %/% 4 + ( 1461 * ya ) %/% 4 + ( 153 * m + 2 ) %/%5 + d +1721119	
	if( !only.origin ) {
		if( all( origin. == 0 ) ) out <- out[-1] else out <- out[-1] -out[1] #### Origin according to the S algorithm
	}
	names( out ) <- nms
	out

}

######## Define the "julian.sp2" function
julian.sp2 <- function ( x, origin = 19600101 )	{

	#### x should be the name of the field which is numeric, i.e., 20010102  
    	day <- x %% 100
    	x <- x %/% 100
    	month <- x %% 100
    	year <- x %/% 100
    	origin.d <- origin %% 100
    	origin <- origin %/% 100
    	origin.m <- origin %% 100
    	origin.y <- origin %/% 100
    	julian.sp( month, day, year, origin.m, origin.d, origin.y )

}

######## Define the "bin1" function
bin1 <- function ( x = stop( "Argument is missing" ), n = stop( "Argument is missing" ) ) {
	
	#### x is a numeric data vector, n is the number of bins we want it cut into.
    	#### bin1 returns an ordered factor with n levels and a roughly equal number
    	#### of data points in each level. NA values are permitted in x
    	x.not.na =x[!is.na( x )]
    	breaks <- c( min( x.not.na ) - 1, quantile( x.not.na, seq( from = 1 / n,
        	by = 1 / n, length = n - 1 ) ), max( x.not.na ) + 1 )
    	out <- ordered( cut( x, breaks = breaks ) )
    	sss <- summary( out )
    	if ( any( is.na( x ) ) ) {
        	most <- max( rev( sss )[-1] )
        	least <- min( rev( sss )[-1] )
    	}
    	else {
        	most <- max( sss )
        	least <- min( sss )
    	}
    	if ( most / least > 1.3 )
        warning( paste( "bins uneven, most/least =", most / least ) )
    	out

}

######## Define the "consist" function
######## This function deals with situations where proc and land are sufficently close
######## to each another that they can be said to be "consistent", that is if the catch were landed unprocessed. 
######## If so, the "consist" function returns "TRUE" 
consist <- function ( land, proc, cold )	{

    landLB <- ( 1 - ( 1 / 2 ) * ( 1 - ( 1 / cold ) ) ) * land
    landUB <- ( 1 + ( 1 / 2 ) * ( 1 - ( 1 / cold ) ) ) * land
    out <- ( landLB <= proc ) & ( proc <= landUB )
    return( out )

}

######## Define the "count.unique.values" function
######## This is a function to count up and return the number of unique records
######## in y for each unique value in x
count.unique.values <- function ( x, y ) {

    	COUNT <- tapply( y, x, function( x ) {
		L <- length( unique( x ) )
		return(L)
	} )
	return( COUNT )

}

######## Define the "Cut" function
######## The "Cut" function is identical to the "cut" function except that it uses midpoints as labels
Cut <- function ( x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE,  dig.lab = 3, ... )  {
    
	if ( !is.numeric( x ) ) 
        	stop( "'x' must be numeric" )
    	if ( length( breaks ) == 1 ) {
        	if ( is.na( breaks ) | breaks < 2 ) 
            	stop( "Invalid number of intervals" )
        	nb <- as.integer( breaks + 1 )
        	dx <- diff( rx <- range( x, na.rm = TRUE ) )
        	if ( dx == 0 ) 
            	dx <- rx[1]
        	breaks <- seq( rx[1] - dx / 1000, rx[2] + dx / 1000, len = nb )
    	}
   	else nb <- length( breaks <- sort( breaks ) )
    	if ( any( duplicated( breaks ) ) ) 
        	stop( "'breaks' are not unique" )
    	codes.only <- FALSE
    	if ( is.null( labels ) ) {
        	for ( dig in dig.lab : max( 12, dig.lab ) ) {
            	ch.br <- formatC( breaks, digits = dig, wid = 1 )
            	if ( ok <- all( ch.br[-1] != ch.br[-nb] ) ) 
                		break
        	}
        	labels <- if ( ok ) 
            	( breaks[-1] + breaks[-nb] ) / 2
        	else ( 1 : ( nb - 1 ) + 2 : nb ) / 2
    	}
    	else if ( is.logical( labels ) && !labels ) 
        	codes.only <- TRUE
    	else if ( length( labels ) != nb - 1 ) 
        	stop( "Labels/breaks length conflict" )
    	code <- .C( "bincode", x = as.double( x ), n = as.integer( length( x ) ), 
        	breaks = as.double( breaks ), as.integer( nb ), code = integer( length( x ) ), 
        	right = as.logical( right ), include = as.logical( include.lowest ), 
        	naok = TRUE, NAOK = TRUE, DUP = FALSE, PACKAGE = "base" )$code
    	if ( codes.only ) 
        	code
    	else factor( code, seq( labels ), labels )

}

######## Define the "find.most.common" function
######## This function finds and returns the maximum value in a vector of data
######## Ties can be broken RANDOMLY using "which.is.max" from the "nnet" package
find.most.common <- function ( xin, break.ties.at.random = FALSE ) {

	if( break.ties.at.random ) { require( nnet )}
	tmp.xin <- as.character( xin )
	tmp.table <- table( tmp.xin )
	if( length( tmp.table ) == 0 )	{
        	return( "NA" )
    	}
    	else {
        	tmp.max <- if( break.ties.at.random ) { which.is.max( tmp.table ) } else { which.max( tmp.table ) }
        	tmp.labels <- labels( tmp.table[tmp.max] )
        	return( tmp.labels )
    	}

}

######## Define the "find.outliers" function
######## This function uses either a multiple of the IQR or a specified range check and/or missing
######## values etc. to define "outliers"
find.outliers <- function ( x, chk.type = "iqr", iqr.n = 3, starr.p = NULL, starr.q = NULL, 
	range.floor = FALSE, range.ceiling = FALSE, find.NAs = TRUE, find.zeros = TRUE, record.log = TRUE, zap.log = FALSE ) {

	if( !( chk.type %in% c( "iqr", "range", "starr" ) ) ) {
		stop( "Invalid check type" )
	}
	CALL <- match.call()
	if( find.NAs ) { 
		O1 <- is.na( x )
	} else {
		O1 <- rep( FALSE, length( x ) )
	}
	if( find.zeros ) { 
		O2 <- x == 0
	} else { 
		O2 <- rep( FALSE, length( x ) )
	}
	if( range.floor ) {
		O3 <- x < range.floor
	} else {
		O3 <- rep( FALSE, length( x ) ) 
	}
	if( range.ceiling ) {
		O4 <- x > range.ceiling
	} else {
		O4 <- rep( FALSE, length( x ) ) 
	}
    	if( chk.type == "iqr" ) {
        	QN <- quantile( x, p = c( 0.25, 0.75 ), na.rm = TRUE )
        	IQRN <- IQR( x, na.rm = TRUE ) * iqr.n        
        	out <- ifelse( x < ( QN[1] - IQRN ) | x > ( QN[2] + IQRN ) | O1 | O2 | O3 | O4, TRUE, FALSE )
    	} else if( chk.type == "range" ) {
        	QN <- NA
        	out <- ifelse( x < range.chk[1] | x > range.chk[2] | O1 | O2 | O3 | O4, TRUE, FALSE )
    	} else if( chk.type == "starr" ) {
        	QN <- quantile( x, p = starr.p, na.rm = TRUE )
        	out <- ifelse( x < ( QN[1] * ( 1 / starr.q[1] ) ) | 
			x > ( QN[2] * ( starr.q[2] ) ) | O1 | O2 | O3 | O4, TRUE, FALSE )
    	}
	if(record.log) {
        	LOG <- data.frame( N = length( x ),
			outliers = length( out[out] ),
			spec_quan = if( chk.type != "range" ){ paste( "(", paste( QN, collapse = ", ", sep = ""), ")", sep = "" ) },
			missing = length( O1[O1] ),
			zeros = length( O2[O2] ),
			range.floor = length( O3[O3] ),
			range.ceiling = length( O4[O4] ) )
		sink( file = "outliers_found.log", append = file.exists( "outliers_found.log" ) )
        	print( CALL )
        	cat( "\n" )
        	print( LOG )
        	cat( "\n" )        
        	sink()        
    	}
    	return( out )

}

######## Define the "get.top.n" function
get.top.n <- function ( x, y, FUN = sum, n = 10, na.rm = TRUE, zap.zeros = TRUE, ... ) {
	
	TAB <- tapply( y, x, FUN, na.rm = TRUE, ... )
    	TAB <- sort( TAB, decreasing = TRUE )
	if( zap.zeros )	{
		TAB <- TAB[TAB > 0]
		if( length( TAB ) < n )	{
            	N <- length( TAB )
            	index <- 1 : N
            	warning( paste( "You have fewer non-zero names available than your specified n, ", 
				N, " names only will (can) be returned", sep = "" ) )
        	}
        	else {
            	index <- 1 : n
        	}
    	} else {
        	index <- 1 : n
    	}
    	out <- names( TAB )[index]
	return( out )

}

######## Define the "impute.value" function
######## VAR is the variable to correct
######## fix.index is a true/false vector of the values of VAR to correct
######## group.index is the grouping index vector
impute.value <- function ( VAR, fix.index, group.index, FUN = median, ... ) {

    	VAR[fix.index] <- NA
    	group.use <- group.index %in% unique( group.index[fix.index] )
    	temp <- tapply( VAR[group.use], group.index[group.use], FUN, ... )
    	temp.index <- names( temp )
    	VAR[fix.index] <- temp[match( group.index[fix.index], temp.index )]
    	return( VAR )

}

######## Define the "hist2d" function
######## This is an equivalent function of single.tab for continuous variables x, y
######## It turns x into factors with levels being the midpoints of xbreaks
hist2d <- function ( x, z, FUN = Sum, xbreaks, ... ) {

    x <- Cut( x, xbreaks ) 
    z <- tapply( z, list( x ), FUN, ... )
    return( list( x = as.numeric( levels( x ) ), z = z ) )

}

######## Define the "hist2d" function
######## This is an equivalent function of cross.tab for continuous variables x, y
######## It turns x into factors with levels being the midpoints of xbreaks
hist3d <- function ( x, y, z, FUN = Sum, xbreaks, ybreaks, ... ) {
    
    x <- Cut( x, xbreaks ) 
    y <- Cut( y, ybreaks )
    z <- tapply( z, list( x, y ), FUN, ... )
    return( list( x = as.numeric( levels( x ) ), y = as.numeric( levels( y ) ), z = z ) )

}

######## Define the "match.up" function
######## This is a function to match up values in one vector with those in
######## another vector using some common index
match.up <- function ( Dx, Ix, Iy, nomatch = NA, incomparables = FALSE, zap.NAs = FALSE ) {

	if( ! all( is.vector( Dx ) || is.vector( Ix ) || is.vector( Iy ) ) ) {
        	stop( "At least one of Dx, Ix, or Iy is not a vector" )
    	}
    	if( !any( unique( Ix ) %in% unique( Iy ) ) ) {
        	stop( "Ix and Iy contain no values in common" )
    	}
    	if( length( Dx ) != length( Ix ) ) {
        	stop( "The length of your data vector (", 
			deparse( substitute( Dx ) ), ") does not equal the length of the corresponding index (", 
			deparse( substitute( Ix ) ), ")" )
    	}
	OUT <- match( as.character( Iy ), as.character( Ix ), nomatch = nomatch, incomparables = incomparables )
    	Dy <- Dx[OUT]
	if(zap.NAs) {
		Dy[is.na( Dy )] <- rep( 0, length( Dy[is.na( Dy )] ) )
	}
    	return( Dy )

}

######## Define the "re.factor" function
re.factor <- function ( x, xlevels = NULL ) {
    
	if( !is.null( xlevels ) ) {
        	x <- ifelse( as.character( x ) %in% xlevels, as.character( x ), "Other" )
        	if( "Other" %in% unique( x ) ) xlevels <- union( xlevels, "Other" )
    	} else {
        	if( is.factor( x ) )
            	xlevels = levels( x )
        	else
            	xlevels = unique( x ) ;
    	}
    	x = factor( x, levels = xlevels)
}

######## Define the "collapse" function
collapse <- function ( data, var, group.index, FUN, use.names = F, ... ) {

   	col.index <- function ( x, select, psudo.select = substitute( select ) ) {
    		
		#### select: used in Global Env
    		#### psydo.select: used in function calls
		if (missing( select ) & missing( psudo.select ) )
      		vars <- TRUE
		else {
     			nl <- as.list( 1 : ncol( x ) )
            	names( nl ) <- names( x )      
            	vars <- eval( psudo.select, nl, parent.frame() )
		}
    	}    
     	VAR <- data[,col.index( data, psudo.select = substitute( var ) )]
     	if( is.factor( VAR ) )
         	stop( "It doesn't work with factors" )
     	tmp.split = split( VAR, group.index )
     	tmp.split = unlist( lapply( tmp.split, FUN = FUN, ... ), use.names = use.names )
     	return ( tmp.split )

}

######## Define the "cross.grid" function
cross.grid <- function ( x, levels, data, FUN = Sum, o.value = T, ... ) {

  	if( any( c( x, levels ) %in% colnames( data ) == F ) ) {
        	stop( "Names are not right" )
    	}
     l = list()
    	for ( o in levels ) {
        	l = c( l, list( data[[o]] ) )
    	}
   	tmp.tab <- tapply( data[[x]], l, FUN, ... )
   	if( o.value ) {
        	tmp.tab = ifelse( is.na( tmp.tab ), 0, tmp.tab )
    	}
	tmp.tab <- cbind( expand.grid( dimnames( tmp.tab ) ), expand.grid( tmp.tab ) )
   	names( tmp.tab ) <- c( levels, x )
   	tmp.tab

}

######## Define the "cross.mat" function
######## This function converts a matrix into a trellis compatible data table
######## mat: a matrix with similar columns
######## names.arg: the given column name of the data table
cross.mat <- function ( mat, col.names = colnames( mat ), names.arg ) {

  	colnames( mat ) <- col.names
    	data <- cbind( expand.grid( dimnames( mat ) ), expand.grid( mat ) )
    	colnames(data) <- names.arg
    	data

}
          
######## Define the "cross.tab" function
#### x: Rows
#### y: Columns
#### xlevels: Factor levels of x
#### ylevels: Factor levels of y
#### proption: If T, each cell is the proportion of the value to the row total
cross.tab <- function ( x, y, z, xlevels = NULL, ylevels = NULL, FUN = Sum, total = F, proption = F, o.value = F, ... ) {

	x = re.factor( x, xlevels )
    	y = re.factor( y, ylevels )
    	if( total == T ) {
        	x = factor( x, levels = c( levels( x ), "Total" ) )
        	y = factor( y, levels = c( levels( y ), "Total" ) )
    	}
    	tmp.tab <- tapply( z, list( x, y ), FUN, ... )
    	if( o.value ) {
        	tmp.tab = ifelse( is.na( tmp.tab ), 0, tmp.tab )
    	}
    	if( total == T ) {
        	tmp.tab[,"Total"] <- apply( tmp.tab, 1, Sum )
        	tmp.tab["Total",] <- apply( tmp.tab, 2, Sum )
    	}      
    	if( proption == T ) {
        	if( total == T ) {
            	tmp.tab[,1 : ( ncol( tmp.tab ) - 1 )] <- tmp.tab[,1 : ( ncol( tmp.tab ) - 1 )] / tmp.tab[,ncol( tmp.tab )]
		} else {
           	 	total <- apply( tmp.tab, 1, Sum )
            	tmp.tab[,1 : ncol( tmp.tab )] <-  tmp.tab[,1 : ncol( tmp.tab )] / total
		}       
    	}
    	return( tmp.tab )

}

######## Define the "do.cross.tab" function
do.cross.tab <- function ( x, covariates, cross.func = "sum", exclude = c( NA, NaN ), row.vars = NULL, col.vars = NULL, 
	row.totals = TRUE, col.totals = TRUE, sweep.rows = TRUE, do.round = TRUE, digits = 0 ) {

   	cross.func <- eval( parse( text = cross.func ) )
    	TAB <- tapply( x, covariates, cross.func, na.rm = TRUE )
	if( row.totals ) {
        	TOTALS <- apply( TAB, 1, sum )
        	TAB <- cbind( TAB, "Total" = TOTALS )
    	}
    	if( col.totals ) {
        	TOTALS <- apply( TAB, 2, sum )
        	TAB <- rbind( TAB, "Total" = TOTALS )
    	}
    	if( sweep.rows ) {
        	TAB[-nrow( TAB ), -ncol( TAB )] <- sweep( TAB[-nrow( TAB ), -ncol( TAB )], 1, TAB[, ncol( TAB )], "/" )
    	}
	if( do.round ) {
        	TAB <- round( TAB, digits = digits )
    	}
    	TAB

}

######## Define the "make.grid" function. This is a convenience function for generating a grid suitable for 
######## lattice plotting functions from a ragged array, typically a data.frame
make.grid <- function ( y, x, grid.names = NULL, FUN = sum, as.props = FALSE, fix.nas = TRUE, ... ) {

  	TAB <- tapply( y, x, FUN, na.rm = fix.nas, ... )
	if( all( as.props ) ) {
		TAB <- sweep( TAB, as.props, apply (TAB, as.props, sum, na.rm = fix.nas ), "/" ) 
	}
	GRD <- expand.grid( dimnames( TAB ) )
    	GRD <- cbind( GRD, y = as.vector( TAB ) )
	if( fix.nas ) {
		GRD$y <- ifelse( is.na( GRD$y ), 0, GRD$y )
	}
	if( !is.null( grid.names ) ) {
        	names( GRD ) <- grid.names
    	} else {
        	names( GRD ) <- if( is.list( x ) && ( length( x ) > 1 ) ) {
			c( paste( "x", 1 : length( x ), sep = "" ), "y" )
		} else {
        		c( "x","y" )
		}
  	}
    	GRD

}

######## Define the "subsets" function
subsets <- function ( r, n, v = 1 : n ) {
  	
	if( r <= 0 ) NULL
  	else if( r >= n ) v[1:n]
  	else rbind( cbind( v[1], Recall( r - 1, n - 1, v[-1] ) ), Recall( r, n - 1, v[-1] ) )

}

######## Define the "single.tab" function
#### x: Rows
#### xlevels: Factor levels of x
#### proption: if T, each cell is the proportion of the value to the row total
single.tab <- function ( x, z, xlevels = NULL, FUN = Sum, total = F, proption = F, o.value = T, ... ) {

	x = re.factor( x, xlevels )
    	tmp.tab <- tapply( z, x, FUN, ... )
    	if( o.value ) {
        	tmp.tab = ifelse( is.na( tmp.tab ), 0, tmp.tab )
    	}
    	total.val <- Sum( tmp.tab )
    	if( proption == T ) {
        	tmp.tab <- tmp.tab / total.val
    	}
    	if( total == T ) {
        	tmp.tab["Total"] <- total.val
    	}          
    	return( tmp.tab )

}

######## Define the "Table" function
Table <- function ( ... ) {

	if ( length( list( ... ) ) == 1 ) return( c( table( ..., exclude = c() ), "TOTAL.OBS" = length( ... ) ) )
    	else {
        	ANS <- table( ..., exclude = c() )
        	return( rbind( ANS, "TOTAL.OBS" = apply( ANS, 2, sum ) ) )
    	}

}

######## Define the "pr.table" function. This function prints a table in a form easily transferrable to Word
######## (i.e., with columns separated by tabs)
pr.table <- function ( tble, file = '', append = T ) {
 
  	if( length( dim( tble ) ) < 2 ) tble <- matrix( tble, 1, dimnames = list('', names( tble ) ) )
  	cat( "\t", paste( dimnames( tble )[[2]], collapse = "\t" ), "\n", sep = "", file = file, append = append )
  	for( i in 1 : nrow( tble ) )
    		cat( dimnames( tble )[[1]][i], "\t", paste(tble[i,  ], collapse = "\t" ), "\n", 
			sep = "", file = file, append = T )
  	invisible()

}

######## Define the "word.table" function
word.table <- function ( x, digits = 2, na = NA ) {

	write.table( format( Round( x, digits ) ), file = 'clipboard', sep = '\t',
		dimnames.write = F, na = na, end.of.row = "\n" )

}

######## Define the "summary.default" function
summary.default <- function ( object, ..., digits = max( options()$digits - 3, 3 ) ) {

	if( length( levels( object ) ) ) return( summary.factor( object, ... ) )
	value <- if( is.numeric( object ) ) {
  		nas <- is.na( object )
  		object <- object[!nas]
  		qq <- quantile( object )
  		qq <- signif( c( qq[1:3], mean( object ), qq[4:5] ), digits )
  		names( qq ) <- c( "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max." )
  		if( any( nas ) ) qq <- c( qq, "NA's" = sum( nas ) )
  		qq <- c( qq, "Var" = var( object ), "CV" = sqrt( var( object ) ) / mean( object ) )
  		qq <- c( qq[1 : ( length( qq ) - 2 )], signif( qq[( length( qq ) - 1 ) : length( qq )], digits ) )
  		qq
  	} else if( is.recursive( object ) && !is.language( object ) && ( n <- length( object ) ) ) {
		sumry <- array( "", c( n, 3 ), list( names( object ), c( "Length", "Class", "Mode" ) ) )
  		ll <- numeric( n )
  		for( i in 1 : n ) {
    			ii <- object[[i]]
    			ll[i] <- length( ii )
    			sumry[i, 2] <- class( ii )
    			sumry[i, 3] <- mode( ii )
    		}
  		sumry[, 1] <- format( ll )
  		class( sumry ) <- "table"
  		sumry
  	}
	else c( Length = length( object ), Class = class( object ), Mode = mode( object ) )
	class( value ) <- "table"
	value

}

######## Define the "Format" function
Format <- function ( x, n = 2, pad = F, na.string = "NA" ) {
  
	old.options <- options()$digits
  	options( digits = 17 )
  	if( pad ) {
   	 	digits <- max( nchar( as.character( round( x, 0 ) ) ), na.rm = T )
    		is.negative <- ifelse( !is.na( x ) & x < 0, T, F )
    		ANS <- format( round( abs( x ) + 10^digits, n ), nsmall = n, scientific = F )
    		na.string <- paste( paste( rep( " ", max( nchar( ANS ) - 1 ) - nchar( na.string ) ),
			collapse = ""), na.string, sep = "" )
    		ANS <- ifelse( !is.na( x ), substring( ANS, 2, nchar( ANS ) ), na.string )
    		if( length( is.negative[is.negative == T] ) >0 ) {
      		ANS <- ifelse( is.negative, paste( "-", ANS, sep = "" ), paste( " ", ANS, sep = "" ) )
    		}
  	} else {
    		ANS <- format( round( x, n ), nsmall = n, scientific = F )
    		na.string <- paste( paste( rep( " ", max( nchar( ANS ) - 1 ) - nchar( na.string ) ),
			collapse = ""), na.string, sep = "" )
    		ANS <- ifelse( ANS == "NA", na.string, ANS )
  	}
  	options( digits = old.options )
  	return( ANS )

}

######## Define the "na.tighten.omit" function
na.tighten.omit <- function ( frame ) {

	tighten( na.omit( frame ) )

}

######## Define the "tighten" function. This function returns a data frame where all factors (ordered) 
######## have been refactored to take out redundant levels
"tighten" <- function ( data.frame ) {

	vars <- seq( length = length( data.frame ) )
	for( j in vars ) {
  		x <- data.frame[[j]]
  		if( is.factor( x ) ) {
    			if( any( class( x ) == "ordered" ) )
      			data.frame[[j]] <- ordered( x )
    			else
      			data.frame[[j]] <- factor( x )
  		}
	}
	data.frame
  
}

######## Define the "Between" function. This function determines whether x0 is between x1 and x2, returning
######## 1 if x0 < xlo, 2 if x0 = xlo, 3 if xlo <x0 < xhi, 4 if xo = xhi and 5 if x > xhi
######## where xlo = min( x1, x2 ), xhi = max( x1, x2 )
######## For the special case where x1 == x2, the function never returns 3 or 4, and it returns 2 if x0 = x1 = x2
Between <- function ( x0, x1, x2 ) {
	
	
		if(x0 < x1) {
			out <- if(x0 < x2) 1 else if(x0 == x2)
				2
			else 3
		}
		else if(x0 > x1) {
			out <- if(x0 < x2) 3 else if(x0 == x2)
				4
			else 5
		}
		else {
			out <- if(x0 < x2) 2 else if(x0 == x2)
				2
			else 4
		}
		out
	}


######## Define a suite of useful mathematical and statistic functions
is.even <- function ( x ) ifelse( ( x / 2 ) == ( x%/%2 ), T, F )
is.odd <- function ( x ) ifelse( ( x / 2 ) == ( x%/%2 ), F, T )
in.range <- function ( x, v ){ ifelse( x >= range( v )[1] && x <= range( v )[2], TRUE, FALSE ) }
is.Finite <- function ( x ){ ( ( is.numeric( x ) || is.complex( x ) ) & !is.na( x ) ) & x != Inf }
"Max" <- function ( ..., na.rm = T ) max( ..., na.rm = na.rm )
"Mean" <- function ( x, na.rm = T,... ) mean( x, na.rm = na.rm, ... )
"Median" <- function ( x, na.rm = T, ... )	median( x, na.rm = na.rm, ... )
"Min" <- function ( ..., na.rm = T ) min( ..., na.rm = na.rm )
"Quantile" <- function ( x, probs = seq( 0, 1, 0.25 ), na.rm = T, names = T, type = 7, ... ) {
	quantile( x, probs = probs, na.rm = na.rm, names = names, type = type, ... )
}
"Sum" <- function ( ..., na.rm = T ) sum( ..., na.rm = na.rm )
"Var" <- function ( x, y = x ) {

	new.x <- x[!is.na( x )]
  	new.y <- y[!is.na( y )]
  	return( var( new.x, new.y ) )

}
skew <- function ( x ) 3 * ( Mean( x ) - Median( x ) ) / sqrt( Var( x ) )
geometric.mean <- function ( x ) exp( sum( log( x ) ) / length( x ) )
Intersect <- function ( ... ) {

  	stuff <- lapply( list( ... ), unique )
  	answer <- stuff[[1]]
  	for( i in seq( along = stuff )[-1] )
  		answer <- answer[match( stuff[[i]], answer, nomatch = 0 )]
  	return( answer )

}

######## Define the "rlognorm" function. This function generates random numbers under a lognormal distribution,
######## with specificed c.v
rlognorm <- function ( number, mean, CV ) {

  	if ( !( mean > 0 ) ) return( rep( NA, number ) )
  	else {
    		logvar <- sqrt( log( CV^2 + 1 ) )
    		logmean <- log( mean ) - ( logvar^2 ) / 2
    		return( exp( rnorm( number, logmean, logvar ) ) )
  	}

}

######## Define the "Rlognorm" function. This function generates random numbers under a lognormal distribution,
######## with specificed c.v.
Rlognorm <- function ( number, mean, CV ) {

  	if ( !( mean > 0 ) ) return( rep( NA, number ) )
  	else {
    		logvar <- sqrt( log( CV^2 + 1 ) )
    		logmean <- log( mean ) - ( logvar^2 ) / 2
    		return( exp( Rnorm( number, logmean, logvar ) ) )
  }

}

######## Define the "Rnorm" function. This function generates a random normal distribution, 
######## allowing a standard deviation of zero
Rnorm <- function (n, mean=0, sd=1) {


  if(exists("is.R") && is.R()=="TRUE") return(rnorm(n,mean,sd))
  else {
    if(sd > 0) return(.Internal(rnorm(n, par1 = mean, par2 = sd),"S_rfuns", T, 9))
    else {
      res <- rep(0, ifelse(length(as.vector(n)) == 1, n,length(as.vector(n))))
      return(res + mean)
    }
  }
}

######## Define the "qqDist" function
qqDist <- function ( x, standardise = F, add.median = F, ... ) {
  
	n <- length( x )
  	seq.length <- min( 1000, n )
  	if( standardise ) {
    		SEQ <- seq( 1, 2 * n + 1, length = seq.length) / 2
    		U <- qnorm( qbeta( 0.975, SEQ, rev( SEQ ) ) )
    		L <- qnorm( qbeta( 0.025, SEQ, rev( SEQ ) ) )
    		if( add.median ) M <- qnorm( qbeta( 0.5, SEQ, rev( SEQ ) ) )
  	} else {
    		SD <- sqrt( var( x ) * ( n + 1 ) / n )
    		SEQ <- seq( 1, 2 * n + 1, length = seq.length ) / 2
    		U <- mean( x ) + SD * qt( qbeta( 0.975, SEQ, rev( SEQ ) ), n - 1 )
    		L <- mean( x ) + SD * qt( qbeta( 0.025, SEQ, rev( SEQ ) ), n - 1 )
    		if( add.median ) M <- mean( x ) + SD * qt( qbeta( 0.5, SEQ, rev( SEQ ) ), n - 1 )
  	}
  	X <- qnorm( ( SEQ - 0.25 ) / ( n + 0.5 ) )
  	qqnorm( x, main = "", ... )
  	lines( X, U, type = "l" )
  	lines( X, L, type = "l" )
  	if( add.median ) lines( X, M, type = "l" )
  	invisible()

}

######## Define the "ci" function
ci <- function ( data, z.value = 1.96 ) {

  	data.mean <- mean( data, na.rm = T )
  	data.se <- sqrt( Var( data ) / length( data[!is.na( data )] ) )
  	upper.ci <- data.mean + z.value * data.se
  	lower.ci <- data.mean - z.value * data.se
  	return( c( mean = data.mean, std.error = data.se, CV = round( sqrt( Var( data ) ) / data.mean * 100, 2 ), 
		lower.ci = lower.ci, upper.ci = upper.ci ) )

}


######## Define the "proportion.ci" function. This function uses the exact F distribution 
#### to determine the exact confidence intervals
#### r can be a proportion or a number
proportion.ci <- function ( r, n, ci = 0.95 ) {

  	r <- ifelse( r < 1, round( r * n ), r )
  	t1 <- 1 - ( 1 - ci ) / 2
  	old.warn <- options()$warn
  	options( warn = -1 )
  	F1 <- qf( t1, 2 * n - 2 * r + 2, 2 * r )
  	F2 <- qf( t1, 2 * r + 2, 2 * n - 2 * r )
  	options( warn = old.warn )
  	lower.ci <- r / ( r + ( n - r + 1 ) * F1 )
  	upper.ci <- ( r + 1 ) / ( r + 1 + ( n - r ) / F2 )
  	lower.ci <- ifelse( is.na( lower.ci ) & !is.na( n ) & !is.na( r ), 0, lower.ci )
 	upper.ci <- ifelse( is.na( upper.ci ) & !is.na( n ) & !is.na( r ), 1, upper.ci )
  	RES <- data.frame( r, n, p = r / n, lower.ci, upper.ci )
  	return( RES )

}

######## Define the "Sample" function
Sample <- function ( x, size = n, replace = F, prob = rep( 1, n ) ) {

	n <- length( x )
	if( n == 1 ) return( rep( x, size ) )
	else return( sample( x, size, replace, prob ) )

}

######## Define the "Area" function. This function finds the area of a polygon
#### corners$x, $y: Corners of polygon
#### Area( list( x = c( 0, 2, 3, 4, 4 ), y = c( 0, 2, 1, 2, 0 ) ) ) --> 5

Area <- function ( corners ) {

	if( length( corners$x ) < 3 | length( corners$y ) < 3 ) {
    		print( "ERROR Area(): Needs 3 or more points ************" )
    		return( NULL )
    	}
	if( length( corners$x ) != length( corners$y ) ) {
    		print( "ERROR Area(): Number of points in corners$y not equal to number of points for corners$x ************" )
    		return( NULL )
    	}
	TotArea <- 0
	SubArea <- 0
	n <- length( corners$x )
	I0 <- 0
	for( j in c( 1 : ( n ) ) ) {
  		if( !is.na( corners$y[j] ) & !is.na( corners$x[j] ) & I0 == 0 ) I0 <- j
  	}
	for( j in c( I0 : ( n ) ) ) {
   		i <- j
   		i1 <- j + 1
   		if ( i1 > n ) i1 <- I0
   		if( is.na( corners$y[i1] ) | is.na( corners$x[i1] ) ) i1 <- I0
		if( !is.na( corners$y[i] ) & !is.na( corners$x[i] ) ) {
     			SubArea <- SubArea + corners$x[i] * corners$y[i1]- corners$x[i1] * corners$y[i]
   		} else {
     			TotArea <- TotArea + abs( SubArea )
     			SubArea <- 0
     			I0 <- i1
     		}
   	}
	TotArea <-( TotArea + abs( SubArea ) ) / 2
	return( TotArea )

}

######## Define the "DistanceLongLat" function
#### Inputs: start = ( long1, lat1 ) and end = ( long2, lat2 ) in decimal degrees
#### OR assumes that locator is used to define exactly two points
#### Assumes longitude numbers are positive and that numbers > 180 are WESTINGS
#### Assumes lattitude negative numbers are SOUTHINGS
#### Outputs nautical miles if metres = F, else distance in metres
DistanceLongLat <- function ( long1, long2, lat1, lat2, metres = F ) {

	if( missing( long1 ) | missing( long2 ) | missing( lat1 ) | missing( lat2 ) ) {
  		cat( "Using function \"locator(2)\" to locate end points\n" )
    		x <- nz.locator(2)
    		long1 <- x$x[1]
    		long2 <- x$x[2]
    		lat1 <- x$y[1]
    		lat2 <- x$y[2]
    		print( unlist( c( long1, long2, lat1, lat2 ) ) )
  	}
  	long1 <- ( long1 * pi ) / 180
  	long2 <- ( long2 * pi ) / 180
  	lat1 <- ifelse( lat1 > 180, 360 - lat1, - lat1 )
  	lat2 <- ifelse( lat2 > 180, 360 - lat2,  - lat2 )
  	lat1 <- ( lat1 * pi ) / 180
  	lat2 <- ( lat2 * pi ) / 180
  	d <- 2 * asin( sqrt( ( sin( ( lat1 - lat2 ) / 2 ) )^2 + cos( lat1 ) * 
		cos( lat2 ) * ( sin( ( long1 - long2 ) / 2 ) )^2 ) )
  	nm <- ( d * 180 * 60 ) / pi
  	if( metres )
    	nm <- nm * 1.852 * 1000
  	return( nm )

}

######## Define the "distance.lat.long" function. This function returns the distance separating two points 
######## on the Earth's surface, in either kilometers or meters. It uses the simple spherical model. 
######## Points MUST be in decimalised degrees
distance.lat.long <-  function ( x1, y1, x2, y2, units = "metres" ) {

	radians <- function( xin ) {
        	tmp.rad <- ( pi * xin ) / 180
        	return( tmp.rad )
	}
	if(units == "metres") {
		r <- 6378388
	} else if( units=="km" ) {
        	r <- 6378.388
      } else if( is.numeric( units ) == TRUE ) {
        	r <- units
      } else {
        	stop( "Not a valid unit type..." )
      }
	tmp.trig <- ( sin( radians( y1 ) ) * sin( radians( y2 ) ) ) + ( cos( radians( y1 ) ) * 
		cos( radians( y2 ) ) * cos( radians( x1 ) - radians( x2 ) ) )
	tmp.d <- r * acos( tmp.trig )
	return( tmp.d )

}

######## Define the "AreaLongLat" function. This function finds the area of a polygon whose latitudes and longitudes 
######## are in the decimal form (e.g., 174.5° = 174 degrees 30 min)
AreaLongLat <- function ( Long, Lat ) {

	if( missing( Lat ) ) Poly <- Long else Poly <-list( x = Long, y = Lat )
	Tna <- !is.na( Poly$x )
	Poly$x[Tna] <- Poly$x[Tna] * ( 111.41 * cos( pi / 180 * Poly$y[Tna] ) - 0.01 * cos( 3 * pi / 180 * Poly$y[Tna] ) )
	Tna <- !is.na( Poly$y )
	Poly$y[Tna] <- Poly$y[Tna] * ( 111.14 - 0.56 * cos( 2 * pi / 180 * Poly$y[Tna] ) )
	return( Area( Poly ) )

}

######## Define the "inRegion" function. This function checks if rx,ry are in polygon "corners"
inRegion <- function ( corners, rx, ry ) {

	In <- rep( F, length( rx ) )
	a1 <- rep( 0, length( rx ) )
	n <- length( corners$x )
	for( j in c( 1 : ( n + 1 ) ) ) {
  		i <- j
  		if( j > n ) i <- 1
  		Inn <- ( ( corners$y[i] == ry ) & ( corners$x[i] == rx ) )
  		In[Inn] <- T
  		Atan <- atan2( corners$y[i] - ry, corners$x[i] - rx )
  		if( j == 1)  prevAtan <- Atan
 		 a0 <- Atan - prevAtan
		a0[!is.finite( a0 )] <- 0
  		a0[a0 > pi] <- a0[a0 > pi] - 2 * pi
  		a0[a0 <  - pi] <- a0[a0 <  - pi] + 2 * pi
  		a1 <- a1 + a0
  		prevAtan <- Atan
  	}
	In[!In] <- abs( abs( a1[!In] ) - 2 * pi ) < 0.001	
	if( length( In[!In] ) > 0 ) {
  		Inn <- abs( a1[!In] ) > 0.001
  		if( length( Inn[Inn] ) > 0 ) {
			print( "Queer points ... CHECK them" )
    			print( t( matrix( c( a1[!In][Inn] / 2 / pi * 360, c( 1 : n )[!In][Inn] ),
				ncol = 2, dimnames = list( NULL, c( "Sum angles", "Index of point" ) ) ) ) )
    		}
  	}	
	return( In )

}

######## Define the "convert.degrees" function. This function returns a character representation of decimal degrees
######## in degrees, minutes and decimal minutes. It assumes that negative numbers are latitudes
convert.degrees <- function ( x, digits = 3, symbol = F ) {

	degree <- abs( trunc( x ) )
	minute <- (abs( x ) - degree ) * 60
	if( symbol ) {
  		RES <- paste( Format( degree, 0, pad = T ), "\260", Format( minute, digits, pad = T ), "'", sep = "" )
  		a1 <- substring( RES, 1, regexpr( " ", RES ) - 1 )
  		a2 <- substring( RES, regexpr( " ", RES ) + 1, nchar( RES ) )
  		RES <- ifelse( nchar( a1 ) > 0, paste( a1, "0", a2, sep = "" ), a2 )
	} else RES <- paste( Format( degree, 0, pad = T ), " ", Format( minute, digits, pad = T ), sep = "" )
	return( RES )

}

######## Define the "geo.dist" function
######## This is a function to calculate the angular or great-circle distance between pairs of coordinates 
######## on the Earth's surface. Coordinates are entered in decimal degrees.
#### Arguments: 
#### x, y, x2, y2       Either matrices, data frames or vectors of the sets of
####                    coordinates of pairs of points between which the angle or
####                    distance is calculated. The coordinates of each set of
####                    points is either in a data frame or matrix or in a pair of
####                    vectors. In a data frame or matrix either the longitudes and
####                    latitudes have column names "long" and "lat respectively, or
####                    the first first and second column are the longitudes and
####                    latitudes, respectively. See Details for usage.
####    r               Local radius of the Earth in appropriate units. Default is
####                    r = 1 in which case the angular distance in radians is
####                    returned. Equatorial radius of the Earth is r = 6378.137 km.
####                    At NZ latitudes the local radius is approximately 6365 km.
#####                   If f (!= 0) is specified, then r must be the equatorial radius.
####    f               Flattening of the polar ellipse in the oblate spheroid model
####                    for the Earth. The default is f = 0 which assumes a sphere. 
####                    The actual flattening of the Earth is f = 0.00335281. 
####                    f can also be negative (prolate sphere).
####                    In general the local radius at latitude theta is
####                    r / sqrt( 1 + ( 2 * f - f^2 ) * ( sin( theta ) )^2 / ( 1 - f )^2 ) 
####  Details:
#### This function will handle any of the following command syntaxes in relation to the
#### entry of the coordinates of the pairs of points:
####    1. geo.dist( mat1, mat2 ),
####    2. geo.dist( mat1, vec3, vec4 ),              
####    3. geo.dist( mat1, vec3, y2 = vec4 ),         
####    4. geo.dist( mat1, x2 = vec3, y2 = vec4 ),
####    5. geo.dist( vec1, vec2, vec3, vec4 ),
#### where:
####    mat1 is a data frame or matrix with 1st set of coords,
####    mat2 is a data frame or matrix with 2nd set of coords,
####   vec1 and vec2 are vectors of 1st set of longs and lats,
####    vec3 and vec4 are vectors of 2nd set of longs and lats,
geo.dist <- function ( x, y, x2 = NULL, y2 = NULL, r = 1, f = 0 ) {

	if ( missing( y ) ) y <- NULL
	if ( is.matrix( x ) | is.data.frame( x ) ) {
		if ( all( c( "long", "lat" ) %in% colnames( x ) ) ) {
			long1 <- x[, "long"]
			lat1 <-  x[, "lat"]
		} else {
			long1 <- x[, 1]
			lat1 <-  x[, 2]
		}
		if ( is.matrix( y ) | is.data.frame( y ) ) {
            	if ( all( c( "long", "lat" ) %in% colnames( y ) ) ) {
                		long2 <- y[, "long"]
                		lat2 <-  y[, "lat"]
            	} else {
                		long2 <- y[, 1]
                		lat2 <-  y[, 2]
            	}
        	} else {
            	if ( !( is.vector( x2 ) & is.vector( y2 ) ) )
                		stop( "The coordinates of second points are not in the vector, matrix or data frame format" )
            	long2 <- x2
            	lat2 <-  y2
        	}
	} else {
        	if ( !( is.vector( x ) & is.vector( y ) & is.vector( x2 ) & is.vector( y2 ) ) )
            		stop( "The coordinates of points are not in the vector, matrix or data frame format" )
        	long1 <- x
        	lat1 <-  y
        	long2 <- x2
        	lat2 <-  y2
    	}
    	llg1 <- length( long1 )
    	llg2 <- length( long2 )
    	if ( llg1 != length( lat1 ) )
        	stop( "Vectors of latitude and longititude for first points are not of the same length" )
    	if ( llg2 != length( lat2 ) )
        	stop( "Vectors of latitude and longititude for second points are not of the same length" )
	if ( llg2 < llg1 ) {
        	l0 <- numeric( llg1 )
        	long2 <- long2 + l0
        	lat2 <- lat2 + l0
        	if( llg2 > 1 )
            	warning( "Second points are being recycled" )
    	}
    	if ( llg1 < llg2 ) {
		l0 <- numeric( llg2 )
        	long1 <- long1 + l0
        	lat1 <- lat1 + l0
        	if( llg1 > 1 )
            	warning( "First points are being recycled" )
    	}
     if ( any( abs( lat1 )[!is.na( lat1 )] >= 90 ) )
        	stop( "Some latitudes for the first points are out of range" )
    	if ( any( abs( lat2 )[!is.na( lat2 )] >= 90 ) )
        	stop( "Some latitudes for the second points are out of range" )
	long1 <- ( long1 %% 360) * pi / 180
    	long2 <- ( long2 %% 360 ) * pi / 180
    	lat1 <- lat1 * pi / 180
    	lat2 <- lat2 * pi / 180
	if ( abs( f ) > 1.0e-6 ) { 
		warning( paste( "Oblate spheroid Earth option not implemented;", "we are assuming that Earth is a sphere" ) )
    	}
	return( r * acos( sin( lat1 ) * sin( lat2 ) + cos( lat1 ) * cos( lat2 ) * cos( long1 - long2 ) ) )

}

######## Define the "dectodeg" function
"dectodeg" <- function ( x ) {

	return( ( trunc( x ) * 100 ) + ( ( ( ( x - trunc( x ) ) * 0.6 ) * 100 ) ) )

}

######## Define the "degtodec" function
"degtodec" <- function ( x ) {

	return( round( ( ( (x / 100 ) - trunc( x / 100 ) ) / 0.6 ) + trunc( x / 100 ), 3 ) )

}

######## Define the "get.Excel.file" function
make.filename <- function ( file = "", path = "", add.terminal = F ) {
  
	if( path != "" ) {
    		plc <- substring( path, nchar( path ) )
    		if( !( plc == "\\" | plc == "/" ) ) path <- paste( path, "\\", sep = "" )
  	}
  	filename <- paste( path, file, sep = "" )
  	if( add.terminal == T ) {
    		plc <- substring( filename, nchar( filename ) )
    		if( !(plc == "\\" | plc == "/" ) ) filename <- paste( filename, "\\", sep = "" )
  	}
  	return( filename )

}

######## Define the "get.Excel.file" function
get.Excel.file <- function ( file, path, sheet = "" ) {

  	filename <- make.filename( file, path )
  	channel <- odbcConnectExcel( filename )
  	indata <- sqlFetch( channel, sheet )
  	odbcClose( channel )
  	return( indata )

}

######## Define the "Save" function 
Save <- function ( File, Path, GraphName, dir = "Figures", ExportType = "WMF" ){
    
	if( !exists( ".Device", frame = 0 ) ) stop( "No active graphics device\n" )
    	if( .Device == "null device" ) stop( "No active graphics device\n" )
    	if( missing( Path ) ) Path <- ""
    	if( missing( File ) ) stop( "File must be supplied\n" )
    	dot <- regexpr( "\\.", File )
	if( dot != -1 ) File <- ( substring( File, 1, dot - 1 ) )
	if( substring( Path, nchar( Path ) ) != "\\" ) Path <- paste( Path, "\\", sep = "" )
    	res <- c()
    	i <- 0
    	while( regexpr( "[\\]", Path ) != -1 ) {
        	i <- i + 1
        	res[i] <- substring( Path, 1, regexpr( "[\\]", Path ) - 1 )
        	Path <- substring( Path, regexpr( "[\\]", Path ) + 1 )
    	}
    	if( dir != "" ) res[length( res )] <- dir
    	Path <- paste( res, collapse = "\\" )
    	if( !is.dir( Path ) ) mkdir( Path )
    	FileName <- paste( Path, "\\", File, ".", ExportType, sep = "" )
    	if( !is.dir( Path ) ) stop( "Path is invalid\n" )
    	savePlot( filename = FileName, type = ExportType, device = dev.cur() )
    	cat( paste( "Saved as\n  ", FileName, "\n\n", sep = "" ) )

}

######## Define the "pwd" function 
pwd <- function () getwd()
