###############################################################################################################################
##
##  plot_population_index function
##
###############################################################################################################################

plot_population_index <- function ( fit, year_labels = NULL, years_to_plot = NULL, DirName = paste0( getwd(), "/" ), 
	PlotName = "Index", interval_width = 1, strata_names = NULL, category_names = NULL, use_biascorr = TRUE, 
    	plot_legend = TRUE, total_area_km2 = NULL, plot_log = FALSE, width = NULL, height = NULL, 
	create_covariance_table = FALSE, Yrange = c( ifelse( plot_log == TRUE, NA, 0 ), NA ), TmbData = fit$data_list, 
	Sdreport = fit$parameter_estimates$SD, extrapolation_list = fit$extrapolation_list, ... ) {

    		if ( is.null( Sdreport ) ) 
        		stop( "Sdreport is NULL; please provide Sdreport" )
    		if ( !is.null( category_names ) && length( category_names ) != TmbData$n_c ) 
        		stop( " category_names` must have the same length as `TmbData$n_c`" )
    		if ( !is.null( year_labels ) && length( year_labels ) != TmbData$n_t ) 
        		stop( "`year_labels` must have the same length as `TmbData$n_t`" )
    		if ( !is.null( strata_names ) && length( strata_names ) != TmbData$n_l ) 
        		stop( "`strata_names` must have the same length as `TmbData$n_l`" )
    		if ( "ln_Index_ctl" %in% rownames( TMB::summary.sdreport( Sdreport ) ) ) {
        		ParName = "Index_ctl"
    		} else if ( "ln_Index_cyl" %in% rownames( TMB::summary.sdreport( Sdreport ) ) ) {
        		ParName = "Index_cyl"
        		TmbData[["n_t"]] = nrow( TmbData[["t_yz"]] )
    		} else {
        		stop( "`plot_biomass_index` is not compatible with your version" )
    		}
    		if ( !( "t_i" %in% names( TmbData ) ) ) {
        		TmbData$t_i = TmbData$t_iz[, 1]
    		}
    		if ( !( "t_yz" %in% names( TmbData ) ) ) {
        		TmbData$t_yz = matrix( 1 : TmbData$n_t - 1, ncol = 1 )
    		}
    		mfrow = c( ceiling( sqrt( TmbData$n_c ) ), ceiling( TmbData$n_c / ceiling( sqrt( TmbData$n_c ) ) ) )
    		if ( is.null( width ) ) 
        		width = mfrow[2] * 3
    		if ( is.null( height ) ) 
        		height = mfrow[1] * 3
    		if ( is.null( year_labels ) ) 
        		year_labels = 1 : TmbData$n_t
    		if ( is.null( years_to_plot ) ) 
        		years_to_plot = 1 : TmbData$n_t
    		if ( is.null( strata_names ) ) 
        		strata_names = 1 : TmbData$n_l
    		if ( is.null( category_names ) ) 
        		category_names = 1 : TmbData$n_c
    		if ( "unbiased" %in% names( Sdreport ) ) {
        		if ( all( is.na( Sdreport$unbiased$value ) ) ) {
            		stop( paste0( "You appear to be using bias-correction, but all values are NA. ", 
					"Please report problem to the package author." ) )
        		}
    		}
    		if ( "treat_nonencounter_as_zero" %in% names( TmbData$Options_list$Options ) ) {
        		treat_missing_as_zero = TmbData$Options_list$Options["treat_nonencounter_as_zero"]
    		} else {
        		treat_missing_as_zero = FALSE
    		}
    		SD = TMB::summary.sdreport( Sdreport )
    		if ( !"report" %in% names( as.list( args( TMB:::as.list.sdreport ) ) ) ) {
        		warning( "package `TMB` should be updated to easily access standard errors" )
    		}
    		SD_stderr = TMB:::as.list.sdreport( Sdreport, what = "Std. Error", report = TRUE )
    		SD_estimate = TMB:::as.list.sdreport( Sdreport, what = "Estimate", report = TRUE )
    		if ( use_biascorr == TRUE && "unbiased" %in% names( Sdreport ) ) {
        		SD_estimate_biascorrect = TMB:::as.list.sdreport( Sdreport, what = "Est. (bias.correct)", report = TRUE )
    		}
    		if ( any( is.na( SD_estimate ) ) | any( is.na( SD_stderr ) ) ) {
        		stop( "Problem: Standard errors contain NAs" )
    		}
    		if ( ParName %in% c( "Index_tl", "Index_ctl", "Index_cyl" ) ) {
        		Index_ctl = log_Index_ctl = array( NA, dim = c( unlist( TmbData[c( "n_c", "n_t", "n_l" )] ), 2 ), 
				dimnames = list( category_names, year_labels, strata_names, c( "Estimate", "Std. Error" ) ) )
        		if ( use_biascorr == TRUE && "unbiased" %in% names( Sdreport ) ) {
           	 		Index_ctl[] = SD[which( rownames( SD ) == ParName ), c( "Est. (bias.correct)", "Std. Error" )]
        		}
        		if ( !any( is.na( Index_ctl ) ) ) {
            		message( "Using bias-corrected estimates for abundance index (natural-scale)..." )
        		} else {
            		message( "Not using bias-corrected estimates for abundance index (natural-scale)..." )
            		Index_ctl[] = SD[which(rownames( SD ) == ParName ), c( "Estimate", "Std. Error" )]
        		}
        		if ( use_biascorr == TRUE && "unbiased" %in% names( Sdreport ) ) {
            		log_Index_ctl[] = SD[which( rownames( SD ) == paste0( "ln_", ParName ) ), 
					c( "Est. (bias.correct)", "Std. Error" )]
        		}
        		if ( !any( is.na( log_Index_ctl ) ) ) {
            		message( "Using bias-corrected estimates for abundance index (log-scale)..." )
        		} else {
            		message( "Not using bias-corrected estimates for abundance index (log-scale)..." )
            		log_Index_ctl[] = SD[which( rownames( SD ) == paste0( "ln_", ParName ) ), 
					c( "Estimate", "Std. Error" )]
        		}
		}
		if ( ParName %in% c( "Index_tp" ) ) {
        		if ( use_biascorr == TRUE && "unbiased" %in% names( Sdreport ) ) {
            		Index_ctl = aperm( array( c( Sdreport$unbiased$value[which( names( Sdreport$value ) == ParName )], 
					TMB::summary.sdreport( Sdreport )[which( rownames( 
					TMB::summary.sdreport( Sdreport )) == ParName ), "Std. Error"] ), 
					dim = c( unlist( TmbData[c( "n_t", "n_c", "n_l" )] ), 2 ), 
					dimnames = list( NULL, NULL, NULL, c( "Estimate", "Std. Error" ) ) ), perm = c( 2, 1, 3 ) )
            		if ( "ln_Index_tp" %in% rownames( TMB::summary.sdreport( Sdreport ) ) ) {
                			log_Index_ctl = aperm( array( c( Sdreport$unbiased$value[which( names( Sdreport$value ) == 
                  			paste0( "ln_", ParName ) )], TMB::summary.sdreport( Sdreport )[which( 
						rownames( TMB::summary.sdreport( Sdreport ) ) == paste0( 
						"ln_", ParName ) ), "Std. Error"] ), 
                  			dim = c( unlist( TmbData[c( "n_t", "n_c", "n_l" )] ), 2 ), 
						dimnames = list( NULL, NULL, NULL, c( "Estimate", "Std. Error" ) ) ), 
						perm = c( 2, 1, 3 ) )
            		} else {
                			log_Index_ctl = log( Index_ctl )
                			log_Index_ctl[, , , "Std. Error"] = log_Index_ctl[, , , "Std. Error"] / 
						log_Index_ctl[, , , "Estimate"]
                			warning( paste0( "Using kludge for log-standard errors of index, ", 
						"to be replaced in later versions of 'MIST'" ) )
            		}
        		} else {
            		Index_ctl = aperm( array( TMB::summary.sdreport( Sdreport )[which( rownames( 
					TMB::summary.sdreport( Sdreport ) ) == ParName ), ], 
					dim = c( unlist( TmbData[c( "n_t", "n_c", "n_l" )] ), 2 ), 
					dimnames = list( NULL, NULL, NULL, c( "Estimate", "Std. Error" ) ) ), perm = c( 2, 1, 3, 4 ) )
            		if ( "ln_Index_tp" %in% rownames( TMB::summary.sdreport( Sdreport ) ) ) {
                			log_Index_ctl = aperm( array( TMB::summary.sdreport( Sdreport )[which( 
						rownames( TMB::summary.sdreport( Sdreport ) ) == paste0( "ln_", ParName ) ), ], 
						dim = c( unlist( TmbData[c( "n_t", "n_c", "n_l" )] ), 2 ), 
						dimnames = list( NULL, NULL, NULL, c( "Estimate", "Std. Error" ) ) ), 
						perm = c( 2, 1, 3, 4 ) )
            		} else {
                			log_Index_ctl = log( Index_ctl )
                			log_Index_ctl[, , , "Std. Error"] = log_Index_ctl[, , , "Std. Error"] / 
						log_Index_ctl[, , , "Estimate"]
                			warning( paste0( "Using kludge for log-standard errors of index, ", 
						"to be replaced in later versions of 'MIST'" ) )
            		}
        		}
		}
    		units( Index_ctl ) = units( log_Index_ctl ) = units( TmbData$b_i / TmbData$a_i * extrapolation_list$Area_km2[1] )
    		if ( "Bratio_cyl" %in% rownames( TMB::summary.sdreport( Sdreport ) ) ) {
        		Bratio_ctl = array( NA, dim = c( unlist( TmbData[c( "n_c", "n_t", "n_l" )] ), 2 ), 
				dimnames = list( category_names, year_labels, strata_names, c( "Estimate", "Std. Error" ) ) )
        		if ( use_biascorr == TRUE && "unbiased" %in% names( Sdreport ) ) {
            		Bratio_ctl[] = SD[which( rownames( SD ) == "Bratio_cyl" ), c( "Est. ( bias.correct )", "Std. Error" )]
        		}
        		if ( !any( is.na( Bratio_ctl ) ) ) {
            		message( "Using bias-corrected estimates for biomass ratio (natural-scale)..." )
        		} else {
            		message( "Not using bias-corrected estimates for biomass ratio (natural-scale)..." )
            		Bratio_ctl[] = SD[which( rownames( SD ) == "Bratio_cyl" ), c( "Estimate", "Std. Error" )]
        		}
        		units( Bratio_ctl ) = unitless
    		} else {
        		Bratio_ctl = NULL
		}
    		if ( "ln_Bratio_cyl" %in% rownames( TMB::summary.sdreport( Sdreport ) ) ) {
        		log_Bratio_ctl = array( NA, dim = c( unlist( TmbData[c( "n_c", "n_t", "n_l" )] ), 2 ), 
				dimnames = list( category_names, year_labels, strata_names, c( "Estimate", "Std. Error" ) ) )
        		if ( use_biascorr == TRUE && "unbiased" %in% names( Sdreport ) ) {
            		log_Bratio_ctl[] = SD[which( rownames( SD ) == "ln_Bratio_cyl" ), 
					c( "Est. (bias.correct)", "Std. Error" )]
        		}
        		if ( !any( is.na( log_Bratio_ctl ) ) ) {
            		message( "Using bias-corrected estimates for biomass ratio (log-scale)..." )
        		} else {
            		message( "Not using bias-corrected estimates for biomass ratio (log-scale)..." )
            		log_Bratio_ctl[] = SD[which( rownames( SD ) == "ln_Bratio_cyl" ), c( "Estimate", "Std. Error" )]
        		}
		} else {
        		log_Bratio_ctl = NULL
    		}
    		if ( "Fratio_ct" %in% rownames( TMB::summary.sdreport( Sdreport ) ) ) {
        		Fratio_ct = array( NA, dim = c( unlist( TmbData[c( "n_c", "n_t" )] ), 2 ), 
				dimnames = list( category_names, year_labels, c( "Estimate", "Std. Error" ) ) )
        		Fratio_ct[] = SD[which( rownames( SD ) == "Fratio_ct" ), c( "Estimate", "Std. Error" )]
    		} else {
        		Fratio_ct = NULL
    		}
    		if ( treat_missing_as_zero == TRUE ) {
        		Num_ctl = abind::adrop( TmbData$Options_list$metadata_ctz[, , "num_notna", drop = FALSE], drop = 3 ) %o% 
            		rep( 1, TmbData$n_l )
        		Index_ctl[, , , "Estimate"] = ifelse( Num_ctl == 0, 0, Index_ctl[, , , "Estimate"] )
        		Index_ctl[, , , "Std. Error"] = ifelse( Num_ctl == 0, NA, Index_ctl[, , , "Std. Error"] )
        		log_Index_ctl[, , , "Estimate"] = ifelse( Num_ctl == 0, -Inf, log_Index_ctl[, , , "Estimate"] )
        		log_Index_ctl[, , , "Std. Error"] = ifelse( Num_ctl == 0, NA, log_Index_ctl[, , , "Std. Error"] )
    		}
    		Plot_suffix = ""
    		if ( !is.null( Bratio_ctl ) ) 
        		Plot_suffix = c( Plot_suffix, "-Bratio" )
    		for ( plotI in 1 : length( Plot_suffix ) ) {
        		if ( Plot_suffix[plotI] == "" ) {
           		Array_ctl = Index_ctl
           		log_Array_ctl = log_Index_ctl
      	}
        	if ( Plot_suffix[plotI] == "-Bratio" ) {
            	Array_ctl = Bratio_ctl
           		log_Array_ctl = log_Bratio_ctl
        	}
        	plot_index( Index_ctl = array( Index_ctl[, , , "Estimate"], dim( Index_ctl )[1 : 3] ), 
			sd_Index_ctl = array( log_Index_ctl[, , , "Std. Error"], dim( log_Index_ctl )[1 : 3] ), 
            	year_labels = year_labels, years_to_plot = years_to_plot, 
            	strata_names = strata_names, category_names = category_names, 
           		DirName = DirName, PlotName = paste0( PlotName, Plot_suffix[plotI], ".png" ), 
			interval_width = interval_width, width = width, height = height, xlab = "Year", 
           		ylab = make_unit_label( u = units( Array_ctl ), lab = "Index", parse = TRUE ), 
			scale = "log", plot_args = list( log = ifelse( plot_log == TRUE, "y", "" ) ), Yrange = Yrange )
    		}
    		if ( !is.null( Fratio_ct ) ) {
        		Array_ct = Fratio_ct
        		Array_ct[, , "Estimate"] = ifelse( Array_ct[, , "Estimate"] == 0, NA, Array_ct[, , "Estimate"] )
        		plot_index( Index_ctl = array( Array_ct[, , "Estimate"], dim( Array_ct )[1 : 2] ), 
				sd_Index_ctl = array( Array_ct[, , "Std. Error"], dim( Array_ct )[1 : 2] ), 
				year_labels = year_labels, years_to_plot = years_to_plot, strata_names = strata_names, 
            		category_names = category_names, DirName = DirName, 
            		PlotName = paste0( PlotName, "-Fratio.png" ), 
            		scale = "uniform", interval_width = interval_width, 
           	 		width = width, height = height, xlab = "Year", ylab = "Fishing ratio" )
    		}
    		if ( !is.null( Bratio_ctl ) & !is.null( Fratio_ct ) ) {
        		Par = list( mar = c( 2, 2, 1, 0 ), mgp = c( 2, 0.5, 0 ), tck = -0.02, 
           			yaxs = "i", oma = c( 1, 2, 0, 0 ), mfrow = mfrow, ... )
        		Col = colorRampPalette( colors = c( "blue", "purple", "red" ) )
        		png( file = paste0( DirName, "/", PlotName, "-Status.png" ), 
           			width = width, height = height, res = 200, units = "in" )
        		par( Par )
       		Array1_ct = Bratio_ctl[, , 1, ]
        		Array1_ct = ifelse( Array1_ct == 0, NA, Array1_ct )
        		Array2_ct = Fratio_ct
        		Array2_ct = ifelse( Array2_ct == 0, NA, Array2_ct )
        		for ( cI in 1 : TmbData$n_c ) {
            		Xlim = c( 0, max( 1, Array1_ct[cI, years_to_plot, "Estimate"] %o% 
                			c( 1, 1 ) + Array1_ct[cI, years_to_plot, "Std. Error"] %o% 
                			c( -interval_width, interval_width ), na.rm = TRUE ) )
            		Ylim = c( 0, max( 2, Array2_ct[cI, years_to_plot, "Estimate"] %o% 
                			c( 1, 1 ) + Array2_ct[cI, years_to_plot, "Std. Error"] %o% 
                			c( -interval_width, interval_width ), na.rm = TRUE ) )
            		plot( 1, type = "n", xlim = Xlim, ylim = Ylim, xlab = "", ylab = "", 
					main = ifelse( TmbData$n_c > 1, category_names[cI], "" ) )
            		points( x = Array1_ct[cI, years_to_plot, "Estimate"], 
                			y = Array2_ct[cI, years_to_plot, "Estimate"], 
                			col = Col( length( year_labels ) )[years_to_plot] )
            		for ( tI in years_to_plot ) {
               			lines( x = rep( Array1_ct[cI, tI, "Estimate"], 2 ), 
						y = Array2_ct[cI, tI, "Estimate"] + 
                  			Array2_ct[cI, tI, "Std. Error"] * c( -interval_width, interval_width ), 
						col = Col( length( year_labels ) )[tI] )
                			lines( x = Array1_ct[cI, tI, "Estimate"] + 
                  			Array1_ct[cI, tI, "Std. Error"] * c( -interval_width, interval_width ), 
						y = rep( Array2_ct[cI, tI, "Estimate"], 2 ), 
						col = Col( length( year_labels ) )[tI] )
           			}
           			abline( v = 0.4, lty = "dotted" )
            		abline( h = 1, lty = "dotted" )
        		}
      		legend( "topright", bty = "n", fill = c( Col( length( year_labels ) )[years_to_plot[1]], 
           			Col( length( year_labels ) )[rev( years_to_plot )[1]] ), 
           			legend = c( year_labels[years_to_plot[1]], year_labels[rev( years_to_plot )[1]] ) )
        		mtext( side = 1 : 2, text = c( "Biomass relative to unfished", "Fishing relative to F_40%" ), 
				outer = TRUE, line = c( 0, 0 ) )
        		dev.off()
   		}
    		Table = NULL
    		for ( cI in 1 : TmbData$n_c ) {
        		index_units = make_unit_label( u = units( Index_ctl ), lab = "", parse = FALSE )
       		Tmp = data.frame( Year = year_labels, Unit = index_units, 
           			Estimate = as.vector( Index_ctl[cI, , , "Estimate"] ), 
           			SD_log = as.vector( log_Index_ctl[cI, , , "Std. Error"] ), 
           			SD_mt = as.vector( Index_ctl[cI, , , "Std. Error"] ) )
        		if ( TmbData$n_c > 1 ) 
           			Tmp = cbind( Category = category_names[cI], Tmp )
        		Table = rbind( Table, Tmp )
   		}
    		write.csv( Table, file = paste0( DirName, "/Index.csv" ), row.names = FALSE )
    		Return = list( Table = Table, log_Index_ctl = log_Index_ctl, Index_ctl = Index_ctl )
    		if ( "cov" %in% names( Sdreport ) & create_covariance_table == TRUE ) {
       		DF = expand.grid( Category = 1 : TmbData$n_c, Year = 1 : TmbData$n_t, Stratum = 1 : TmbData$n_l )
        		Which = which(names( Sdreport$value ) == ParName )
        		Cov = Sdreport$cov[Which, Which]
       		Corr = cov2cor( Cov ) - diag( nrow( Cov ) )
        		rowcolDF = cbind( RowNum = row( Corr )[lower.tri( Corr, diag = TRUE )], 
            		ColNum = col( Corr )[lower.tri( Corr, diag = TRUE )] )
        		Table = cbind( DF[rowcolDF[, "ColNum"], ], DF[rowcolDF[, "RowNum"], ] )
      			colnames( Table ) = paste0( colnames( Table ), rep( c( 1, 2 ), each = 3 ) )
        		Table = cbind( Table, Correlation = cov2cor( Cov )[lower.tri( Corr, diag = TRUE )], 
				Covariance = Cov[lower.tri( Corr, diag = TRUE )] )
        		Table = cbind( Table, Index1 = Index_ctl[as.matrix( cbind( DF[rowcolDF[, "ColNum"], ], 1 ) )], 
				Index2 = Index_ctl[as.matrix( cbind( DF[rowcolDF[, "RowNum"], ], 1 ) )] )
        		WhichZero = which( ( Table[, "Index1"] * Table[, "Index2"] ) == 0 )
        		Table[WhichZero, c( "Correlation", "Covariance" )] = 0
        		Return = c( Return, Table_of_estimted_covariance = Table )
    		}
    		if ( !is.null( Bratio_ctl ) ) 
        		Return = c(Return, list(Bratio_ctl = Bratio_ctl))
    		if ( !is.null( log_Bratio_ctl ) ) 
        		Return = c( Return, list( log_Bratio_ctl = log_Bratio_ctl ) )
    		if ( !is.null( Fratio_ct ) ) 
        		Return = c( Return, list( Fratio_ct = Fratio_ct ) )
		return( invisible( Return ) )

}
