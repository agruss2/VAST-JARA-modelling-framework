###############################################################################################################################
##
##  (00) Initialisation
##
###############################################################################################################################

######## Define some functions 
#### Define the "make.filename" function 
make.filename <- function ( file = "", path = "", add.terminal = F ) {

	if ( path != "" ) {
		plc <- substring( path, nchar( path ) )
    		if ( ! ( plc == "\\" | plc == "/" ) ) path <- paste( path, "\\", sep = "" )
  	}
  	filename <- paste( path, file, sep = "" )
	if ( add.terminal == T ) {
    		plc <- substring( filename, nchar( filename ) )
    		if ( !( plc == "\\" | plc == "/" ) ) filename <- paste( filename, "\\", sep = "" )
  	}
  	return( filename )

}

#### Define the "assign.directories" function 
assign.directories <- function ( base = "", subdir = "" ) {

	DIR <- list()

  	## Base directory
  	DIR[["Base"]] <- base

  	## Subdirectory
  	DIR[["Subdir"]] <- make.filename( subdir, DIR[["Base"]], T )
  	
	## R directory
  	DIR[["R"]] <- make.filename( "R", DIR[["Subdir"]], T )
  	
	## R functions
  	DIR[["Functions"]] <- make.filename( "Functions", DIR[["R"]], T )
  		 
	## Input
  	DIR[["Input"]] <- make.filename( "Input", DIR[["Base"]], T )  
		
	## Output
  	DIR[["Output"]] <- make.filename( "Output", DIR[["Base"]], T )  
	
	## Figures 
  	DIR[["Figures"]] <- make.filename( "Figures", DIR[["Base"]], T ) 
	
	## Maps
  	DIR[["Maps"]] <- make.filename( "Maps", DIR[["Base"]], T ) 

	return( DIR )

}

######## Find a specific user for setup 
if ( Sys.info()[["user"]] == "arnau" ) {
	DIR <- assign.directories( base = "D:\\VAST_JARA_project\\" )
} else if ( Sys.info()[["user"]] == "Other_user" ) {
	DIR <- assign.directories( base = "C:\\My_directory\\" )
} else {
	DIR <- ""
}

