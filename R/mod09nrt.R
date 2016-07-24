require(sfsmisc)
require(raster)	
#' Convert to data.frame, without factors
#' 
#' Shortcut for: \code{as.data.frame(x, row.names=NULL, stringsAsFactors=FALSE)}
#' 
#' This function, and \code{\link{adf2}}, are useful for dealing with errors due to 
#' automatic conversion of some columns to factors.  Another solution may be to prepend
#' \code{options(stringsAsFactors = FALSE)} at the start of one's script, to turn off all default stringsAsFactors silliness.
#' 
#' @param x matrix or other object transformable to data.frame
#' @return data.frame
#' @export
#' @seealso \code{\link{adf2}}
#' @author Nicholas J. Matzke \email{matzke@@berkeley.edu}
#' @examples
#' x = matrix(c(1,2,3,4,5,6), nrow=3, ncol=2)
#' adf(x)
adf <- function(x)
	{
	return(as.data.frame(x, row.names=NULL, stringsAsFactors=FALSE))
	}

#' Convert to data.frame, without factors
#' 
#' Shortcut for: \code{tmp_rownames = 1:nrow(x); as.data.frame(x, row.names=tmp_rownames, stringsAsFactors=FALSE)}
#' 
#' This function, and \code{\link{adf2}}, are useful for dealing with errors due to 
#' automatic conversion of some columns to factors.  Another solution may be to prepend
#' \code{options(stringsAsFactors = FALSE)} at the start of one's script, to turn off all default stringsAsFactors silliness.
#'
#' In adf2, rownames are forced to be numbers; this can prevent errors due to e.g. repeated rownames
#' after an \code{rbind} operation.
#'
#' @param x matrix or other object transformable to data.frame
#' @return data.frame
#' @export
#' @seealso \code{\link{adf}}
#' @author Nicholas J. Matzke \email{matzke@@berkeley.edu}
#' @examples
#' x = matrix(c(1,2,3,4,5,6), nrow=3, ncol=2)
#' adf2(x)
adf2 <- function(x)
	{
	# Deals with the problem of repeated row names
	rownames = 1:nrow(x)
	return(as.data.frame(x, row.names=rownames, stringsAsFactors=FALSE))
	}


#######################################################
# slashslash:
#######################################################
#' Remove double slash (slash a slash)
#' 
#' Shortcut for: \code{gsub(pattern="//", replacement="/", x=tmpstr)}
#' 
#' This function is useful for removing double slashes that can
#' appear in full pathnames due to inconsistencies in trailing
#' slashes in working directories etc.
#'
#' @param tmpstr a path that you want to remove double slashes from
#' @return outstr a string of the fixed path
#' @export
#' @seealso \code{\link{gsub}}
#' @author Nicholas J. Matzke \email{matzke@@berkeley.edu}
#' @examples
#' tmpstr = "/Library/Frameworks/R.framework/Versions/2.15/Resources/library/modiscloud/extdata/2002raw//MYD03.A2002185.0645.005.2009192031332.hdf"
#'
#' outstr = slashslash(tmpstr)
#' outstr
#'
slashslash <- function(tmpstr)
	{
	outstr = gsub(pattern="//", replacement="/", x=tmpstr)
	return(outstr)
	}
#######################################################
# extract_fn_from_path:
#######################################################
#' Get the filename from a path
#'
#' The filename is split on slashes, and the last item is taken; this should be just
#' the filename.
#' 
#' @param fn_with_path The filename, with partial or full path
#' @return \code{fn} The extracted filename
#' @export
#' @seealso \code{\link[base]{strsplit}}
#' @author Nicholas J. Matzke \email{matzke@@berkeley.edu}
#' @examples
#' fn_with_path = "/Library/Frameworks/R.framework/Versions/2.15/Resources/library/modiscloud/extdata/2002raw/MYD35_L2.A2002185.1910.005.2007206043609.hdf"
#' extract_fn_from_path(fn_with_path)
#'
extract_fn_from_path <- function(fn_with_path)
	{
	words = strsplit(fn_with_path, split="/")[[1]]
	fn = words[length(words)]
	return(fn)
	}

#######################################################
# check_for_matching_geolocation_files_mod09nrt:
#######################################################
#' Checks that every MODIS surface reflectance project HDF has a matching MOD03 file
#' 
#' Each MOD09 surface reflectance product file requires a corresponding
#' MOD03 geolocation file to be successfully processed with the MRTSwath tool.
#'
#' MRTSwath is the MRT (MODIS Reprojection Tool) for the MODIS
#'
#' E.g. this surface reflectance file:
#'
#' MOD09.A2016204.0500.005.2016204062219.NRT.hdf	
#'
#' ...goes with this corresponding geolocation file:
#'
#' MOD03.A2016204.0500.005.2016204060250.NRT.hdf
#'
#' ...which is a large file (~30 MB) containing detailed information
#' on the position, tilt, etc. of the MODIS satellite.
#' MRTSwath tool needs one of each, however.
#'
#' @param moddir the string describing the directory containing the MOD09 and MOD03 files; both must be in the same directory.  Default: getwd(), which gives the present working directory.
#' @param modtxt the text string indicating which HDF files are the MODIS surface reflectance product (or hypothetically, other product). Default: MOD09 (MODIS surface reflectance product)
#' @param geoloctxt the text string indicating which HDF files are the MODIS geolocation files (or hypothetically, another set of files). Default: MOD03
#' @param return_geoloc if TRUE, return the list of unmatched geolocation files (e.g. MOD03 )
#' @param return_product if TRUE, return the list of unmatched product files (e.g. MOD09)
#' @return data.frame of matching files; or a list of non-matching files, if \code{return_geoloc} or \code{return_product} are TRUE.
#' @export
#' @seealso \code{\link{extract_time_from_MODISfn}}

#' @author Rishabh Gupta \email{rishabh.uk@gmail.com}
#' @examples
#' # Check your working directory
#' moddir = getwd()
#' 
#' # Here are some example MODIS files in mod09nrt/extdata/
#' # Code excluded from CRAN check because it depends on modiscdata
#' \dontrun{
#' library(devtools)
#' library(modiscdata)
#' moddir = system.file("extdata/2002raw/", package="modiscdata")
#'
#' # You need to have some e.g. MOD files in it (from the MODIS-TERRA platform)
#' list.files(path=moddir, pattern="MOD")
#' list.files(path=moddir, pattern="MOD")
#'
#' # Check for matches (for MODIS-TERRA platform)
#' check_for_matching_geolocation_files_mod09nrt(moddir=moddir, modtxt="MOD09", geoloctxt="MOD03", return_geoloc=FALSE, return_product=FALSE)
#' }
#'
check_for_matching_geolocation_files_mod09nrt <- function(moddir=getwd(), modtxt="MOD09", geoloctxt="MOD03", return_geoloc=FALSE, return_product=FALSE)
	{

	# Get fns with suffix, from either directory or fns list
	# Do NOT use dot in the suffix
	get_fns_matching_txt <- function(tmpdir=NULL, fns=NULL, text_to_match = NULL, returnfullnames=TRUE)
		{
		# If tmpdir is NOT null, get those files from list.files.
		if (is.null(tmpdir) == FALSE)
			{
			fns = list.files(tmpdir, full.names=returnfullnames)
			fns_without_paths = list.files(tmpdir, full.names=FALSE)
			}
		
		# Return true/false for matched text
		TF = grepl(pattern=text_to_match, x=fns_without_paths)
		
		matching_fns = fns[TF]
		return(matching_fns)
		}



	mod03_fns = sort(slashslash(get_fns_matching_txt(moddir, text_to_match=geoloctxt)))
	mod09_fns = sort(slashslash(get_fns_matching_txt(moddir, text_to_match=modtxt)))
	#head(mod03_fns)
	#head(mod09_fns)

	# Check if you have the right # of files
	if (length(mod03_fns) == length(mod09_fns))
		{
		cat("\nProcessing ", length(mod03_fns), " files.\n", sep="")
		
		# Return the list, sorted
		fns_df = cbind(mod09_fns, mod03_fns)
		fns_df = adf(fns_df)
		
		return(fns_df)
		
		} else {
		cat("\nWARNING: You have ", length(mod09_fns), " ", modtxt, " files & ", length(mod03_fns), " ", geoloctxt, " files.\nWill attempt to find just the matching files", sep="")
		}
	
	
	# Get the datestring for each MOD03 file
	mod03_idstrings = rep(NA, times = length(mod03_fns))
	for (i in 1:length(mod03_fns))
		{
		words = strsplit(x=mod03_fns[i], split="\\.")[[1]]
		idnums = words[2:4]
		mod03_idstrings[i] = paste(idnums, sep="", collapse=".")
		
		}
	#mod03_idstrings

	# Get the datestring for each mod09 file
	mod09_idstrings = rep(NA, times = length(mod09_fns))
	for (i in 1:length(mod09_fns))
		{
		words = strsplit(x=mod09_fns[i], split="\\.")[[1]]
		idnums = words[2:4]
		mod09_idstrings[i] = paste(idnums, sep="", collapse=".")
		}
	#mod09_idstrings
	
	
	# Find which match
	mod09_in_mod03_TF = mod09_idstrings %in% mod03_idstrings
	mod09_fns_dropped = mod09_fns[mod09_in_mod03_TF == FALSE]
	mod09_fns = mod09_fns[mod09_in_mod03_TF == TRUE]
	mod09_idstrings = mod09_idstrings[mod09_in_mod03_TF == TRUE]
	
	mod03_in_mod09_TF = mod03_idstrings %in% mod09_idstrings
	mod03_fns_dropped = mod03_fns[mod03_in_mod09_TF == FALSE]
	mod03_fns = mod03_fns[mod03_in_mod09_TF == TRUE]
	mod03_idstrings = mod03_idstrings[mod03_in_mod09_TF == TRUE]
	
	
	# Correct // to / (if any)
	# Could also use slashslash
	mod03_fns = gsub(pattern="//", replacement="/", x=mod03_fns)
	mod09_fns = gsub(pattern="//", replacement="/", x=mod09_fns)
	mod09_fns_dropped = gsub(pattern="//", replacement="/", x=mod09_fns_dropped)
	mod03_fns_dropped = gsub(pattern="//", replacement="/", x=mod03_fns_dropped)
	
	# Check lengths (manual)
	length(mod09_fns)
	length(mod03_fns)	
	length(mod09_idstrings)
	length(mod03_idstrings)
	sum(mod09_idstrings == mod03_idstrings)
	
	# Return the list or matching files, sorted
	fns_df = cbind(mod09_fns, mod03_fns)
	fns_df = adf(fns_df)
	
	# Print the dropped files
	cat("\n", sep="")
	cat("\nWarning: ", length(mod09_fns_dropped), " ", modtxt, " files dropped with no matches:\n", sep="")
	cat(head(mod09_fns_dropped), sep="\n")
	cat("...", sep="")
	cat("\n", sep="")

	cat("\nWarning: ", length(mod03_fns_dropped), " ", geoloctxt, " files dropped with no matches:\n", sep="")
	cat(head(mod03_fns_dropped), sep="\n")
	cat("...", sep="")
	cat("\n", sep="")
	
	# Return unmatched geolocation files, if desired
	if (return_geoloc == TRUE)
		{
		return(mod03_fns_dropped)
		}

	# Return unmatched product files, if desired
	if (return_product == TRUE)
		{
		return(mod09_fns_dropped)
		}
	
	# Otherwise, return the matched files...
	return(fns_df)	
	}

	#######################################################
# write_MRTSwath_param_file_mod09nrt:
#######################################################
#' Write a parameter control file for MRTSwath
#'
#' MRTSwath is the "MODIS Reprojection Tool for swath products".  See:
#' \url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool_swath}).
#' 
#' If you want this function to use MRTSwath tool successfully, you should 
#' add the directory with the MRTSwath executable to the default R PATH
#' by editing \code{~/.Rprofile}.
#'
#' This function hard-codes these options into the parameter file:\cr
#' * all the bands are extracted\cr
#' * the output file is a GeoTIFF\cr
#' * the output projection is Geographic (plain unprojected Latitude/Longitude)\cr
#' * the resampling is Nearest Neighbor (NN), which of course is the only one which makes sense when the pixels encode bytes that encode bits that encode discrete classification results, 0/1 error flags, etc.\cr
#'
#' MRTswath can do many other projections and output formats; users can modify this function to run those options.
#'
#' @param prmfn The name of the parameter/control file which will be the input to MRTSwath's \code{swath2grid} function.
#' @param tifsdir The directory to save the output TIF files in
#' @param modfn The filename of the MODIS data
#' @param geoloc_fn The filename of the corresponding geolocation file (annoyingly, this is a much larger
#' file than the data file!)
#' @param ul_lon Upper left (ul) longitude (x-coordinate) for subsetting
#' @param ul_lat Upper left (ul) latitude (y-coordinate) for subsetting
#' @param lr_lon Lower right (lr) longitude (x-coordinate) for subsetting
#' @param lr_lat Lower right (lr) latitude (y-coordinate) for subsetting
#' @return \code{prmfn} The name of the temporary parameter file
#' @export
#' @seealso \code{\link{run_swath2grid_mod09nrt}}
#' @seealso \url{http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=hdf_filename}
#'   @cite NASA2001
#' @author Rishabh Gupta \email{rishabh.uk@gmail.com}
#' @examples
#'
#' # Source MODIS files (both data and geolocation)
#' # Code excluded from CRAN check because it depends on modiscdata
#' \dontrun{
#' library(devtools)
#' library(modiscdata)
#' moddir = system.file("extdata/2002raw/", package="modiscdata")
#' 
#' # Get the matching data/geolocation file pairs
#' fns_df = check_for_matching_geolocation_files_mod09nrt(moddir, modtxt="MOD09", geoloctxt="MOD03")
#' fns_df
#' 
#' # Resulting TIF files go in this directory
#' tifsdir = getwd()
#' 
#' 
#' # Box to subset
#' ul_lat = 13
#' ul_lon = -87
#' lr_lat = 8
#' lr_lon = -82
#' 
#' for (i in 1:nrow(fns_df))
#' 	{
#' 	
#' 	prmfn = write_MRTSwath_param_file_mod09nrt(prmfn="tmpMRTparams.prm", tifsdir=tifsdir, modfn=fns_df$mod09_fns[i], geoloc_fn=fns_df$mod03_fns[i], ul_lon=ul_lon, ul_lat=ul_lat, lr_lon=lr_lon, lr_lat=lr_lat)
#' 	print(scan(file=prmfn, what="character", sep="\n"))
#' 	
#' 	}
#' }
#' 
write_MRTSwath_param_file_mod09nrt <- function(prmfn="tmpMRTparams.prm", tifsdir, modfn, geoloc_fn, ul_lon, ul_lat, lr_lon, lr_lat)
{
  # Initialize the list of lines in the parameter file
  prmfile = NULL
  pnum = 0
  prmfile[[(pnum=pnum+1)]] = 	" "
  
  # Input files
  prmfile[[(pnum=pnum+1)]] = 	paste("INPUT_FILENAME = ", modfn, sep="")
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	paste("GEOLOCATION_FILENAME = ", geoloc_fn, sep="")
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	paste("INPUT_SDS_NAME = 1km_Atmospheric_Optical_Depth_Band_1; 1km_Atmospheric_Optical_Depth_Band_3; 1km_Atmospheric_Optical_Depth_Band_8; 1km_Atmospheric_Optical_Depth_Model; 1km_Atmospheric_Optical_Depth_Band_QA; 1km_Atmospheric_Optical_Depth_Band_CM; 250m_Surface_Reflectance_Band_1; 250m_Surface_Reflectance_Band_2; 500m_Surface_Reflectance_Band_1; 500m_Surface_Reflectance_Band_2; 500m_Surface_Reflectance_Band_3; 500m_Surface_Reflectance_Band_4; 500m_Surface_Reflectance_Band_5; 500m_Surface_Reflectance_Band_6; 500m_Surface_Reflectance_Band_7; 1km_Surface_Reflectance_Band_1; 1km_Surface_Reflectance_Band_2; 1km_Surface_Reflectance_Band_3; 1km_Surface_Reflectance_Band_4; 1km_Surface_Reflectance_Band_5; 1km_Surface_Reflectance_Band_6; 1km_Surface_Reflectance_Band_7; 1km_Surface_Reflectance_Band_8; 1km_Surface_Reflectance_Band_9; 1km_Surface_Reflectance_Band_10; 1km_Surface_Reflectance_Band_11; 1km_Surface_Reflectance_Band_12; 1km_Surface_Reflectance_Band_13; 1km_Surface_Reflectance_Band_14; 1km_Surface_Reflectance_Band_15; 1km_Surface_Reflectance_Band_16; BAND20; 1km_Surface_Reflectance_Band_26; BAND31; BAND32; BAND20ALBEDO; 250m_Reflectance_Band_Quality; 500m_Reflectance_Band_Quality; 1km_Reflectance_Band_Quality; 1km_Reflectance_Data_State_QA; 1km_Band_3_Path_Radiance", sep="")
  
  # Subset parameters
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_SPATIAL_SUBSET_TYPE = LAT_LONG", sep="")
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) =", ul_lon, ul_lat, sep=" ")
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_SPACE_LOWER_RIGHT_CORNER (LONG LAT) = ", lr_lon, lr_lat, sep=" ")
  
  
  # Output filename
  prmfile[[(pnum=pnum+1)]] = 	" "
  
  outfn = gsub(pattern=".hdf", replacement=".tif", modfn)
  outfn = extract_fn_from_path(fn_with_path=outfn)
  outfn = slashslash(paste(tifsdir, outfn, sep="/"))
  
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_FILENAME = ", outfn, sep="")
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_FILE_FORMAT = GEOTIFF_FMT", sep="")
  
  # Reprojection information (for Geographic Projection, with nearest-neighbor resampling)
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	"KERNEL_TYPE (CC/BI/NN) = NN"
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	"OUTPUT_PROJECTION_NUMBER = GEO"
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	"OUTPUT_PROJECTION_PARAMETER = 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0"
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	"OUTPUT_PROJECTION_SPHERE = 8"
  prmfile[[(pnum=pnum+1)]] = 	" "
  
  #prmfile
  
  
  # Write the MRTSwath tool parameter file
  write.table(x=prmfile, file=prmfn, append=FALSE, quote=FALSE, sep="\n", row.names=FALSE, col.names=FALSE)
  #moref(prmfn)
  
  return(prmfn)
}
#######################################################
# run_swath2grid_mod09nrt:
#######################################################
#' Run MRTSwath swath2grid tool
#'
#' MRTSwath is the "MODIS Reprojection Tool for swath products".  See:
#' \url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool_swath}).
#' 
#' If you want this function to use MRTSwath tool successfully, you should 
#' add the directory with the MRTSwath executable to the default R PATH
#' by editing \code{~/.Rprofile}.
#'
#' @param mrtpath This is the path to the MRTSwath executable \code{swath2grid}. If your \code{~/.Rprofile}
#' file has the location of \code{swath2grid} in the PATH, then you can just use \code{mrtpath="swath2grid"}.
#' Otherwise, the user must provide the full path to swath2grid.
#' @param prmfn The name of the parameter/control file which will be the input to MRTSwath's \code{swath2grid} function.
#' @param tifsdir The directory to save the output TIF files in
#' @param modfn The filename of the MODIS data
#' @param geoloc_fn The filename of the corresponding geolocation file (annoyingly, this is a much larger
#' file than the data file!)
#' @param ul_lon Upper left (ul) longitude (x-coordinate) for subsetting
#' @param ul_lat Upper left (ul) latitude (y-coordinate) for subsetting
#' @param lr_lon Lower right (lr) longitude (x-coordinate) for subsetting
#' @param lr_lat Lower right (lr) latitude (y-coordinate) for subsetting
#' @return \code{cmdstr} The string giving the system command that ran \code{swath2grid}
#' @export
#' @seealso \code{\link{write_MRTSwath_param_file_mod09nrt}}
#' @seealso \url{http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=hdf_filename}
#'   @cite NASA2001
#' @author Nicholas J. Matzke \email{matzke@@berkeley.edu}
#' @examples
#' #######################################################
#' # Run MRTSwath tool "swath2grid"
#' #######################################################
#' 
#' # Source MODIS files (both data and geolocation)
#' # Code excluded from CRAN check because it depends on modiscdata
#' \dontrun{
#' library(devtools)
#' library(modiscdata)
#' moddir = system.file("extdata/2002raw/", package="modiscdata")
#' 
#' # Get the matching data/geolocation file pairs
#' fns_df = check_for_matching_geolocation_files(moddir, modtxt="MOD09", geoloctxt="MOD03")
#' fns_df
#' 
#' # Resulting TIF files go in this directory
#' tifsdir = getwd()
#' 
#' 
#' # Box to subset
#' ul_lat = 13
#' ul_lon = -87
#' lr_lat = 8
#' lr_lon = -82
#' 
#' for (i in 1:nrow(fns_df))
#' 	{
#' 	
#'	prmfn = write_MRTSwath_param_file_mod09nrt(prmfn="tmpMRTparams.prm", tifsdir=tifsdir, modfn=fns_df$mod09_fns[i], geoloc_fn=fns_df$mod03_fns[i], ul_lon=ul_lon, ul_lat=ul_lat, lr_lon=lr_lon, lr_lat=lr_lat)
#'	print(scan(file=prmfn, what="character", sep="\n"))
#' 	
#'	run_swath2grid_mod09nrt(mrtpath="swath2grid", prmfn="tmpMRTparams.prm", tifsdir=tifsdir, modfn=fns_df$mod309_fns[i], geoloc_fn=fns_df$mod03_fns[i], ul_lon=ul_lon, ul_lat=ul_lat, lr_lon=lr_lon, lr_lat=lr_lat)
#'	
#' 	}
#'
#' list.files(tifsdir, pattern=".tif", full.names=TRUE)
#' }
#'
run_swath2grid_mod09nrt <- function(mrtpath="swath2grid", prmfn="tmpMRTparams.prm", tifsdir, modfn, geoloc_fn, ul_lon, ul_lat, lr_lon, lr_lat)
	{
	
	# Write the temporary parameter file
	prmfn = write_MRTSwath_param_file_mod09nrt(prmfn=prmfn, tifsdir=tifsdir, modfn=modfn, geoloc_fn=geoloc_fn, ul_lon=ul_lon, ul_lat=ul_lat, lr_lon=lr_lon, lr_lat=lr_lat)
	
	# Run MRTSwath tool (swath2grid)
	cmdstr = paste(mrtpath, " -pf=", prmfn, sep="")
	system(cmdstr)
	
	return(cmdstr)
	}
