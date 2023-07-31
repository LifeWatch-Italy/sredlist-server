api_error <- function(message, status) {
  err <- structure(
    list(message = message, status = status),
    class = c("api_error", "error", "condition")
  )
  signalCondition(err)
}

error_handler <- function(req, res, err) {
  if (!inherits(err, "api_error")) {
    # Read Error Message in Response Object
    res$status <- 500
    messageBody <- paste0("Internal server error, please try again in a minute and report that error if it persists. \n", err$message) # nolint
    # Print the internal error so we can see it from the server side. A more
    # robust implementation would use proper logging.
    log_error(err$message)
    list(message = messageBody)
  } else {
    # We know that the message is intended to be user-facing.
    res$status <- err$status
    log_error('{req$PATH_INFO} : {err$message}') # nolint
    list(message = err$message)
  }
}

payload_too_large  <- function(message = "Payload Too Large") {
  api_error(message = message, status = 413)
}

not_found <- function() {
  api_error(message = "Shapefile of the species does not exist!", status = 404)
}

no_records <- function() {
  api_error(message = "No data found! Check whether the scientific name of the species has been typed correctly or select other data sources", status=400)
}

species_not_in_distrib <- function() {
  api_error(message = "The species is not present in the shapefile, there may be a mistake in column 'binomial'", status = 400)
}

bug_distribution_loading <- function() {
  api_error(message = "The distribution could not be loaded; check if it includes the 4 files required (.shp, .shx, .prj, .dbf)", status=400)
}

empty_distrib <- function() {
  api_error(message = "The distribution is empty", status = 400)
}

missing_params <- function(message = "Missing required parameters.") {
  api_error(message = message, status = 400)
}

invalid_params <- function(message = "Invalid parameter value(s).") {
  api_error(message = message, status = 400)
}

invalid_extension <- function(file) {
  message=paste0("The following file has an invalid format: ", file)
  api_error(message = message, status = 400)
}

no_habitat_pref <- function() {
  api_error(message = "At least one suitable habitat must be selected", status = 400)
}

no_habitats_crosswalk <- function() {
  api_error(message = "This habitat cannot be mapped on the platform (the crosswalk we use to link with land cover data does not include the habitats you selected)", status = 400)
}

no_gbif_data <- function() {
  api_error(message = "All records have been excluded in Step 2", status = 400)
}

wrong_csv_upload <- function() {
  api_error(message = "The csv uploaded is invalid. Check the separator you used.", status=400)
}

wrong_zip_extension<-function() {
  api_error(message = "Only provide files with extension .zip", status=400)
}

no_coords_update <- function() {
  api_error(message = "Longitude (dec_long) and latitude (dec_lat) columns were not found in uploaded data", status = 400)
}

wrong_density <- function() {
  api_error(message = "Density estimate is not valid. Should be a number or two numbers separated by a hyphen", status = 400)
}

wrong_percentages <- function() {
  api_error(message = "One of the percentage values is incorrect, they should always be between 0 and 100", status = 400)
}

incorrect_GL <- function() {
  api_error(message = "The Generation Length value entered is not valid", status = 400)
}

neg_kernel <- function() {
  api_error(message = "Kernel parameter should be positive", status = 400)
}

neg_alpha <- function() {
  api_error(message = "Alpha parameter should be positive", status = 400)
}

bug_alpha <- function() {
  api_error(message = "Alpha hull could not be calculated, try with a higher alpha parameter value or use another Starting point", status = 400)
}

hydro_too_large <- function() {
  api_error(message = "The distribution of that species is too large to use hydrobasins level 10 or 12, please use level 8 instead", status = 400)
}

no_hydrobasins <-  function() {
  api_error(message = "The distribution of that species does not overlap with hydrobasins map", status = 400)
}

too_few_occurrences <- function() {
  api_error(message = "There are not enough occurrence records to create the distribution with mcp, alpha hull, or kernel. Please add some occurrence records or choose another starting point", status = 400)
}

coords_outofbound <- function() {
  api_error(message = "Coordinates should be in decimal with longitude (dec_long) between -180 and 180 and latitude (dec_lat) between -90 and 90. Make sure you used dots and not commas to write decimals in your csv file, this may cause issues.", status = 400)
}

wrong_species_upload <- function() {
  api_error(message = "At least one of the uploaded observations has a different species name (sci_name) than the name of the species you are assessing. Please edit your csv file before uploading again", status = 400)
}

no_storage <- function() {
  api_error(message = "The session was inactive for more than 60 minutes so all files have been deleted... We are sorry but you have to start again from the beginning", status=400)
}

elev_not_valid <- function(){
  api_error(message = "Elevation preference is not valid: it should be either a number or two numbers separated by a hyphen.", status=400)
}

elev_decreasing <- function(){
  api_error(message = "Elevation preference is not valid: minimum elevation higher than maximum.", status=400)
}

no_hab_API <- function(){
  api_error(message = "We cannot find habitat/elevation preferences because the Red List API is not working. Please add them manually.", status=400)
}

run_Step2 <- function(){
  api_error(message = "You need to run Step 2 (filtering records) once again before creating range map since you updated Step 1", status=400)
}



