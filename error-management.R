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
    messageBody <- paste0("Internal server error: ", err$message) # nolint
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

species_not_in_distrib <- function() {
  api_error(message = "The species is not present in the shapefile, there may be a mistake in column 'binomial'", status = 400)
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

no_density_fragm <- function() {
  api_error(message = "A density must be provided in the AOH step to perform this step", status = 400)
}

neg_kernel <- function() {
  api_error(message = "Kernel parameter cannot be negative", status = 400)
}

neg_alpha <- function() {
  api_error(message = "Alpha parameter cannot be negative", status = 400)
}

coords_outofbound <- function() {
  api_error(message = "Coordinates should be in decimal with longitude (dec_long) between -180 and 180 and latitude (dec_lat) between -90 and 90. Make sure you used dots and not commas to write decimals in your csv file, this may cause issues.", status = 400)
}

wrong_species_upload <- function() {
  api_error(message = "At least one of the uploaded observations has a different species name (sci_name) than the name of the species you are assessing. Please edit your csv file before uploading again.", status = 400)
}
