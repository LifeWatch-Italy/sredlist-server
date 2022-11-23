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
    #res$body <- "{\"status\":500,\"message\":\"Internal server error.\"}"
    res$body <- sprintf(
      "{\"status\":%d,\"message\":\"%s\"}", 500, messageBody
    )
    # Print the internal error so we can see it from the server side. A more
    # robust implementation would use proper logging.
    log_error(err$message)
  } else {
    # We know that the message is intended to be user-facing.
    res$status <- err$status
    res$body <- sprintf(
      "{\"status\":%d,\"message\":\"%s\"}", err$status, err$message
    )
    log_error('{req$PATH_INFO} : {err$message}') # nolint
  }
}

payload_too_large  <- function(message = "Payload Too Large") {
  api_error(message = message, status = 413)
}

not_found <- function(message = "Not found.") {
  api_error(message = message, status = 404)
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

no_habitat_pref <- function(message = "At least one suitable habitat must be selected") {
  api_error(message = message, status = 400)
}