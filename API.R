# API.R

#* @apiTitle LifeWach-sRedList-Platform API
#* @apiDescription This is a sample server for a sRedList workflow.
#* @apiTOS http://example.com/terms/
#* @apiContact list(name = "API Support", url = "http://www.example.com/support", email = "support@example.com") # nolint
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html") # nolint
#* @apiVersion 1.0.0
#* @apiTag RedList RedList API
#* @apiTag sRedList Plaftorm API

#* @filter cors
function(req, res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS) #nolint
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}