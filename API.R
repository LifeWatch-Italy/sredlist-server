# API.R

#* @apiTitle LifeWach-sRedList-Platform API
#* @apiDescription This is a sample server for a Red List workflow.
#* @apiTOS http://example.com/terms/
#* @apiContact list(name = "API Support", url = "http://www.example.com/support", email = "support@example.com") # nolint
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html") # nolint
#* @apiVersion 1.0.0
#* @apiTag RedList-API API from RedList
#* @apiTag sRedList Plaftorm API

#* @filter cors
function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    plumber::forward()
}