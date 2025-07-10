#' Create a new RecordJobStatus
#'
#' @description
#' RecordJobStatus Class
#'
#' @docType class
#' @title RecordJobStatus
#' @description RecordJobStatus Class
#' @format An \code{R6Class} generator object
#' @field jobId Unique job identifier character [optional]
#' @field batchId Batch ID used to track the job (identical to jobId in most cases) character [optional]
#' @field state Current state of the job (e.g., created, completed) character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
RecordJobStatus <- R6::R6Class(
  "RecordJobStatus",
  public = list(
    `jobId` = NULL,
    `batchId` = NULL,
    `state` = NULL,

    #' @description
    #' Initialize a new RecordJobStatus class.
    #'
    #' @param jobId Unique job identifier
    #' @param batchId Batch ID used to track the job (identical to jobId in most cases)
    #' @param state Current state of the job (e.g., created, completed)
    #' @param ... Other optional arguments.
    initialize = function(`jobId` = NULL, `batchId` = NULL, `state` = NULL, ...) {
      if (!is.null(`jobId`)) {
        if (!(is.character(`jobId`) && length(`jobId`) == 1)) {
          stop(paste("Error! Invalid data for `jobId`. Must be a string:", `jobId`))
        }
        self$`jobId` <- `jobId`
      }
      if (!is.null(`batchId`)) {
        if (!(is.character(`batchId`) && length(`batchId`) == 1)) {
          stop(paste("Error! Invalid data for `batchId`. Must be a string:", `batchId`))
        }
        self$`batchId` <- `batchId`
      }
      if (!is.null(`state`)) {
        if (!(is.character(`state`) && length(`state`) == 1)) {
          stop(paste("Error! Invalid data for `state`. Must be a string:", `state`))
        }
        self$`state` <- `state`
      }
    },

    #' @description
    #' Convert to an R object. This method is deprecated. Use `toSimpleType()` instead.
    toJSON = function() {
      .Deprecated(new = "toSimpleType", msg = "Use the '$toSimpleType()' method instead since that is more clearly named. Use '$toJSONString()' to get a JSON string")
      return(self$toSimpleType())
    },

    #' @description
    #' Convert to a List
    #'
    #' Convert the R6 object to a list to work more easily with other tooling.
    #'
    #' @return RecordJobStatus as a base R list.
    #' @examples
    #' # convert array of RecordJobStatus (x) to a data frame
    #' \dontrun{
    #' library(purrr)
    #' library(tibble)
    #' df <- x |> map(\(y)y$toList()) |> map(as_tibble) |> list_rbind()
    #' df
    #' }
    toList = function() {
      return(self$toSimpleType())
    },

    #' @description
    #' Convert RecordJobStatus to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      RecordJobStatusObject <- list()
      if (!is.null(self$`jobId`)) {
        RecordJobStatusObject[["jobId"]] <-
          self$`jobId`
      }
      if (!is.null(self$`batchId`)) {
        RecordJobStatusObject[["batchId"]] <-
          self$`batchId`
      }
      if (!is.null(self$`state`)) {
        RecordJobStatusObject[["state"]] <-
          self$`state`
      }
      return(RecordJobStatusObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of RecordJobStatus
    #'
    #' @param input_json the JSON input
    #' @return the instance of RecordJobStatus
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`jobId`)) {
        self$`jobId` <- this_object$`jobId`
      }
      if (!is.null(this_object$`batchId`)) {
        self$`batchId` <- this_object$`batchId`
      }
      if (!is.null(this_object$`state`)) {
        self$`state` <- this_object$`state`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return RecordJobStatus in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of RecordJobStatus
    #'
    #' @param input_json the JSON input
    #' @return the instance of RecordJobStatus
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`jobId` <- this_object$`jobId`
      self$`batchId` <- this_object$`batchId`
      self$`state` <- this_object$`state`
      self
    },

    #' @description
    #' Validate JSON input with respect to RecordJobStatus and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of RecordJobStatus
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      TRUE
    },

    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    getInvalidFields = function() {
      invalid_fields <- list()
      invalid_fields
    },

    #' @description
    #' Print the object
    print = function() {
      print(jsonlite::prettify(self$toJSONString()))
      invisible(self)
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)
## Uncomment below to unlock the class to allow modifications of the method or field
# RecordJobStatus$unlock()
#
## Below is an example to define the print function
# RecordJobStatus$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# RecordJobStatus$lock()

