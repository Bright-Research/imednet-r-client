#' Create a new Metadata
#'
#' @description
#' Metadata Class
#'
#' @docType class
#' @title Metadata
#' @description Metadata Class
#' @format An \code{R6Class} generator object
#' @field status HTTP status of the response (e.g., OK or ERROR) character [optional]
#' @field method HTTP method of the request character [optional]
#' @field path Requested URI path character [optional]
#' @field timestamp Timestamp when response was generated character [optional]
#' @field error  \link{ComponentsSchemasMetadataError} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Metadata <- R6::R6Class(
  "Metadata",
  public = list(
    `status` = NULL,
    `method` = NULL,
    `path` = NULL,
    `timestamp` = NULL,
    `error` = NULL,

    #' @description
    #' Initialize a new Metadata class.
    #'
    #' @param status HTTP status of the response (e.g., OK or ERROR)
    #' @param method HTTP method of the request
    #' @param path Requested URI path
    #' @param timestamp Timestamp when response was generated
    #' @param error error
    #' @param ... Other optional arguments.
    initialize = function(`status` = NULL, `method` = NULL, `path` = NULL, `timestamp` = NULL, `error` = NULL, ...) {
      if (!is.null(`status`)) {
        if (!(is.character(`status`) && length(`status`) == 1)) {
          stop(paste("Error! Invalid data for `status`. Must be a string:", `status`))
        }
        self$`status` <- `status`
      }
      if (!is.null(`method`)) {
        if (!(is.character(`method`) && length(`method`) == 1)) {
          stop(paste("Error! Invalid data for `method`. Must be a string:", `method`))
        }
        self$`method` <- `method`
      }
      if (!is.null(`path`)) {
        if (!(is.character(`path`) && length(`path`) == 1)) {
          stop(paste("Error! Invalid data for `path`. Must be a string:", `path`))
        }
        self$`path` <- `path`
      }
      if (!is.null(`timestamp`)) {
        if (!is.character(`timestamp`)) {
          stop(paste("Error! Invalid data for `timestamp`. Must be a string:", `timestamp`))
        }
        self$`timestamp` <- `timestamp`
      }
      if (!is.null(`error`)) {
        stopifnot(R6::is.R6(`error`))
        self$`error` <- `error`
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
    #' @return Metadata as a base R list.
    #' @examples
    #' # convert array of Metadata (x) to a data frame
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
    #' Convert Metadata to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      MetadataObject <- list()
      if (!is.null(self$`status`)) {
        MetadataObject[["status"]] <-
          self$`status`
      }
      if (!is.null(self$`method`)) {
        MetadataObject[["method"]] <-
          self$`method`
      }
      if (!is.null(self$`path`)) {
        MetadataObject[["path"]] <-
          self$`path`
      }
      if (!is.null(self$`timestamp`)) {
        MetadataObject[["timestamp"]] <-
          self$`timestamp`
      }
      if (!is.null(self$`error`)) {
        MetadataObject[["error"]] <-
          self$`error`$toSimpleType()
      }
      return(MetadataObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Metadata
    #'
    #' @param input_json the JSON input
    #' @return the instance of Metadata
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`status`)) {
        self$`status` <- this_object$`status`
      }
      if (!is.null(this_object$`method`)) {
        self$`method` <- this_object$`method`
      }
      if (!is.null(this_object$`path`)) {
        self$`path` <- this_object$`path`
      }
      if (!is.null(this_object$`timestamp`)) {
        self$`timestamp` <- this_object$`timestamp`
      }
      if (!is.null(this_object$`error`)) {
        `error_object` <- ComponentsSchemasMetadataError$new()
        `error_object`$fromJSON(jsonlite::toJSON(this_object$`error`, auto_unbox = TRUE, digits = NA))
        self$`error` <- `error_object`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Metadata in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Metadata
    #'
    #' @param input_json the JSON input
    #' @return the instance of Metadata
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`status` <- this_object$`status`
      self$`method` <- this_object$`method`
      self$`path` <- this_object$`path`
      self$`timestamp` <- this_object$`timestamp`
      self$`error` <- ComponentsSchemasMetadataError$new()$fromJSON(jsonlite::toJSON(this_object$`error`, auto_unbox = TRUE, digits = NA))
      self
    },

    #' @description
    #' Validate JSON input with respect to Metadata and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Metadata
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
# Metadata$unlock()
#
## Below is an example to define the print function
# Metadata$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Metadata$lock()

