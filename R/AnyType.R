#' AnyType Class
#'
#' Generic container for flexible JSON objects used in the API.
#' @docType class
#' @title AnyType
#' @description Represents an arbitrary object with key/value pairs.
#' @export
AnyType <- R6::R6Class(
  "AnyType",
  public = list(
    `_field_list` = character(0),
    `additional_properties` = list(),

    initialize = function(additional_properties = NULL, ...) {
      if (!is.null(additional_properties)) {
        for (key in names(additional_properties)) {
          self$additional_properties[[key]] <- additional_properties[[key]]
        }
      }
    },

    toJSON = function() {
      .Deprecated(new = "toSimpleType", msg = "Use the '$toSimpleType()' method instead since that is more clearly named. Use '$toJSONString()' to get a JSON string")
      return(self$toSimpleType())
    },

    toList = function() {
      return(self$toSimpleType())
    },

    toSimpleType = function() {
      result <- list()
      for (key in names(self$additional_properties)) {
        result[[key]] <- self$additional_properties[[key]]
      }
      return(result)
    },

    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$additional_properties <- list()
      for (key in names(this_object)) {
        self$additional_properties[[key]] <- this_object[[key]]
      }
      self
    },

    fromJSONString = function(input_json) {
      self$fromJSON(input_json)
    },

    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    validateJSON = function(input) {
      jsonlite::fromJSON(input)
    },

    toString = function() {
      self$toJSONString()
    },

    isValid = function() {
      TRUE
    },

    getInvalidFields = function() {
      list()
    },

    print = function() {
      print(jsonlite::prettify(self$toJSONString()))
      invisible(self)
    }
  ),
  lock_class = TRUE
)
