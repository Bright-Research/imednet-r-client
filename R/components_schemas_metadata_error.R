#' Create a new ComponentsSchemasMetadataError
#'
#' @description
#' Error details if an error occurred
#'
#' @docType class
#' @title ComponentsSchemasMetadataError
#' @description ComponentsSchemasMetadataError Class
#' @format An \code{R6Class} generator object
#' @field message Error message describing the issue character [optional]
#' @field _field_list a list of fields list(character)
#' @field additional_properties additional properties list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ComponentsSchemasMetadataError <- R6::R6Class(
  "ComponentsSchemasMetadataError",
  inherit = AnyType,
  public = list(
    `message` = NULL,
    `_field_list` = c("message"),
    `additional_properties` = list(),

    #' @description
    #' Initialize a new ComponentsSchemasMetadataError class.
    #'
    #' @param message Error message describing the issue
    #' @param additional_properties additional properties (optional)
    #' @param ... Other optional arguments.
    initialize = function(`message` = NULL, additional_properties = NULL, ...) {
      if (!is.null(`message`)) {
        if (!(is.character(`message`) && length(`message`) == 1)) {
          stop(paste("Error! Invalid data for `message`. Must be a string:", `message`))
        }
        self$`message` <- `message`
      }
      if (!is.null(additional_properties)) {
        for (key in names(additional_properties)) {
          self$additional_properties[[key]] <- additional_properties[[key]]
        }
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
    #' @return ComponentsSchemasMetadataError as a base R list.
    #' @examples
    #' # convert array of ComponentsSchemasMetadataError (x) to a data frame
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
    #' Convert ComponentsSchemasMetadataError to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      ComponentsSchemasMetadataErrorObject <- list()
      if (!is.null(self$`message`)) {
        ComponentsSchemasMetadataErrorObject[["message"]] <-
          self$`message`
      }
      for (key in names(self$additional_properties)) {
        ComponentsSchemasMetadataErrorObject[[key]] <- self$additional_properties[[key]]
      }

      return(ComponentsSchemasMetadataErrorObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of ComponentsSchemasMetadataError
    #'
    #' @param input_json the JSON input
    #' @return the instance of ComponentsSchemasMetadataError
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`message`)) {
        self$`message` <- this_object$`message`
      }
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return ComponentsSchemasMetadataError in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      for (key in names(self$additional_properties)) {
        simple[[key]] <- self$additional_properties[[key]]
      }
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of ComponentsSchemasMetadataError
    #'
    #' @param input_json the JSON input
    #' @return the instance of ComponentsSchemasMetadataError
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`message` <- this_object$`message`
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },

    #' @description
    #' Validate JSON input with respect to ComponentsSchemasMetadataError and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of ComponentsSchemasMetadataError
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
# ComponentsSchemasMetadataError$unlock()
#
## Below is an example to define the print function
# ComponentsSchemasMetadataError$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# ComponentsSchemasMetadataError$lock()

