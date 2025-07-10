#' Create a new InlineObject5
#'
#' @description
#' InlineObject5 Class
#'
#' @docType class
#' @title InlineObject5
#' @description InlineObject5 Class
#' @format An \code{R6Class} generator object
#' @field metadata  \link{Metadata} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
InlineObject5 <- R6::R6Class(
  "InlineObject5",
  public = list(
    `metadata` = NULL,

    #' @description
    #' Initialize a new InlineObject5 class.
    #'
    #' @param metadata metadata
    #' @param ... Other optional arguments.
    initialize = function(`metadata` = NULL, ...) {
      if (!is.null(`metadata`)) {
        stopifnot(R6::is.R6(`metadata`))
        self$`metadata` <- `metadata`
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
    #' @return InlineObject5 as a base R list.
    #' @examples
    #' # convert array of InlineObject5 (x) to a data frame
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
    #' Convert InlineObject5 to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      InlineObject5Object <- list()
      if (!is.null(self$`metadata`)) {
        InlineObject5Object[["metadata"]] <-
          self$`metadata`$toSimpleType()
      }
      return(InlineObject5Object)
    },

    #' @description
    #' Deserialize JSON string into an instance of InlineObject5
    #'
    #' @param input_json the JSON input
    #' @return the instance of InlineObject5
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`metadata`)) {
        `metadata_object` <- Metadata$new()
        `metadata_object`$fromJSON(jsonlite::toJSON(this_object$`metadata`, auto_unbox = TRUE, digits = NA))
        self$`metadata` <- `metadata_object`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return InlineObject5 in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of InlineObject5
    #'
    #' @param input_json the JSON input
    #' @return the instance of InlineObject5
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`metadata` <- Metadata$new()$fromJSON(jsonlite::toJSON(this_object$`metadata`, auto_unbox = TRUE, digits = NA))
      self
    },

    #' @description
    #' Validate JSON input with respect to InlineObject5 and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of InlineObject5
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
# InlineObject5$unlock()
#
## Below is an example to define the print function
# InlineObject5$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# InlineObject5$lock()

