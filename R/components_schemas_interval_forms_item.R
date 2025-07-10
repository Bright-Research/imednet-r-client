#' Create a new ComponentsSchemasIntervalFormsItem
#'
#' @description
#' ComponentsSchemasIntervalFormsItem Class
#'
#' @docType class
#' @title ComponentsSchemasIntervalFormsItem
#' @description ComponentsSchemasIntervalFormsItem Class
#' @format An \code{R6Class} generator object
#' @field formId Form ID scheduled in the interval integer [optional]
#' @field formKey Form key scheduled in the interval character [optional]
#' @field formName Form name scheduled in the interval character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ComponentsSchemasIntervalFormsItem <- R6::R6Class(
  "ComponentsSchemasIntervalFormsItem",
  public = list(
    `formId` = NULL,
    `formKey` = NULL,
    `formName` = NULL,

    #' @description
    #' Initialize a new ComponentsSchemasIntervalFormsItem class.
    #'
    #' @param formId Form ID scheduled in the interval
    #' @param formKey Form key scheduled in the interval
    #' @param formName Form name scheduled in the interval
    #' @param ... Other optional arguments.
    initialize = function(`formId` = NULL, `formKey` = NULL, `formName` = NULL, ...) {
      if (!is.null(`formId`)) {
        if (!(is.numeric(`formId`) && length(`formId`) == 1)) {
          stop(paste("Error! Invalid data for `formId`. Must be an integer:", `formId`))
        }
        self$`formId` <- `formId`
      }
      if (!is.null(`formKey`)) {
        if (!(is.character(`formKey`) && length(`formKey`) == 1)) {
          stop(paste("Error! Invalid data for `formKey`. Must be a string:", `formKey`))
        }
        self$`formKey` <- `formKey`
      }
      if (!is.null(`formName`)) {
        if (!(is.character(`formName`) && length(`formName`) == 1)) {
          stop(paste("Error! Invalid data for `formName`. Must be a string:", `formName`))
        }
        self$`formName` <- `formName`
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
    #' @return ComponentsSchemasIntervalFormsItem as a base R list.
    #' @examples
    #' # convert array of ComponentsSchemasIntervalFormsItem (x) to a data frame
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
    #' Convert ComponentsSchemasIntervalFormsItem to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      ComponentsSchemasIntervalFormsItemObject <- list()
      if (!is.null(self$`formId`)) {
        ComponentsSchemasIntervalFormsItemObject[["formId"]] <-
          self$`formId`
      }
      if (!is.null(self$`formKey`)) {
        ComponentsSchemasIntervalFormsItemObject[["formKey"]] <-
          self$`formKey`
      }
      if (!is.null(self$`formName`)) {
        ComponentsSchemasIntervalFormsItemObject[["formName"]] <-
          self$`formName`
      }
      return(ComponentsSchemasIntervalFormsItemObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of ComponentsSchemasIntervalFormsItem
    #'
    #' @param input_json the JSON input
    #' @return the instance of ComponentsSchemasIntervalFormsItem
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`formId`)) {
        self$`formId` <- this_object$`formId`
      }
      if (!is.null(this_object$`formKey`)) {
        self$`formKey` <- this_object$`formKey`
      }
      if (!is.null(this_object$`formName`)) {
        self$`formName` <- this_object$`formName`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return ComponentsSchemasIntervalFormsItem in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of ComponentsSchemasIntervalFormsItem
    #'
    #' @param input_json the JSON input
    #' @return the instance of ComponentsSchemasIntervalFormsItem
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`formId` <- this_object$`formId`
      self$`formKey` <- this_object$`formKey`
      self$`formName` <- this_object$`formName`
      self
    },

    #' @description
    #' Validate JSON input with respect to ComponentsSchemasIntervalFormsItem and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of ComponentsSchemasIntervalFormsItem
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
# ComponentsSchemasIntervalFormsItem$unlock()
#
## Below is an example to define the print function
# ComponentsSchemasIntervalFormsItem$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# ComponentsSchemasIntervalFormsItem$lock()

