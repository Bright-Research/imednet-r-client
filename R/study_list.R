#' Create a new StudyList
#'
#' @description
#' StudyList Class
#'
#' @docType class
#' @title StudyList
#' @description StudyList Class
#' @format An \code{R6Class} generator object
#' @field metadata  \link{Metadata} [optional]
#' @field pagination  \link{Pagination} [optional]
#' @field data  list(\link{Study}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
StudyList <- R6::R6Class(
  "StudyList",
  public = list(
    `metadata` = NULL,
    `pagination` = NULL,
    `data` = NULL,

    #' @description
    #' Initialize a new StudyList class.
    #'
    #' @param metadata metadata
    #' @param pagination pagination
    #' @param data data
    #' @param ... Other optional arguments.
    initialize = function(`metadata` = NULL, `pagination` = NULL, `data` = NULL, ...) {
      if (!is.null(`metadata`)) {
        stopifnot(R6::is.R6(`metadata`))
        self$`metadata` <- `metadata`
      }
      if (!is.null(`pagination`)) {
        stopifnot(R6::is.R6(`pagination`))
        self$`pagination` <- `pagination`
      }
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), length(`data`) != 0)
        sapply(`data`, function(x) stopifnot(R6::is.R6(x)))
        self$`data` <- `data`
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
    #' @return StudyList as a base R list.
    #' @examples
    #' # convert array of StudyList (x) to a data frame
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
    #' Convert StudyList to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      StudyListObject <- list()
      if (!is.null(self$`metadata`)) {
        StudyListObject[["metadata"]] <-
          self$`metadata`$toSimpleType()
      }
      if (!is.null(self$`pagination`)) {
        StudyListObject[["pagination"]] <-
          self$`pagination`$toSimpleType()
      }
      if (!is.null(self$`data`)) {
        StudyListObject[["data"]] <-
          lapply(self$`data`, function(x) x$toSimpleType())
      }
      return(StudyListObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of StudyList
    #'
    #' @param input_json the JSON input
    #' @return the instance of StudyList
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`metadata`)) {
        `metadata_object` <- Metadata$new()
        `metadata_object`$fromJSON(jsonlite::toJSON(this_object$`metadata`, auto_unbox = TRUE, digits = NA))
        self$`metadata` <- `metadata_object`
      }
      if (!is.null(this_object$`pagination`)) {
        `pagination_object` <- Pagination$new()
        `pagination_object`$fromJSON(jsonlite::toJSON(this_object$`pagination`, auto_unbox = TRUE, digits = NA))
        self$`pagination` <- `pagination_object`
      }
      if (!is.null(this_object$`data`)) {
        self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "array[Study]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return StudyList in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of StudyList
    #'
    #' @param input_json the JSON input
    #' @return the instance of StudyList
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`metadata` <- Metadata$new()$fromJSON(jsonlite::toJSON(this_object$`metadata`, auto_unbox = TRUE, digits = NA))
      self$`pagination` <- Pagination$new()$fromJSON(jsonlite::toJSON(this_object$`pagination`, auto_unbox = TRUE, digits = NA))
      self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "array[Study]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to StudyList and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of StudyList
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
# StudyList$unlock()
#
## Below is an example to define the print function
# StudyList$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# StudyList$lock()

