#' Create a new Sort
#'
#' @description
#' Sort Class
#'
#' @docType class
#' @title Sort
#' @description Sort Class
#' @format An \code{R6Class} generator object
#' @field property Name of the property by which the result is sorted character [optional]
#' @field direction Sort direction (ASC or DESC) character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Sort <- R6::R6Class(
  "Sort",
  public = list(
    `property` = NULL,
    `direction` = NULL,

    #' @description
    #' Initialize a new Sort class.
    #'
    #' @param property Name of the property by which the result is sorted
    #' @param direction Sort direction (ASC or DESC)
    #' @param ... Other optional arguments.
    initialize = function(`property` = NULL, `direction` = NULL, ...) {
      if (!is.null(`property`)) {
        if (!(is.character(`property`) && length(`property`) == 1)) {
          stop(paste("Error! Invalid data for `property`. Must be a string:", `property`))
        }
        self$`property` <- `property`
      }
      if (!is.null(`direction`)) {
        if (!(`direction` %in% c("ASC", "DESC"))) {
          stop(paste("Error! \"", `direction`, "\" cannot be assigned to `direction`. Must be \"ASC\", \"DESC\".", sep = ""))
        }
        if (!(is.character(`direction`) && length(`direction`) == 1)) {
          stop(paste("Error! Invalid data for `direction`. Must be a string:", `direction`))
        }
        self$`direction` <- `direction`
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
    #' @return Sort as a base R list.
    #' @examples
    #' # convert array of Sort (x) to a data frame
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
    #' Convert Sort to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      SortObject <- list()
      if (!is.null(self$`property`)) {
        SortObject[["property"]] <-
          self$`property`
      }
      if (!is.null(self$`direction`)) {
        SortObject[["direction"]] <-
          self$`direction`
      }
      return(SortObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Sort
    #'
    #' @param input_json the JSON input
    #' @return the instance of Sort
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`property`)) {
        self$`property` <- this_object$`property`
      }
      if (!is.null(this_object$`direction`)) {
        if (!is.null(this_object$`direction`) && !(this_object$`direction` %in% c("ASC", "DESC"))) {
          stop(paste("Error! \"", this_object$`direction`, "\" cannot be assigned to `direction`. Must be \"ASC\", \"DESC\".", sep = ""))
        }
        self$`direction` <- this_object$`direction`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Sort in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Sort
    #'
    #' @param input_json the JSON input
    #' @return the instance of Sort
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`property` <- this_object$`property`
      if (!is.null(this_object$`direction`) && !(this_object$`direction` %in% c("ASC", "DESC"))) {
        stop(paste("Error! \"", this_object$`direction`, "\" cannot be assigned to `direction`. Must be \"ASC\", \"DESC\".", sep = ""))
      }
      self$`direction` <- this_object$`direction`
      self
    },

    #' @description
    #' Validate JSON input with respect to Sort and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Sort
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
# Sort$unlock()
#
## Below is an example to define the print function
# Sort$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Sort$lock()

