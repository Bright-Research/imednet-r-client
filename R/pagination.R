#' Create a new Pagination
#'
#' @description
#' Pagination Class
#'
#' @docType class
#' @title Pagination
#' @description Pagination Class
#' @format An \code{R6Class} generator object
#' @field currentPage Current index page returned integer [optional]
#' @field size Number of items per page returned integer [optional]
#' @field totalPages Total number of pages available integer [optional]
#' @field totalElements Total number of elements (items) available integer [optional]
#' @field sort  list(\link{Sort}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Pagination <- R6::R6Class(
  "Pagination",
  public = list(
    `currentPage` = NULL,
    `size` = NULL,
    `totalPages` = NULL,
    `totalElements` = NULL,
    `sort` = NULL,

    #' @description
    #' Initialize a new Pagination class.
    #'
    #' @param currentPage Current index page returned
    #' @param size Number of items per page returned
    #' @param totalPages Total number of pages available
    #' @param totalElements Total number of elements (items) available
    #' @param sort sort
    #' @param ... Other optional arguments.
    initialize = function(`currentPage` = NULL, `size` = NULL, `totalPages` = NULL, `totalElements` = NULL, `sort` = NULL, ...) {
      if (!is.null(`currentPage`)) {
        if (!(is.numeric(`currentPage`) && length(`currentPage`) == 1)) {
          stop(paste("Error! Invalid data for `currentPage`. Must be an integer:", `currentPage`))
        }
        self$`currentPage` <- `currentPage`
      }
      if (!is.null(`size`)) {
        if (!(is.numeric(`size`) && length(`size`) == 1)) {
          stop(paste("Error! Invalid data for `size`. Must be an integer:", `size`))
        }
        self$`size` <- `size`
      }
      if (!is.null(`totalPages`)) {
        if (!(is.numeric(`totalPages`) && length(`totalPages`) == 1)) {
          stop(paste("Error! Invalid data for `totalPages`. Must be an integer:", `totalPages`))
        }
        self$`totalPages` <- `totalPages`
      }
      if (!is.null(`totalElements`)) {
        if (!(is.numeric(`totalElements`) && length(`totalElements`) == 1)) {
          stop(paste("Error! Invalid data for `totalElements`. Must be an integer:", `totalElements`))
        }
        self$`totalElements` <- `totalElements`
      }
      if (!is.null(`sort`)) {
        stopifnot(is.vector(`sort`), length(`sort`) != 0)
        sapply(`sort`, function(x) stopifnot(R6::is.R6(x)))
        self$`sort` <- `sort`
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
    #' @return Pagination as a base R list.
    #' @examples
    #' # convert array of Pagination (x) to a data frame
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
    #' Convert Pagination to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      PaginationObject <- list()
      if (!is.null(self$`currentPage`)) {
        PaginationObject[["currentPage"]] <-
          self$`currentPage`
      }
      if (!is.null(self$`size`)) {
        PaginationObject[["size"]] <-
          self$`size`
      }
      if (!is.null(self$`totalPages`)) {
        PaginationObject[["totalPages"]] <-
          self$`totalPages`
      }
      if (!is.null(self$`totalElements`)) {
        PaginationObject[["totalElements"]] <-
          self$`totalElements`
      }
      if (!is.null(self$`sort`)) {
        PaginationObject[["sort"]] <-
          lapply(self$`sort`, function(x) x$toSimpleType())
      }
      return(PaginationObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Pagination
    #'
    #' @param input_json the JSON input
    #' @return the instance of Pagination
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`currentPage`)) {
        self$`currentPage` <- this_object$`currentPage`
      }
      if (!is.null(this_object$`size`)) {
        self$`size` <- this_object$`size`
      }
      if (!is.null(this_object$`totalPages`)) {
        self$`totalPages` <- this_object$`totalPages`
      }
      if (!is.null(this_object$`totalElements`)) {
        self$`totalElements` <- this_object$`totalElements`
      }
      if (!is.null(this_object$`sort`)) {
        self$`sort` <- ApiClient$new()$deserializeObj(this_object$`sort`, "array[Sort]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Pagination in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Pagination
    #'
    #' @param input_json the JSON input
    #' @return the instance of Pagination
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`currentPage` <- this_object$`currentPage`
      self$`size` <- this_object$`size`
      self$`totalPages` <- this_object$`totalPages`
      self$`totalElements` <- this_object$`totalElements`
      self$`sort` <- ApiClient$new()$deserializeObj(this_object$`sort`, "array[Sort]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to Pagination and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Pagination
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
# Pagination$unlock()
#
## Below is an example to define the print function
# Pagination$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Pagination$lock()

