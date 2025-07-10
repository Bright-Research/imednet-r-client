#' Create a new QueryComment
#'
#' @description
#' QueryComment Class
#'
#' @docType class
#' @title QueryComment
#' @description QueryComment Class
#' @format An \code{R6Class} generator object
#' @field sequence Sequence number of the comment/action in the query history integer [optional]
#' @field annotationStatus Status of the query after this comment (e.g., Open, Closed) character [optional]
#' @field user Username of the user who made the comment or action character [optional]
#' @field comment Text of the comment character [optional]
#' @field closed Whether the query was marked closed at this step character [optional]
#' @field date Date of the comment or action character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
QueryComment <- R6::R6Class(
  "QueryComment",
  public = list(
    `sequence` = NULL,
    `annotationStatus` = NULL,
    `user` = NULL,
    `comment` = NULL,
    `closed` = NULL,
    `date` = NULL,

    #' @description
    #' Initialize a new QueryComment class.
    #'
    #' @param sequence Sequence number of the comment/action in the query history
    #' @param annotationStatus Status of the query after this comment (e.g., Open, Closed)
    #' @param user Username of the user who made the comment or action
    #' @param comment Text of the comment
    #' @param closed Whether the query was marked closed at this step
    #' @param date Date of the comment or action
    #' @param ... Other optional arguments.
    initialize = function(`sequence` = NULL, `annotationStatus` = NULL, `user` = NULL, `comment` = NULL, `closed` = NULL, `date` = NULL, ...) {
      if (!is.null(`sequence`)) {
        if (!(is.numeric(`sequence`) && length(`sequence`) == 1)) {
          stop(paste("Error! Invalid data for `sequence`. Must be an integer:", `sequence`))
        }
        self$`sequence` <- `sequence`
      }
      if (!is.null(`annotationStatus`)) {
        if (!(is.character(`annotationStatus`) && length(`annotationStatus`) == 1)) {
          stop(paste("Error! Invalid data for `annotationStatus`. Must be a string:", `annotationStatus`))
        }
        self$`annotationStatus` <- `annotationStatus`
      }
      if (!is.null(`user`)) {
        if (!(is.character(`user`) && length(`user`) == 1)) {
          stop(paste("Error! Invalid data for `user`. Must be a string:", `user`))
        }
        self$`user` <- `user`
      }
      if (!is.null(`comment`)) {
        if (!(is.character(`comment`) && length(`comment`) == 1)) {
          stop(paste("Error! Invalid data for `comment`. Must be a string:", `comment`))
        }
        self$`comment` <- `comment`
      }
      if (!is.null(`closed`)) {
        if (!(is.logical(`closed`) && length(`closed`) == 1)) {
          stop(paste("Error! Invalid data for `closed`. Must be a boolean:", `closed`))
        }
        self$`closed` <- `closed`
      }
      if (!is.null(`date`)) {
        if (!(is.character(`date`) && length(`date`) == 1)) {
          stop(paste("Error! Invalid data for `date`. Must be a string:", `date`))
        }
        self$`date` <- `date`
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
    #' @return QueryComment as a base R list.
    #' @examples
    #' # convert array of QueryComment (x) to a data frame
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
    #' Convert QueryComment to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      QueryCommentObject <- list()
      if (!is.null(self$`sequence`)) {
        QueryCommentObject[["sequence"]] <-
          self$`sequence`
      }
      if (!is.null(self$`annotationStatus`)) {
        QueryCommentObject[["annotationStatus"]] <-
          self$`annotationStatus`
      }
      if (!is.null(self$`user`)) {
        QueryCommentObject[["user"]] <-
          self$`user`
      }
      if (!is.null(self$`comment`)) {
        QueryCommentObject[["comment"]] <-
          self$`comment`
      }
      if (!is.null(self$`closed`)) {
        QueryCommentObject[["closed"]] <-
          self$`closed`
      }
      if (!is.null(self$`date`)) {
        QueryCommentObject[["date"]] <-
          self$`date`
      }
      return(QueryCommentObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of QueryComment
    #'
    #' @param input_json the JSON input
    #' @return the instance of QueryComment
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`sequence`)) {
        self$`sequence` <- this_object$`sequence`
      }
      if (!is.null(this_object$`annotationStatus`)) {
        self$`annotationStatus` <- this_object$`annotationStatus`
      }
      if (!is.null(this_object$`user`)) {
        self$`user` <- this_object$`user`
      }
      if (!is.null(this_object$`comment`)) {
        self$`comment` <- this_object$`comment`
      }
      if (!is.null(this_object$`closed`)) {
        self$`closed` <- this_object$`closed`
      }
      if (!is.null(this_object$`date`)) {
        self$`date` <- this_object$`date`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return QueryComment in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of QueryComment
    #'
    #' @param input_json the JSON input
    #' @return the instance of QueryComment
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`sequence` <- this_object$`sequence`
      self$`annotationStatus` <- this_object$`annotationStatus`
      self$`user` <- this_object$`user`
      self$`comment` <- this_object$`comment`
      self$`closed` <- this_object$`closed`
      self$`date` <- this_object$`date`
      self
    },

    #' @description
    #' Validate JSON input with respect to QueryComment and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of QueryComment
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
# QueryComment$unlock()
#
## Below is an example to define the print function
# QueryComment$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# QueryComment$lock()

