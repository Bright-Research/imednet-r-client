#' Create a new Keyword
#'
#' @description
#' Keyword Class
#'
#' @docType class
#' @title Keyword
#' @description Keyword Class
#' @format An \code{R6Class} generator object
#' @field keywordName Name of the keyword character [optional]
#' @field keywordKey Key of the keyword (short code) character [optional]
#' @field keywordId Internal keyword ID integer [optional]
#' @field dateAdded Date when this keyword was added character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Keyword <- R6::R6Class(
  "Keyword",
  public = list(
    `keywordName` = NULL,
    `keywordKey` = NULL,
    `keywordId` = NULL,
    `dateAdded` = NULL,

    #' @description
    #' Initialize a new Keyword class.
    #'
    #' @param keywordName Name of the keyword
    #' @param keywordKey Key of the keyword (short code)
    #' @param keywordId Internal keyword ID
    #' @param dateAdded Date when this keyword was added
    #' @param ... Other optional arguments.
    initialize = function(`keywordName` = NULL, `keywordKey` = NULL, `keywordId` = NULL, `dateAdded` = NULL, ...) {
      if (!is.null(`keywordName`)) {
        if (!(is.character(`keywordName`) && length(`keywordName`) == 1)) {
          stop(paste("Error! Invalid data for `keywordName`. Must be a string:", `keywordName`))
        }
        self$`keywordName` <- `keywordName`
      }
      if (!is.null(`keywordKey`)) {
        if (!(is.character(`keywordKey`) && length(`keywordKey`) == 1)) {
          stop(paste("Error! Invalid data for `keywordKey`. Must be a string:", `keywordKey`))
        }
        self$`keywordKey` <- `keywordKey`
      }
      if (!is.null(`keywordId`)) {
        if (!(is.numeric(`keywordId`) && length(`keywordId`) == 1)) {
          stop(paste("Error! Invalid data for `keywordId`. Must be an integer:", `keywordId`))
        }
        self$`keywordId` <- `keywordId`
      }
      if (!is.null(`dateAdded`)) {
        if (!(is.character(`dateAdded`) && length(`dateAdded`) == 1)) {
          stop(paste("Error! Invalid data for `dateAdded`. Must be a string:", `dateAdded`))
        }
        self$`dateAdded` <- `dateAdded`
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
    #' @return Keyword as a base R list.
    #' @examples
    #' # convert array of Keyword (x) to a data frame
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
    #' Convert Keyword to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      KeywordObject <- list()
      if (!is.null(self$`keywordName`)) {
        KeywordObject[["keywordName"]] <-
          self$`keywordName`
      }
      if (!is.null(self$`keywordKey`)) {
        KeywordObject[["keywordKey"]] <-
          self$`keywordKey`
      }
      if (!is.null(self$`keywordId`)) {
        KeywordObject[["keywordId"]] <-
          self$`keywordId`
      }
      if (!is.null(self$`dateAdded`)) {
        KeywordObject[["dateAdded"]] <-
          self$`dateAdded`
      }
      return(KeywordObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Keyword
    #'
    #' @param input_json the JSON input
    #' @return the instance of Keyword
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`keywordName`)) {
        self$`keywordName` <- this_object$`keywordName`
      }
      if (!is.null(this_object$`keywordKey`)) {
        self$`keywordKey` <- this_object$`keywordKey`
      }
      if (!is.null(this_object$`keywordId`)) {
        self$`keywordId` <- this_object$`keywordId`
      }
      if (!is.null(this_object$`dateAdded`)) {
        self$`dateAdded` <- this_object$`dateAdded`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Keyword in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Keyword
    #'
    #' @param input_json the JSON input
    #' @return the instance of Keyword
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`keywordName` <- this_object$`keywordName`
      self$`keywordKey` <- this_object$`keywordKey`
      self$`keywordId` <- this_object$`keywordId`
      self$`dateAdded` <- this_object$`dateAdded`
      self
    },

    #' @description
    #' Validate JSON input with respect to Keyword and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Keyword
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
# Keyword$unlock()
#
## Below is an example to define the print function
# Keyword$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Keyword$lock()

