#' Create a new User
#'
#' @description
#' User Class
#'
#' @docType class
#' @title User
#' @description User Class
#' @format An \code{R6Class} generator object
#' @field userId Unique user ID (UUID) character [optional]
#' @field login User login name character [optional]
#' @field firstName User first name character [optional]
#' @field lastName User last name character [optional]
#' @field email User email address character [optional]
#' @field userActiveInStudy Whether the user is active in the given study character [optional]
#' @field roles Roles that the user has in the study list(\link{Role}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
User <- R6::R6Class(
  "User",
  public = list(
    `userId` = NULL,
    `login` = NULL,
    `firstName` = NULL,
    `lastName` = NULL,
    `email` = NULL,
    `userActiveInStudy` = NULL,
    `roles` = NULL,

    #' @description
    #' Initialize a new User class.
    #'
    #' @param userId Unique user ID (UUID)
    #' @param login User login name
    #' @param firstName User first name
    #' @param lastName User last name
    #' @param email User email address
    #' @param userActiveInStudy Whether the user is active in the given study
    #' @param roles Roles that the user has in the study
    #' @param ... Other optional arguments.
    initialize = function(`userId` = NULL, `login` = NULL, `firstName` = NULL, `lastName` = NULL, `email` = NULL, `userActiveInStudy` = NULL, `roles` = NULL, ...) {
      if (!is.null(`userId`)) {
        if (!(is.character(`userId`) && length(`userId`) == 1)) {
          stop(paste("Error! Invalid data for `userId`. Must be a string:", `userId`))
        }
        self$`userId` <- `userId`
      }
      if (!is.null(`login`)) {
        if (!(is.character(`login`) && length(`login`) == 1)) {
          stop(paste("Error! Invalid data for `login`. Must be a string:", `login`))
        }
        self$`login` <- `login`
      }
      if (!is.null(`firstName`)) {
        if (!(is.character(`firstName`) && length(`firstName`) == 1)) {
          stop(paste("Error! Invalid data for `firstName`. Must be a string:", `firstName`))
        }
        self$`firstName` <- `firstName`
      }
      if (!is.null(`lastName`)) {
        if (!(is.character(`lastName`) && length(`lastName`) == 1)) {
          stop(paste("Error! Invalid data for `lastName`. Must be a string:", `lastName`))
        }
        self$`lastName` <- `lastName`
      }
      if (!is.null(`email`)) {
        if (!(is.character(`email`) && length(`email`) == 1)) {
          stop(paste("Error! Invalid data for `email`. Must be a string:", `email`))
        }
        self$`email` <- `email`
      }
      if (!is.null(`userActiveInStudy`)) {
        if (!(is.logical(`userActiveInStudy`) && length(`userActiveInStudy`) == 1)) {
          stop(paste("Error! Invalid data for `userActiveInStudy`. Must be a boolean:", `userActiveInStudy`))
        }
        self$`userActiveInStudy` <- `userActiveInStudy`
      }
      if (!is.null(`roles`)) {
        stopifnot(is.vector(`roles`), length(`roles`) != 0)
        sapply(`roles`, function(x) stopifnot(R6::is.R6(x)))
        self$`roles` <- `roles`
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
    #' @return User as a base R list.
    #' @examples
    #' # convert array of User (x) to a data frame
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
    #' Convert User to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      UserObject <- list()
      if (!is.null(self$`userId`)) {
        UserObject[["userId"]] <-
          self$`userId`
      }
      if (!is.null(self$`login`)) {
        UserObject[["login"]] <-
          self$`login`
      }
      if (!is.null(self$`firstName`)) {
        UserObject[["firstName"]] <-
          self$`firstName`
      }
      if (!is.null(self$`lastName`)) {
        UserObject[["lastName"]] <-
          self$`lastName`
      }
      if (!is.null(self$`email`)) {
        UserObject[["email"]] <-
          self$`email`
      }
      if (!is.null(self$`userActiveInStudy`)) {
        UserObject[["userActiveInStudy"]] <-
          self$`userActiveInStudy`
      }
      if (!is.null(self$`roles`)) {
        UserObject[["roles"]] <-
          lapply(self$`roles`, function(x) x$toSimpleType())
      }
      return(UserObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of User
    #'
    #' @param input_json the JSON input
    #' @return the instance of User
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`userId`)) {
        self$`userId` <- this_object$`userId`
      }
      if (!is.null(this_object$`login`)) {
        self$`login` <- this_object$`login`
      }
      if (!is.null(this_object$`firstName`)) {
        self$`firstName` <- this_object$`firstName`
      }
      if (!is.null(this_object$`lastName`)) {
        self$`lastName` <- this_object$`lastName`
      }
      if (!is.null(this_object$`email`)) {
        self$`email` <- this_object$`email`
      }
      if (!is.null(this_object$`userActiveInStudy`)) {
        self$`userActiveInStudy` <- this_object$`userActiveInStudy`
      }
      if (!is.null(this_object$`roles`)) {
        self$`roles` <- ApiClient$new()$deserializeObj(this_object$`roles`, "array[Role]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return User in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of User
    #'
    #' @param input_json the JSON input
    #' @return the instance of User
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`userId` <- this_object$`userId`
      self$`login` <- this_object$`login`
      self$`firstName` <- this_object$`firstName`
      self$`lastName` <- this_object$`lastName`
      self$`email` <- this_object$`email`
      self$`userActiveInStudy` <- this_object$`userActiveInStudy`
      self$`roles` <- ApiClient$new()$deserializeObj(this_object$`roles`, "array[Role]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to User and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of User
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
# User$unlock()
#
## Below is an example to define the print function
# User$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# User$lock()

