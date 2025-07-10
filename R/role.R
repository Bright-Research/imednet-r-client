#' Create a new Role
#'
#' @description
#' Role Class
#'
#' @docType class
#' @title Role
#' @description Role Class
#' @format An \code{R6Class} generator object
#' @field dateCreated Timestamp when the role assignment was created (as [year, month, day, hour, minute, second, nanosecond]) list(integer) [optional]
#' @field dateModified Timestamp when the role assignment was last modified list(integer) [optional]
#' @field roleId Unique role ID (UUID) character [optional]
#' @field communityId Community ID or level associated with the role integer [optional]
#' @field name Name of the role character [optional]
#' @field description Description of the role character [optional]
#' @field level Role level or hierarchy integer [optional]
#' @field type Role type/category character [optional]
#' @field inactive Whether the role is inactive character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Role <- R6::R6Class(
  "Role",
  public = list(
    `dateCreated` = NULL,
    `dateModified` = NULL,
    `roleId` = NULL,
    `communityId` = NULL,
    `name` = NULL,
    `description` = NULL,
    `level` = NULL,
    `type` = NULL,
    `inactive` = NULL,

    #' @description
    #' Initialize a new Role class.
    #'
    #' @param dateCreated Timestamp when the role assignment was created (as [year, month, day, hour, minute, second, nanosecond])
    #' @param dateModified Timestamp when the role assignment was last modified
    #' @param roleId Unique role ID (UUID)
    #' @param communityId Community ID or level associated with the role
    #' @param name Name of the role
    #' @param description Description of the role
    #' @param level Role level or hierarchy
    #' @param type Role type/category
    #' @param inactive Whether the role is inactive
    #' @param ... Other optional arguments.
    initialize = function(`dateCreated` = NULL, `dateModified` = NULL, `roleId` = NULL, `communityId` = NULL, `name` = NULL, `description` = NULL, `level` = NULL, `type` = NULL, `inactive` = NULL, ...) {
      if (!is.null(`dateCreated`)) {
        stopifnot(is.vector(`dateCreated`), length(`dateCreated`) != 0)
        sapply(`dateCreated`, function(x) stopifnot(is.character(x)))
        self$`dateCreated` <- `dateCreated`
      }
      if (!is.null(`dateModified`)) {
        stopifnot(is.vector(`dateModified`), length(`dateModified`) != 0)
        sapply(`dateModified`, function(x) stopifnot(is.character(x)))
        self$`dateModified` <- `dateModified`
      }
      if (!is.null(`roleId`)) {
        if (!(is.character(`roleId`) && length(`roleId`) == 1)) {
          stop(paste("Error! Invalid data for `roleId`. Must be a string:", `roleId`))
        }
        self$`roleId` <- `roleId`
      }
      if (!is.null(`communityId`)) {
        if (!(is.numeric(`communityId`) && length(`communityId`) == 1)) {
          stop(paste("Error! Invalid data for `communityId`. Must be an integer:", `communityId`))
        }
        self$`communityId` <- `communityId`
      }
      if (!is.null(`name`)) {
        if (!(is.character(`name`) && length(`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", `name`))
        }
        self$`name` <- `name`
      }
      if (!is.null(`description`)) {
        if (!(is.character(`description`) && length(`description`) == 1)) {
          stop(paste("Error! Invalid data for `description`. Must be a string:", `description`))
        }
        self$`description` <- `description`
      }
      if (!is.null(`level`)) {
        if (!(is.numeric(`level`) && length(`level`) == 1)) {
          stop(paste("Error! Invalid data for `level`. Must be an integer:", `level`))
        }
        self$`level` <- `level`
      }
      if (!is.null(`type`)) {
        if (!(is.character(`type`) && length(`type`) == 1)) {
          stop(paste("Error! Invalid data for `type`. Must be a string:", `type`))
        }
        self$`type` <- `type`
      }
      if (!is.null(`inactive`)) {
        if (!(is.logical(`inactive`) && length(`inactive`) == 1)) {
          stop(paste("Error! Invalid data for `inactive`. Must be a boolean:", `inactive`))
        }
        self$`inactive` <- `inactive`
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
    #' @return Role as a base R list.
    #' @examples
    #' # convert array of Role (x) to a data frame
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
    #' Convert Role to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      RoleObject <- list()
      if (!is.null(self$`dateCreated`)) {
        RoleObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        RoleObject[["dateModified"]] <-
          self$`dateModified`
      }
      if (!is.null(self$`roleId`)) {
        RoleObject[["roleId"]] <-
          self$`roleId`
      }
      if (!is.null(self$`communityId`)) {
        RoleObject[["communityId"]] <-
          self$`communityId`
      }
      if (!is.null(self$`name`)) {
        RoleObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        RoleObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`level`)) {
        RoleObject[["level"]] <-
          self$`level`
      }
      if (!is.null(self$`type`)) {
        RoleObject[["type"]] <-
          self$`type`
      }
      if (!is.null(self$`inactive`)) {
        RoleObject[["inactive"]] <-
          self$`inactive`
      }
      return(RoleObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Role
    #'
    #' @param input_json the JSON input
    #' @return the instance of Role
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`dateCreated`)) {
        self$`dateCreated` <- ApiClient$new()$deserializeObj(this_object$`dateCreated`, "array[integer]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`dateModified`)) {
        self$`dateModified` <- ApiClient$new()$deserializeObj(this_object$`dateModified`, "array[integer]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`roleId`)) {
        self$`roleId` <- this_object$`roleId`
      }
      if (!is.null(this_object$`communityId`)) {
        self$`communityId` <- this_object$`communityId`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`level`)) {
        self$`level` <- this_object$`level`
      }
      if (!is.null(this_object$`type`)) {
        self$`type` <- this_object$`type`
      }
      if (!is.null(this_object$`inactive`)) {
        self$`inactive` <- this_object$`inactive`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Role in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Role
    #'
    #' @param input_json the JSON input
    #' @return the instance of Role
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`dateCreated` <- ApiClient$new()$deserializeObj(this_object$`dateCreated`, "array[integer]", loadNamespace("openapi"))
      self$`dateModified` <- ApiClient$new()$deserializeObj(this_object$`dateModified`, "array[integer]", loadNamespace("openapi"))
      self$`roleId` <- this_object$`roleId`
      self$`communityId` <- this_object$`communityId`
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`level` <- this_object$`level`
      self$`type` <- this_object$`type`
      self$`inactive` <- this_object$`inactive`
      self
    },

    #' @description
    #' Validate JSON input with respect to Role and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Role
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
# Role$unlock()
#
## Below is an example to define the print function
# Role$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Role$lock()

