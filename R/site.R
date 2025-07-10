#' Create a new Site
#'
#' @description
#' Site Class
#'
#' @docType class
#' @title Site
#' @description Site Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field siteId Unique site ID integer [optional]
#' @field siteName Name of the site character [optional]
#' @field siteEnrollmentStatus Enrollment status of the site character [optional]
#' @field dateCreated Date when this site was created character [optional]
#' @field dateModified Date when this site was last modified character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Site <- R6::R6Class(
  "Site",
  public = list(
    `studyKey` = NULL,
    `siteId` = NULL,
    `siteName` = NULL,
    `siteEnrollmentStatus` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,

    #' @description
    #' Initialize a new Site class.
    #'
    #' @param studyKey Unique study key
    #' @param siteId Unique site ID
    #' @param siteName Name of the site
    #' @param siteEnrollmentStatus Enrollment status of the site
    #' @param dateCreated Date when this site was created
    #' @param dateModified Date when this site was last modified
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `siteId` = NULL, `siteName` = NULL, `siteEnrollmentStatus` = NULL, `dateCreated` = NULL, `dateModified` = NULL, ...) {
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
      }
      if (!is.null(`siteId`)) {
        if (!(is.numeric(`siteId`) && length(`siteId`) == 1)) {
          stop(paste("Error! Invalid data for `siteId`. Must be an integer:", `siteId`))
        }
        self$`siteId` <- `siteId`
      }
      if (!is.null(`siteName`)) {
        if (!(is.character(`siteName`) && length(`siteName`) == 1)) {
          stop(paste("Error! Invalid data for `siteName`. Must be a string:", `siteName`))
        }
        self$`siteName` <- `siteName`
      }
      if (!is.null(`siteEnrollmentStatus`)) {
        if (!(is.character(`siteEnrollmentStatus`) && length(`siteEnrollmentStatus`) == 1)) {
          stop(paste("Error! Invalid data for `siteEnrollmentStatus`. Must be a string:", `siteEnrollmentStatus`))
        }
        self$`siteEnrollmentStatus` <- `siteEnrollmentStatus`
      }
      if (!is.null(`dateCreated`)) {
        if (!(is.character(`dateCreated`) && length(`dateCreated`) == 1)) {
          stop(paste("Error! Invalid data for `dateCreated`. Must be a string:", `dateCreated`))
        }
        self$`dateCreated` <- `dateCreated`
      }
      if (!is.null(`dateModified`)) {
        if (!(is.character(`dateModified`) && length(`dateModified`) == 1)) {
          stop(paste("Error! Invalid data for `dateModified`. Must be a string:", `dateModified`))
        }
        self$`dateModified` <- `dateModified`
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
    #' @return Site as a base R list.
    #' @examples
    #' # convert array of Site (x) to a data frame
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
    #' Convert Site to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      SiteObject <- list()
      if (!is.null(self$`studyKey`)) {
        SiteObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`siteId`)) {
        SiteObject[["siteId"]] <-
          self$`siteId`
      }
      if (!is.null(self$`siteName`)) {
        SiteObject[["siteName"]] <-
          self$`siteName`
      }
      if (!is.null(self$`siteEnrollmentStatus`)) {
        SiteObject[["siteEnrollmentStatus"]] <-
          self$`siteEnrollmentStatus`
      }
      if (!is.null(self$`dateCreated`)) {
        SiteObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        SiteObject[["dateModified"]] <-
          self$`dateModified`
      }
      return(SiteObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Site
    #'
    #' @param input_json the JSON input
    #' @return the instance of Site
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`siteId`)) {
        self$`siteId` <- this_object$`siteId`
      }
      if (!is.null(this_object$`siteName`)) {
        self$`siteName` <- this_object$`siteName`
      }
      if (!is.null(this_object$`siteEnrollmentStatus`)) {
        self$`siteEnrollmentStatus` <- this_object$`siteEnrollmentStatus`
      }
      if (!is.null(this_object$`dateCreated`)) {
        self$`dateCreated` <- this_object$`dateCreated`
      }
      if (!is.null(this_object$`dateModified`)) {
        self$`dateModified` <- this_object$`dateModified`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Site in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Site
    #'
    #' @param input_json the JSON input
    #' @return the instance of Site
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`siteId` <- this_object$`siteId`
      self$`siteName` <- this_object$`siteName`
      self$`siteEnrollmentStatus` <- this_object$`siteEnrollmentStatus`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self
    },

    #' @description
    #' Validate JSON input with respect to Site and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Site
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
# Site$unlock()
#
## Below is an example to define the print function
# Site$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Site$lock()

