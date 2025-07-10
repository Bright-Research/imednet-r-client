#' Create a new Study
#'
#' @description
#' Study Class
#'
#' @docType class
#' @title Study
#' @description Study Class
#' @format An \code{R6Class} generator object
#' @field sponsorKey Sponsor key that this study belongs to character [optional]
#' @field studyKey Unique study key character [optional]
#' @field studyId Mednet study ID (internal numeric identifier) integer [optional]
#' @field studyName Name of the study character [optional]
#' @field studyDescription Description of the study character [optional]
#' @field studyType Type of study (e.g., STUDY) character [optional]
#' @field dateCreated Date when the study record was created character [optional]
#' @field dateModified Date when the study record was last modified character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Study <- R6::R6Class(
  "Study",
  public = list(
    `sponsorKey` = NULL,
    `studyKey` = NULL,
    `studyId` = NULL,
    `studyName` = NULL,
    `studyDescription` = NULL,
    `studyType` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,

    #' @description
    #' Initialize a new Study class.
    #'
    #' @param sponsorKey Sponsor key that this study belongs to
    #' @param studyKey Unique study key
    #' @param studyId Mednet study ID (internal numeric identifier)
    #' @param studyName Name of the study
    #' @param studyDescription Description of the study
    #' @param studyType Type of study (e.g., STUDY)
    #' @param dateCreated Date when the study record was created
    #' @param dateModified Date when the study record was last modified
    #' @param ... Other optional arguments.
    initialize = function(`sponsorKey` = NULL, `studyKey` = NULL, `studyId` = NULL, `studyName` = NULL, `studyDescription` = NULL, `studyType` = NULL, `dateCreated` = NULL, `dateModified` = NULL, ...) {
      if (!is.null(`sponsorKey`)) {
        if (!(is.character(`sponsorKey`) && length(`sponsorKey`) == 1)) {
          stop(paste("Error! Invalid data for `sponsorKey`. Must be a string:", `sponsorKey`))
        }
        self$`sponsorKey` <- `sponsorKey`
      }
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
      }
      if (!is.null(`studyId`)) {
        if (!(is.numeric(`studyId`) && length(`studyId`) == 1)) {
          stop(paste("Error! Invalid data for `studyId`. Must be an integer:", `studyId`))
        }
        self$`studyId` <- `studyId`
      }
      if (!is.null(`studyName`)) {
        if (!(is.character(`studyName`) && length(`studyName`) == 1)) {
          stop(paste("Error! Invalid data for `studyName`. Must be a string:", `studyName`))
        }
        self$`studyName` <- `studyName`
      }
      if (!is.null(`studyDescription`)) {
        if (!(is.character(`studyDescription`) && length(`studyDescription`) == 1)) {
          stop(paste("Error! Invalid data for `studyDescription`. Must be a string:", `studyDescription`))
        }
        self$`studyDescription` <- `studyDescription`
      }
      if (!is.null(`studyType`)) {
        if (!(is.character(`studyType`) && length(`studyType`) == 1)) {
          stop(paste("Error! Invalid data for `studyType`. Must be a string:", `studyType`))
        }
        self$`studyType` <- `studyType`
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
    #' @return Study as a base R list.
    #' @examples
    #' # convert array of Study (x) to a data frame
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
    #' Convert Study to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      StudyObject <- list()
      if (!is.null(self$`sponsorKey`)) {
        StudyObject[["sponsorKey"]] <-
          self$`sponsorKey`
      }
      if (!is.null(self$`studyKey`)) {
        StudyObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`studyId`)) {
        StudyObject[["studyId"]] <-
          self$`studyId`
      }
      if (!is.null(self$`studyName`)) {
        StudyObject[["studyName"]] <-
          self$`studyName`
      }
      if (!is.null(self$`studyDescription`)) {
        StudyObject[["studyDescription"]] <-
          self$`studyDescription`
      }
      if (!is.null(self$`studyType`)) {
        StudyObject[["studyType"]] <-
          self$`studyType`
      }
      if (!is.null(self$`dateCreated`)) {
        StudyObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        StudyObject[["dateModified"]] <-
          self$`dateModified`
      }
      return(StudyObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Study
    #'
    #' @param input_json the JSON input
    #' @return the instance of Study
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`sponsorKey`)) {
        self$`sponsorKey` <- this_object$`sponsorKey`
      }
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`studyId`)) {
        self$`studyId` <- this_object$`studyId`
      }
      if (!is.null(this_object$`studyName`)) {
        self$`studyName` <- this_object$`studyName`
      }
      if (!is.null(this_object$`studyDescription`)) {
        self$`studyDescription` <- this_object$`studyDescription`
      }
      if (!is.null(this_object$`studyType`)) {
        self$`studyType` <- this_object$`studyType`
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
    #' @return Study in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Study
    #'
    #' @param input_json the JSON input
    #' @return the instance of Study
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`sponsorKey` <- this_object$`sponsorKey`
      self$`studyKey` <- this_object$`studyKey`
      self$`studyId` <- this_object$`studyId`
      self$`studyName` <- this_object$`studyName`
      self$`studyDescription` <- this_object$`studyDescription`
      self$`studyType` <- this_object$`studyType`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self
    },

    #' @description
    #' Validate JSON input with respect to Study and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Study
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
# Study$unlock()
#
## Below is an example to define the print function
# Study$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Study$lock()

