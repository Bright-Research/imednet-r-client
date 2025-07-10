#' Create a new Coding
#'
#' @description
#' Coding Class
#'
#' @docType class
#' @title Coding
#' @description Coding Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field siteName Name of the site associated with the coded data character [optional]
#' @field siteId Site ID associated with the coded data integer [optional]
#' @field subjectId Subject ID associated with the coded data integer [optional]
#' @field subjectKey Subject key (display ID) associated with the coded data character [optional]
#' @field formId Form ID where the coded data originates integer [optional]
#' @field formName Name of the form where the coded data originates character [optional]
#' @field formKey Form key where the coded data originates character [optional]
#' @field revision Revision number of the coding entry integer [optional]
#' @field recordId Record ID associated with the coded data integer [optional]
#' @field variable Variable name (field) that was coded character [optional]
#' @field value Original value entered that required coding character [optional]
#' @field codingId Mednet coding ID integer [optional]
#' @field code Standardized code assigned (e.g., dictionary term) character [optional]
#' @field codedBy Name of the user who performed the coding character [optional]
#' @field reason Reason for coding or any notes on changes character [optional]
#' @field dictionaryName Name of the dictionary used (e.g., MedDRA) character [optional]
#' @field dictionaryVersion Version of the dictionary used character [optional]
#' @field dateCoded Date when the coding was performed character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Coding <- R6::R6Class(
  "Coding",
  public = list(
    `studyKey` = NULL,
    `siteName` = NULL,
    `siteId` = NULL,
    `subjectId` = NULL,
    `subjectKey` = NULL,
    `formId` = NULL,
    `formName` = NULL,
    `formKey` = NULL,
    `revision` = NULL,
    `recordId` = NULL,
    `variable` = NULL,
    `value` = NULL,
    `codingId` = NULL,
    `code` = NULL,
    `codedBy` = NULL,
    `reason` = NULL,
    `dictionaryName` = NULL,
    `dictionaryVersion` = NULL,
    `dateCoded` = NULL,

    #' @description
    #' Initialize a new Coding class.
    #'
    #' @param studyKey Unique study key
    #' @param siteName Name of the site associated with the coded data
    #' @param siteId Site ID associated with the coded data
    #' @param subjectId Subject ID associated with the coded data
    #' @param subjectKey Subject key (display ID) associated with the coded data
    #' @param formId Form ID where the coded data originates
    #' @param formName Name of the form where the coded data originates
    #' @param formKey Form key where the coded data originates
    #' @param revision Revision number of the coding entry
    #' @param recordId Record ID associated with the coded data
    #' @param variable Variable name (field) that was coded
    #' @param value Original value entered that required coding
    #' @param codingId Mednet coding ID
    #' @param code Standardized code assigned (e.g., dictionary term)
    #' @param codedBy Name of the user who performed the coding
    #' @param reason Reason for coding or any notes on changes
    #' @param dictionaryName Name of the dictionary used (e.g., MedDRA)
    #' @param dictionaryVersion Version of the dictionary used
    #' @param dateCoded Date when the coding was performed
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `siteName` = NULL, `siteId` = NULL, `subjectId` = NULL, `subjectKey` = NULL, `formId` = NULL, `formName` = NULL, `formKey` = NULL, `revision` = NULL, `recordId` = NULL, `variable` = NULL, `value` = NULL, `codingId` = NULL, `code` = NULL, `codedBy` = NULL, `reason` = NULL, `dictionaryName` = NULL, `dictionaryVersion` = NULL, `dateCoded` = NULL, ...) {
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
      }
      if (!is.null(`siteName`)) {
        if (!(is.character(`siteName`) && length(`siteName`) == 1)) {
          stop(paste("Error! Invalid data for `siteName`. Must be a string:", `siteName`))
        }
        self$`siteName` <- `siteName`
      }
      if (!is.null(`siteId`)) {
        if (!(is.numeric(`siteId`) && length(`siteId`) == 1)) {
          stop(paste("Error! Invalid data for `siteId`. Must be an integer:", `siteId`))
        }
        self$`siteId` <- `siteId`
      }
      if (!is.null(`subjectId`)) {
        if (!(is.numeric(`subjectId`) && length(`subjectId`) == 1)) {
          stop(paste("Error! Invalid data for `subjectId`. Must be an integer:", `subjectId`))
        }
        self$`subjectId` <- `subjectId`
      }
      if (!is.null(`subjectKey`)) {
        if (!(is.character(`subjectKey`) && length(`subjectKey`) == 1)) {
          stop(paste("Error! Invalid data for `subjectKey`. Must be a string:", `subjectKey`))
        }
        self$`subjectKey` <- `subjectKey`
      }
      if (!is.null(`formId`)) {
        if (!(is.numeric(`formId`) && length(`formId`) == 1)) {
          stop(paste("Error! Invalid data for `formId`. Must be an integer:", `formId`))
        }
        self$`formId` <- `formId`
      }
      if (!is.null(`formName`)) {
        if (!(is.character(`formName`) && length(`formName`) == 1)) {
          stop(paste("Error! Invalid data for `formName`. Must be a string:", `formName`))
        }
        self$`formName` <- `formName`
      }
      if (!is.null(`formKey`)) {
        if (!(is.character(`formKey`) && length(`formKey`) == 1)) {
          stop(paste("Error! Invalid data for `formKey`. Must be a string:", `formKey`))
        }
        self$`formKey` <- `formKey`
      }
      if (!is.null(`revision`)) {
        if (!(is.numeric(`revision`) && length(`revision`) == 1)) {
          stop(paste("Error! Invalid data for `revision`. Must be an integer:", `revision`))
        }
        self$`revision` <- `revision`
      }
      if (!is.null(`recordId`)) {
        if (!(is.numeric(`recordId`) && length(`recordId`) == 1)) {
          stop(paste("Error! Invalid data for `recordId`. Must be an integer:", `recordId`))
        }
        self$`recordId` <- `recordId`
      }
      if (!is.null(`variable`)) {
        if (!(is.character(`variable`) && length(`variable`) == 1)) {
          stop(paste("Error! Invalid data for `variable`. Must be a string:", `variable`))
        }
        self$`variable` <- `variable`
      }
      if (!is.null(`value`)) {
        if (!(is.character(`value`) && length(`value`) == 1)) {
          stop(paste("Error! Invalid data for `value`. Must be a string:", `value`))
        }
        self$`value` <- `value`
      }
      if (!is.null(`codingId`)) {
        if (!(is.numeric(`codingId`) && length(`codingId`) == 1)) {
          stop(paste("Error! Invalid data for `codingId`. Must be an integer:", `codingId`))
        }
        self$`codingId` <- `codingId`
      }
      if (!is.null(`code`)) {
        if (!(is.character(`code`) && length(`code`) == 1)) {
          stop(paste("Error! Invalid data for `code`. Must be a string:", `code`))
        }
        self$`code` <- `code`
      }
      if (!is.null(`codedBy`)) {
        if (!(is.character(`codedBy`) && length(`codedBy`) == 1)) {
          stop(paste("Error! Invalid data for `codedBy`. Must be a string:", `codedBy`))
        }
        self$`codedBy` <- `codedBy`
      }
      if (!is.null(`reason`)) {
        if (!(is.character(`reason`) && length(`reason`) == 1)) {
          stop(paste("Error! Invalid data for `reason`. Must be a string:", `reason`))
        }
        self$`reason` <- `reason`
      }
      if (!is.null(`dictionaryName`)) {
        if (!(is.character(`dictionaryName`) && length(`dictionaryName`) == 1)) {
          stop(paste("Error! Invalid data for `dictionaryName`. Must be a string:", `dictionaryName`))
        }
        self$`dictionaryName` <- `dictionaryName`
      }
      if (!is.null(`dictionaryVersion`)) {
        if (!(is.character(`dictionaryVersion`) && length(`dictionaryVersion`) == 1)) {
          stop(paste("Error! Invalid data for `dictionaryVersion`. Must be a string:", `dictionaryVersion`))
        }
        self$`dictionaryVersion` <- `dictionaryVersion`
      }
      if (!is.null(`dateCoded`)) {
        if (!(is.character(`dateCoded`) && length(`dateCoded`) == 1)) {
          stop(paste("Error! Invalid data for `dateCoded`. Must be a string:", `dateCoded`))
        }
        self$`dateCoded` <- `dateCoded`
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
    #' @return Coding as a base R list.
    #' @examples
    #' # convert array of Coding (x) to a data frame
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
    #' Convert Coding to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      CodingObject <- list()
      if (!is.null(self$`studyKey`)) {
        CodingObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`siteName`)) {
        CodingObject[["siteName"]] <-
          self$`siteName`
      }
      if (!is.null(self$`siteId`)) {
        CodingObject[["siteId"]] <-
          self$`siteId`
      }
      if (!is.null(self$`subjectId`)) {
        CodingObject[["subjectId"]] <-
          self$`subjectId`
      }
      if (!is.null(self$`subjectKey`)) {
        CodingObject[["subjectKey"]] <-
          self$`subjectKey`
      }
      if (!is.null(self$`formId`)) {
        CodingObject[["formId"]] <-
          self$`formId`
      }
      if (!is.null(self$`formName`)) {
        CodingObject[["formName"]] <-
          self$`formName`
      }
      if (!is.null(self$`formKey`)) {
        CodingObject[["formKey"]] <-
          self$`formKey`
      }
      if (!is.null(self$`revision`)) {
        CodingObject[["revision"]] <-
          self$`revision`
      }
      if (!is.null(self$`recordId`)) {
        CodingObject[["recordId"]] <-
          self$`recordId`
      }
      if (!is.null(self$`variable`)) {
        CodingObject[["variable"]] <-
          self$`variable`
      }
      if (!is.null(self$`value`)) {
        CodingObject[["value"]] <-
          self$`value`
      }
      if (!is.null(self$`codingId`)) {
        CodingObject[["codingId"]] <-
          self$`codingId`
      }
      if (!is.null(self$`code`)) {
        CodingObject[["code"]] <-
          self$`code`
      }
      if (!is.null(self$`codedBy`)) {
        CodingObject[["codedBy"]] <-
          self$`codedBy`
      }
      if (!is.null(self$`reason`)) {
        CodingObject[["reason"]] <-
          self$`reason`
      }
      if (!is.null(self$`dictionaryName`)) {
        CodingObject[["dictionaryName"]] <-
          self$`dictionaryName`
      }
      if (!is.null(self$`dictionaryVersion`)) {
        CodingObject[["dictionaryVersion"]] <-
          self$`dictionaryVersion`
      }
      if (!is.null(self$`dateCoded`)) {
        CodingObject[["dateCoded"]] <-
          self$`dateCoded`
      }
      return(CodingObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Coding
    #'
    #' @param input_json the JSON input
    #' @return the instance of Coding
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`siteName`)) {
        self$`siteName` <- this_object$`siteName`
      }
      if (!is.null(this_object$`siteId`)) {
        self$`siteId` <- this_object$`siteId`
      }
      if (!is.null(this_object$`subjectId`)) {
        self$`subjectId` <- this_object$`subjectId`
      }
      if (!is.null(this_object$`subjectKey`)) {
        self$`subjectKey` <- this_object$`subjectKey`
      }
      if (!is.null(this_object$`formId`)) {
        self$`formId` <- this_object$`formId`
      }
      if (!is.null(this_object$`formName`)) {
        self$`formName` <- this_object$`formName`
      }
      if (!is.null(this_object$`formKey`)) {
        self$`formKey` <- this_object$`formKey`
      }
      if (!is.null(this_object$`revision`)) {
        self$`revision` <- this_object$`revision`
      }
      if (!is.null(this_object$`recordId`)) {
        self$`recordId` <- this_object$`recordId`
      }
      if (!is.null(this_object$`variable`)) {
        self$`variable` <- this_object$`variable`
      }
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      if (!is.null(this_object$`codingId`)) {
        self$`codingId` <- this_object$`codingId`
      }
      if (!is.null(this_object$`code`)) {
        self$`code` <- this_object$`code`
      }
      if (!is.null(this_object$`codedBy`)) {
        self$`codedBy` <- this_object$`codedBy`
      }
      if (!is.null(this_object$`reason`)) {
        self$`reason` <- this_object$`reason`
      }
      if (!is.null(this_object$`dictionaryName`)) {
        self$`dictionaryName` <- this_object$`dictionaryName`
      }
      if (!is.null(this_object$`dictionaryVersion`)) {
        self$`dictionaryVersion` <- this_object$`dictionaryVersion`
      }
      if (!is.null(this_object$`dateCoded`)) {
        self$`dateCoded` <- this_object$`dateCoded`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Coding in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Coding
    #'
    #' @param input_json the JSON input
    #' @return the instance of Coding
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`siteName` <- this_object$`siteName`
      self$`siteId` <- this_object$`siteId`
      self$`subjectId` <- this_object$`subjectId`
      self$`subjectKey` <- this_object$`subjectKey`
      self$`formId` <- this_object$`formId`
      self$`formName` <- this_object$`formName`
      self$`formKey` <- this_object$`formKey`
      self$`revision` <- this_object$`revision`
      self$`recordId` <- this_object$`recordId`
      self$`variable` <- this_object$`variable`
      self$`value` <- this_object$`value`
      self$`codingId` <- this_object$`codingId`
      self$`code` <- this_object$`code`
      self$`codedBy` <- this_object$`codedBy`
      self$`reason` <- this_object$`reason`
      self$`dictionaryName` <- this_object$`dictionaryName`
      self$`dictionaryVersion` <- this_object$`dictionaryVersion`
      self$`dateCoded` <- this_object$`dateCoded`
      self
    },

    #' @description
    #' Validate JSON input with respect to Coding and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Coding
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
# Coding$unlock()
#
## Below is an example to define the print function
# Coding$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Coding$lock()

