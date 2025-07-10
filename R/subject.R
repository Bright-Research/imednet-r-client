#' Create a new Subject
#'
#' @description
#' Subject Class
#'
#' @docType class
#' @title Subject
#' @description Subject Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field subjectId Mednet subject ID (internal numeric ID) integer [optional]
#' @field subjectOid Client-assigned subject object identifier (OID) character [optional]
#' @field subjectKey Protocol-assigned subject identifier (display ID) character [optional]
#' @field subjectStatus Current status of the subject (e.g., Enrolled) character [optional]
#' @field siteId Mednet site ID the subject is associated with integer [optional]
#' @field siteName Name of the site the subject is associated with character [optional]
#' @field deleted Whether the subject is marked as deleted character [optional]
#' @field enrollmentStartDate Subject’s enrollment start date character [optional]
#' @field dateCreated Date when this subject record was created character [optional]
#' @field dateModified Date when this subject record was last modified character [optional]
#' @field keywords List of keywords associated with the subject list(\link{Keyword}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Subject <- R6::R6Class(
  "Subject",
  public = list(
    `studyKey` = NULL,
    `subjectId` = NULL,
    `subjectOid` = NULL,
    `subjectKey` = NULL,
    `subjectStatus` = NULL,
    `siteId` = NULL,
    `siteName` = NULL,
    `deleted` = NULL,
    `enrollmentStartDate` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,
    `keywords` = NULL,

    #' @description
    #' Initialize a new Subject class.
    #'
    #' @param studyKey Unique study key
    #' @param subjectId Mednet subject ID (internal numeric ID)
    #' @param subjectOid Client-assigned subject object identifier (OID)
    #' @param subjectKey Protocol-assigned subject identifier (display ID)
    #' @param subjectStatus Current status of the subject (e.g., Enrolled)
    #' @param siteId Mednet site ID the subject is associated with
    #' @param siteName Name of the site the subject is associated with
    #' @param deleted Whether the subject is marked as deleted
    #' @param enrollmentStartDate Subject’s enrollment start date
    #' @param dateCreated Date when this subject record was created
    #' @param dateModified Date when this subject record was last modified
    #' @param keywords List of keywords associated with the subject
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `subjectId` = NULL, `subjectOid` = NULL, `subjectKey` = NULL, `subjectStatus` = NULL, `siteId` = NULL, `siteName` = NULL, `deleted` = NULL, `enrollmentStartDate` = NULL, `dateCreated` = NULL, `dateModified` = NULL, `keywords` = NULL, ...) {
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
      }
      if (!is.null(`subjectId`)) {
        if (!(is.numeric(`subjectId`) && length(`subjectId`) == 1)) {
          stop(paste("Error! Invalid data for `subjectId`. Must be an integer:", `subjectId`))
        }
        self$`subjectId` <- `subjectId`
      }
      if (!is.null(`subjectOid`)) {
        if (!(is.character(`subjectOid`) && length(`subjectOid`) == 1)) {
          stop(paste("Error! Invalid data for `subjectOid`. Must be a string:", `subjectOid`))
        }
        self$`subjectOid` <- `subjectOid`
      }
      if (!is.null(`subjectKey`)) {
        if (!(is.character(`subjectKey`) && length(`subjectKey`) == 1)) {
          stop(paste("Error! Invalid data for `subjectKey`. Must be a string:", `subjectKey`))
        }
        self$`subjectKey` <- `subjectKey`
      }
      if (!is.null(`subjectStatus`)) {
        if (!(is.character(`subjectStatus`) && length(`subjectStatus`) == 1)) {
          stop(paste("Error! Invalid data for `subjectStatus`. Must be a string:", `subjectStatus`))
        }
        self$`subjectStatus` <- `subjectStatus`
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
      if (!is.null(`deleted`)) {
        if (!(is.logical(`deleted`) && length(`deleted`) == 1)) {
          stop(paste("Error! Invalid data for `deleted`. Must be a boolean:", `deleted`))
        }
        self$`deleted` <- `deleted`
      }
      if (!is.null(`enrollmentStartDate`)) {
        if (!(is.character(`enrollmentStartDate`) && length(`enrollmentStartDate`) == 1)) {
          stop(paste("Error! Invalid data for `enrollmentStartDate`. Must be a string:", `enrollmentStartDate`))
        }
        self$`enrollmentStartDate` <- `enrollmentStartDate`
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
      if (!is.null(`keywords`)) {
        stopifnot(is.vector(`keywords`), length(`keywords`) != 0)
        sapply(`keywords`, function(x) stopifnot(R6::is.R6(x)))
        self$`keywords` <- `keywords`
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
    #' @return Subject as a base R list.
    #' @examples
    #' # convert array of Subject (x) to a data frame
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
    #' Convert Subject to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      SubjectObject <- list()
      if (!is.null(self$`studyKey`)) {
        SubjectObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`subjectId`)) {
        SubjectObject[["subjectId"]] <-
          self$`subjectId`
      }
      if (!is.null(self$`subjectOid`)) {
        SubjectObject[["subjectOid"]] <-
          self$`subjectOid`
      }
      if (!is.null(self$`subjectKey`)) {
        SubjectObject[["subjectKey"]] <-
          self$`subjectKey`
      }
      if (!is.null(self$`subjectStatus`)) {
        SubjectObject[["subjectStatus"]] <-
          self$`subjectStatus`
      }
      if (!is.null(self$`siteId`)) {
        SubjectObject[["siteId"]] <-
          self$`siteId`
      }
      if (!is.null(self$`siteName`)) {
        SubjectObject[["siteName"]] <-
          self$`siteName`
      }
      if (!is.null(self$`deleted`)) {
        SubjectObject[["deleted"]] <-
          self$`deleted`
      }
      if (!is.null(self$`enrollmentStartDate`)) {
        SubjectObject[["enrollmentStartDate"]] <-
          self$`enrollmentStartDate`
      }
      if (!is.null(self$`dateCreated`)) {
        SubjectObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        SubjectObject[["dateModified"]] <-
          self$`dateModified`
      }
      if (!is.null(self$`keywords`)) {
        SubjectObject[["keywords"]] <-
          lapply(self$`keywords`, function(x) x$toSimpleType())
      }
      return(SubjectObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Subject
    #'
    #' @param input_json the JSON input
    #' @return the instance of Subject
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`subjectId`)) {
        self$`subjectId` <- this_object$`subjectId`
      }
      if (!is.null(this_object$`subjectOid`)) {
        self$`subjectOid` <- this_object$`subjectOid`
      }
      if (!is.null(this_object$`subjectKey`)) {
        self$`subjectKey` <- this_object$`subjectKey`
      }
      if (!is.null(this_object$`subjectStatus`)) {
        self$`subjectStatus` <- this_object$`subjectStatus`
      }
      if (!is.null(this_object$`siteId`)) {
        self$`siteId` <- this_object$`siteId`
      }
      if (!is.null(this_object$`siteName`)) {
        self$`siteName` <- this_object$`siteName`
      }
      if (!is.null(this_object$`deleted`)) {
        self$`deleted` <- this_object$`deleted`
      }
      if (!is.null(this_object$`enrollmentStartDate`)) {
        self$`enrollmentStartDate` <- this_object$`enrollmentStartDate`
      }
      if (!is.null(this_object$`dateCreated`)) {
        self$`dateCreated` <- this_object$`dateCreated`
      }
      if (!is.null(this_object$`dateModified`)) {
        self$`dateModified` <- this_object$`dateModified`
      }
      if (!is.null(this_object$`keywords`)) {
        self$`keywords` <- ApiClient$new()$deserializeObj(this_object$`keywords`, "array[Keyword]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Subject in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Subject
    #'
    #' @param input_json the JSON input
    #' @return the instance of Subject
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`subjectId` <- this_object$`subjectId`
      self$`subjectOid` <- this_object$`subjectOid`
      self$`subjectKey` <- this_object$`subjectKey`
      self$`subjectStatus` <- this_object$`subjectStatus`
      self$`siteId` <- this_object$`siteId`
      self$`siteName` <- this_object$`siteName`
      self$`deleted` <- this_object$`deleted`
      self$`enrollmentStartDate` <- this_object$`enrollmentStartDate`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self$`keywords` <- ApiClient$new()$deserializeObj(this_object$`keywords`, "array[Keyword]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to Subject and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Subject
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
# Subject$unlock()
#
## Below is an example to define the print function
# Subject$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Subject$lock()

