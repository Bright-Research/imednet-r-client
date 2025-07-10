#' Create a new Record
#'
#' @description
#' Record Class
#'
#' @docType class
#' @title Record
#' @description Record Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field intervalId Interval ID (visit definition) that this record is associated with integer [optional]
#' @field formId Form ID of the form this record instance belongs to integer [optional]
#' @field formKey Form key of the form for this record instance character [optional]
#' @field siteId Site ID associated with the record integer [optional]
#' @field recordId Unique record ID integer [optional]
#' @field recordOid Client-assigned record OID character [optional]
#' @field recordType Type of record (e.g., SUBJECT for subject-related forms) character [optional]
#' @field recordStatus Current status of the record (e.g., Record Incomplete, Record Complete) character [optional]
#' @field deleted Whether the record is marked as deleted character [optional]
#' @field dateCreated Date when this record was created character [optional]
#' @field dateModified Date when this record was last modified character [optional]
#' @field subjectId Mednet subject ID that this record is associated with integer [optional]
#' @field subjectOid Client-assigned subject OID for the subject this record is associated with character [optional]
#' @field subjectKey Subject display ID that this record is associated with character [optional]
#' @field visitId Visit instance ID that this record is associated with (if applicable) integer [optional]
#' @field parentRecordId Record ID of the parent record if this record is a subrecord (if applicable) integer [optional]
#' @field keywords List of keywords associated with the record list(\link{Keyword}) [optional]
#' @field recordData Data fields and values captured in this record. The structure is dynamic, with keys corresponding to variable names. named list(\link{AnyType}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Record <- R6::R6Class(
  "Record",
  public = list(
    `studyKey` = NULL,
    `intervalId` = NULL,
    `formId` = NULL,
    `formKey` = NULL,
    `siteId` = NULL,
    `recordId` = NULL,
    `recordOid` = NULL,
    `recordType` = NULL,
    `recordStatus` = NULL,
    `deleted` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,
    `subjectId` = NULL,
    `subjectOid` = NULL,
    `subjectKey` = NULL,
    `visitId` = NULL,
    `parentRecordId` = NULL,
    `keywords` = NULL,
    `recordData` = NULL,

    #' @description
    #' Initialize a new Record class.
    #'
    #' @param studyKey Unique study key
    #' @param intervalId Interval ID (visit definition) that this record is associated with
    #' @param formId Form ID of the form this record instance belongs to
    #' @param formKey Form key of the form for this record instance
    #' @param siteId Site ID associated with the record
    #' @param recordId Unique record ID
    #' @param recordOid Client-assigned record OID
    #' @param recordType Type of record (e.g., SUBJECT for subject-related forms)
    #' @param recordStatus Current status of the record (e.g., Record Incomplete, Record Complete)
    #' @param deleted Whether the record is marked as deleted
    #' @param dateCreated Date when this record was created
    #' @param dateModified Date when this record was last modified
    #' @param subjectId Mednet subject ID that this record is associated with
    #' @param subjectOid Client-assigned subject OID for the subject this record is associated with
    #' @param subjectKey Subject display ID that this record is associated with
    #' @param visitId Visit instance ID that this record is associated with (if applicable)
    #' @param parentRecordId Record ID of the parent record if this record is a subrecord (if applicable)
    #' @param keywords List of keywords associated with the record
    #' @param recordData Data fields and values captured in this record. The structure is dynamic, with keys corresponding to variable names.
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `intervalId` = NULL, `formId` = NULL, `formKey` = NULL, `siteId` = NULL, `recordId` = NULL, `recordOid` = NULL, `recordType` = NULL, `recordStatus` = NULL, `deleted` = NULL, `dateCreated` = NULL, `dateModified` = NULL, `subjectId` = NULL, `subjectOid` = NULL, `subjectKey` = NULL, `visitId` = NULL, `parentRecordId` = NULL, `keywords` = NULL, `recordData` = NULL, ...) {
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
      }
      if (!is.null(`intervalId`)) {
        if (!(is.numeric(`intervalId`) && length(`intervalId`) == 1)) {
          stop(paste("Error! Invalid data for `intervalId`. Must be an integer:", `intervalId`))
        }
        self$`intervalId` <- `intervalId`
      }
      if (!is.null(`formId`)) {
        if (!(is.numeric(`formId`) && length(`formId`) == 1)) {
          stop(paste("Error! Invalid data for `formId`. Must be an integer:", `formId`))
        }
        self$`formId` <- `formId`
      }
      if (!is.null(`formKey`)) {
        if (!(is.character(`formKey`) && length(`formKey`) == 1)) {
          stop(paste("Error! Invalid data for `formKey`. Must be a string:", `formKey`))
        }
        self$`formKey` <- `formKey`
      }
      if (!is.null(`siteId`)) {
        if (!(is.numeric(`siteId`) && length(`siteId`) == 1)) {
          stop(paste("Error! Invalid data for `siteId`. Must be an integer:", `siteId`))
        }
        self$`siteId` <- `siteId`
      }
      if (!is.null(`recordId`)) {
        if (!(is.numeric(`recordId`) && length(`recordId`) == 1)) {
          stop(paste("Error! Invalid data for `recordId`. Must be an integer:", `recordId`))
        }
        self$`recordId` <- `recordId`
      }
      if (!is.null(`recordOid`)) {
        if (!(is.character(`recordOid`) && length(`recordOid`) == 1)) {
          stop(paste("Error! Invalid data for `recordOid`. Must be a string:", `recordOid`))
        }
        self$`recordOid` <- `recordOid`
      }
      if (!is.null(`recordType`)) {
        if (!(is.character(`recordType`) && length(`recordType`) == 1)) {
          stop(paste("Error! Invalid data for `recordType`. Must be a string:", `recordType`))
        }
        self$`recordType` <- `recordType`
      }
      if (!is.null(`recordStatus`)) {
        if (!(is.character(`recordStatus`) && length(`recordStatus`) == 1)) {
          stop(paste("Error! Invalid data for `recordStatus`. Must be a string:", `recordStatus`))
        }
        self$`recordStatus` <- `recordStatus`
      }
      if (!is.null(`deleted`)) {
        if (!(is.logical(`deleted`) && length(`deleted`) == 1)) {
          stop(paste("Error! Invalid data for `deleted`. Must be a boolean:", `deleted`))
        }
        self$`deleted` <- `deleted`
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
      if (!is.null(`visitId`)) {
        if (!(is.numeric(`visitId`) && length(`visitId`) == 1)) {
          stop(paste("Error! Invalid data for `visitId`. Must be an integer:", `visitId`))
        }
        self$`visitId` <- `visitId`
      }
      if (!is.null(`parentRecordId`)) {
        if (!(is.numeric(`parentRecordId`) && length(`parentRecordId`) == 1)) {
          stop(paste("Error! Invalid data for `parentRecordId`. Must be an integer:", `parentRecordId`))
        }
        self$`parentRecordId` <- `parentRecordId`
      }
      if (!is.null(`keywords`)) {
        stopifnot(is.vector(`keywords`), length(`keywords`) != 0)
        sapply(`keywords`, function(x) stopifnot(R6::is.R6(x)))
        self$`keywords` <- `keywords`
      }
      if (!is.null(`recordData`)) {
        stopifnot(is.vector(`recordData`), length(`recordData`) != 0)
        sapply(`recordData`, function(x) stopifnot(R6::is.R6(x)))
        self$`recordData` <- `recordData`
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
    #' @return Record as a base R list.
    #' @examples
    #' # convert array of Record (x) to a data frame
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
    #' Convert Record to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      RecordObject <- list()
      if (!is.null(self$`studyKey`)) {
        RecordObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`intervalId`)) {
        RecordObject[["intervalId"]] <-
          self$`intervalId`
      }
      if (!is.null(self$`formId`)) {
        RecordObject[["formId"]] <-
          self$`formId`
      }
      if (!is.null(self$`formKey`)) {
        RecordObject[["formKey"]] <-
          self$`formKey`
      }
      if (!is.null(self$`siteId`)) {
        RecordObject[["siteId"]] <-
          self$`siteId`
      }
      if (!is.null(self$`recordId`)) {
        RecordObject[["recordId"]] <-
          self$`recordId`
      }
      if (!is.null(self$`recordOid`)) {
        RecordObject[["recordOid"]] <-
          self$`recordOid`
      }
      if (!is.null(self$`recordType`)) {
        RecordObject[["recordType"]] <-
          self$`recordType`
      }
      if (!is.null(self$`recordStatus`)) {
        RecordObject[["recordStatus"]] <-
          self$`recordStatus`
      }
      if (!is.null(self$`deleted`)) {
        RecordObject[["deleted"]] <-
          self$`deleted`
      }
      if (!is.null(self$`dateCreated`)) {
        RecordObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        RecordObject[["dateModified"]] <-
          self$`dateModified`
      }
      if (!is.null(self$`subjectId`)) {
        RecordObject[["subjectId"]] <-
          self$`subjectId`
      }
      if (!is.null(self$`subjectOid`)) {
        RecordObject[["subjectOid"]] <-
          self$`subjectOid`
      }
      if (!is.null(self$`subjectKey`)) {
        RecordObject[["subjectKey"]] <-
          self$`subjectKey`
      }
      if (!is.null(self$`visitId`)) {
        RecordObject[["visitId"]] <-
          self$`visitId`
      }
      if (!is.null(self$`parentRecordId`)) {
        RecordObject[["parentRecordId"]] <-
          self$`parentRecordId`
      }
      if (!is.null(self$`keywords`)) {
        RecordObject[["keywords"]] <-
          lapply(self$`keywords`, function(x) x$toSimpleType())
      }
      if (!is.null(self$`recordData`)) {
        RecordObject[["recordData"]] <-
          lapply(self$`recordData`, function(x) x$toSimpleType())
      }
      return(RecordObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Record
    #'
    #' @param input_json the JSON input
    #' @return the instance of Record
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`intervalId`)) {
        self$`intervalId` <- this_object$`intervalId`
      }
      if (!is.null(this_object$`formId`)) {
        self$`formId` <- this_object$`formId`
      }
      if (!is.null(this_object$`formKey`)) {
        self$`formKey` <- this_object$`formKey`
      }
      if (!is.null(this_object$`siteId`)) {
        self$`siteId` <- this_object$`siteId`
      }
      if (!is.null(this_object$`recordId`)) {
        self$`recordId` <- this_object$`recordId`
      }
      if (!is.null(this_object$`recordOid`)) {
        self$`recordOid` <- this_object$`recordOid`
      }
      if (!is.null(this_object$`recordType`)) {
        self$`recordType` <- this_object$`recordType`
      }
      if (!is.null(this_object$`recordStatus`)) {
        self$`recordStatus` <- this_object$`recordStatus`
      }
      if (!is.null(this_object$`deleted`)) {
        self$`deleted` <- this_object$`deleted`
      }
      if (!is.null(this_object$`dateCreated`)) {
        self$`dateCreated` <- this_object$`dateCreated`
      }
      if (!is.null(this_object$`dateModified`)) {
        self$`dateModified` <- this_object$`dateModified`
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
      if (!is.null(this_object$`visitId`)) {
        self$`visitId` <- this_object$`visitId`
      }
      if (!is.null(this_object$`parentRecordId`)) {
        self$`parentRecordId` <- this_object$`parentRecordId`
      }
      if (!is.null(this_object$`keywords`)) {
        self$`keywords` <- ApiClient$new()$deserializeObj(this_object$`keywords`, "array[Keyword]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`recordData`)) {
        self$`recordData` <- ApiClient$new()$deserializeObj(this_object$`recordData`, "map(AnyType)", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Record in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Record
    #'
    #' @param input_json the JSON input
    #' @return the instance of Record
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`intervalId` <- this_object$`intervalId`
      self$`formId` <- this_object$`formId`
      self$`formKey` <- this_object$`formKey`
      self$`siteId` <- this_object$`siteId`
      self$`recordId` <- this_object$`recordId`
      self$`recordOid` <- this_object$`recordOid`
      self$`recordType` <- this_object$`recordType`
      self$`recordStatus` <- this_object$`recordStatus`
      self$`deleted` <- this_object$`deleted`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self$`subjectId` <- this_object$`subjectId`
      self$`subjectOid` <- this_object$`subjectOid`
      self$`subjectKey` <- this_object$`subjectKey`
      self$`visitId` <- this_object$`visitId`
      self$`parentRecordId` <- this_object$`parentRecordId`
      self$`keywords` <- ApiClient$new()$deserializeObj(this_object$`keywords`, "array[Keyword]", loadNamespace("openapi"))
      self$`recordData` <- ApiClient$new()$deserializeObj(this_object$`recordData`, "map(AnyType)", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to Record and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Record
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
# Record$unlock()
#
## Below is an example to define the print function
# Record$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Record$lock()

