#' Create a new RecordRevision
#'
#' @description
#' RecordRevision Class
#'
#' @docType class
#' @title RecordRevision
#' @description RecordRevision Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field recordRevisionId Unique system identifier for the record revision integer [optional]
#' @field recordId Unique system identifier of the related record integer [optional]
#' @field recordOid Client-assigned record OID (if any) character [optional]
#' @field recordRevision Revision number of the record (version of the record status) integer [optional]
#' @field dataRevision Revision number of the data within the record integer [optional]
#' @field recordStatus Status of the record at this revision (user-defined status label) character [optional]
#' @field subjectId Mednet subject ID related to the record integer [optional]
#' @field subjectOid Client-assigned subject OID related to the record character [optional]
#' @field subjectKey Subject display ID related to the record character [optional]
#' @field siteId Site ID related to the record integer [optional]
#' @field formKey Form key of the form that the record belongs to character [optional]
#' @field intervalId Interval ID (visit definition) related to the record integer [optional]
#' @field role Role name of the user who saved the record revision character [optional]
#' @field user Username of the user who saved the record revision character [optional]
#' @field reasonForChange Reason for change, if provided (for audit trail) character [optional]
#' @field deleted Whether the record was deleted in this revision character [optional]
#' @field dateCreated Date when this record revision was created character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
RecordRevision <- R6::R6Class(
  "RecordRevision",
  public = list(
    `studyKey` = NULL,
    `recordRevisionId` = NULL,
    `recordId` = NULL,
    `recordOid` = NULL,
    `recordRevision` = NULL,
    `dataRevision` = NULL,
    `recordStatus` = NULL,
    `subjectId` = NULL,
    `subjectOid` = NULL,
    `subjectKey` = NULL,
    `siteId` = NULL,
    `formKey` = NULL,
    `intervalId` = NULL,
    `role` = NULL,
    `user` = NULL,
    `reasonForChange` = NULL,
    `deleted` = NULL,
    `dateCreated` = NULL,

    #' @description
    #' Initialize a new RecordRevision class.
    #'
    #' @param studyKey Unique study key
    #' @param recordRevisionId Unique system identifier for the record revision
    #' @param recordId Unique system identifier of the related record
    #' @param recordOid Client-assigned record OID (if any)
    #' @param recordRevision Revision number of the record (version of the record status)
    #' @param dataRevision Revision number of the data within the record
    #' @param recordStatus Status of the record at this revision (user-defined status label)
    #' @param subjectId Mednet subject ID related to the record
    #' @param subjectOid Client-assigned subject OID related to the record
    #' @param subjectKey Subject display ID related to the record
    #' @param siteId Site ID related to the record
    #' @param formKey Form key of the form that the record belongs to
    #' @param intervalId Interval ID (visit definition) related to the record
    #' @param role Role name of the user who saved the record revision
    #' @param user Username of the user who saved the record revision
    #' @param reasonForChange Reason for change, if provided (for audit trail)
    #' @param deleted Whether the record was deleted in this revision
    #' @param dateCreated Date when this record revision was created
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `recordRevisionId` = NULL, `recordId` = NULL, `recordOid` = NULL, `recordRevision` = NULL, `dataRevision` = NULL, `recordStatus` = NULL, `subjectId` = NULL, `subjectOid` = NULL, `subjectKey` = NULL, `siteId` = NULL, `formKey` = NULL, `intervalId` = NULL, `role` = NULL, `user` = NULL, `reasonForChange` = NULL, `deleted` = NULL, `dateCreated` = NULL, ...) {
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
      }
      if (!is.null(`recordRevisionId`)) {
        if (!(is.numeric(`recordRevisionId`) && length(`recordRevisionId`) == 1)) {
          stop(paste("Error! Invalid data for `recordRevisionId`. Must be an integer:", `recordRevisionId`))
        }
        self$`recordRevisionId` <- `recordRevisionId`
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
      if (!is.null(`recordRevision`)) {
        if (!(is.numeric(`recordRevision`) && length(`recordRevision`) == 1)) {
          stop(paste("Error! Invalid data for `recordRevision`. Must be an integer:", `recordRevision`))
        }
        self$`recordRevision` <- `recordRevision`
      }
      if (!is.null(`dataRevision`)) {
        if (!(is.numeric(`dataRevision`) && length(`dataRevision`) == 1)) {
          stop(paste("Error! Invalid data for `dataRevision`. Must be an integer:", `dataRevision`))
        }
        self$`dataRevision` <- `dataRevision`
      }
      if (!is.null(`recordStatus`)) {
        if (!(is.character(`recordStatus`) && length(`recordStatus`) == 1)) {
          stop(paste("Error! Invalid data for `recordStatus`. Must be a string:", `recordStatus`))
        }
        self$`recordStatus` <- `recordStatus`
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
      if (!is.null(`siteId`)) {
        if (!(is.numeric(`siteId`) && length(`siteId`) == 1)) {
          stop(paste("Error! Invalid data for `siteId`. Must be an integer:", `siteId`))
        }
        self$`siteId` <- `siteId`
      }
      if (!is.null(`formKey`)) {
        if (!(is.character(`formKey`) && length(`formKey`) == 1)) {
          stop(paste("Error! Invalid data for `formKey`. Must be a string:", `formKey`))
        }
        self$`formKey` <- `formKey`
      }
      if (!is.null(`intervalId`)) {
        if (!(is.numeric(`intervalId`) && length(`intervalId`) == 1)) {
          stop(paste("Error! Invalid data for `intervalId`. Must be an integer:", `intervalId`))
        }
        self$`intervalId` <- `intervalId`
      }
      if (!is.null(`role`)) {
        if (!(is.character(`role`) && length(`role`) == 1)) {
          stop(paste("Error! Invalid data for `role`. Must be a string:", `role`))
        }
        self$`role` <- `role`
      }
      if (!is.null(`user`)) {
        if (!(is.character(`user`) && length(`user`) == 1)) {
          stop(paste("Error! Invalid data for `user`. Must be a string:", `user`))
        }
        self$`user` <- `user`
      }
      if (!is.null(`reasonForChange`)) {
        if (!(is.character(`reasonForChange`) && length(`reasonForChange`) == 1)) {
          stop(paste("Error! Invalid data for `reasonForChange`. Must be a string:", `reasonForChange`))
        }
        self$`reasonForChange` <- `reasonForChange`
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
    #' @return RecordRevision as a base R list.
    #' @examples
    #' # convert array of RecordRevision (x) to a data frame
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
    #' Convert RecordRevision to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      RecordRevisionObject <- list()
      if (!is.null(self$`studyKey`)) {
        RecordRevisionObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`recordRevisionId`)) {
        RecordRevisionObject[["recordRevisionId"]] <-
          self$`recordRevisionId`
      }
      if (!is.null(self$`recordId`)) {
        RecordRevisionObject[["recordId"]] <-
          self$`recordId`
      }
      if (!is.null(self$`recordOid`)) {
        RecordRevisionObject[["recordOid"]] <-
          self$`recordOid`
      }
      if (!is.null(self$`recordRevision`)) {
        RecordRevisionObject[["recordRevision"]] <-
          self$`recordRevision`
      }
      if (!is.null(self$`dataRevision`)) {
        RecordRevisionObject[["dataRevision"]] <-
          self$`dataRevision`
      }
      if (!is.null(self$`recordStatus`)) {
        RecordRevisionObject[["recordStatus"]] <-
          self$`recordStatus`
      }
      if (!is.null(self$`subjectId`)) {
        RecordRevisionObject[["subjectId"]] <-
          self$`subjectId`
      }
      if (!is.null(self$`subjectOid`)) {
        RecordRevisionObject[["subjectOid"]] <-
          self$`subjectOid`
      }
      if (!is.null(self$`subjectKey`)) {
        RecordRevisionObject[["subjectKey"]] <-
          self$`subjectKey`
      }
      if (!is.null(self$`siteId`)) {
        RecordRevisionObject[["siteId"]] <-
          self$`siteId`
      }
      if (!is.null(self$`formKey`)) {
        RecordRevisionObject[["formKey"]] <-
          self$`formKey`
      }
      if (!is.null(self$`intervalId`)) {
        RecordRevisionObject[["intervalId"]] <-
          self$`intervalId`
      }
      if (!is.null(self$`role`)) {
        RecordRevisionObject[["role"]] <-
          self$`role`
      }
      if (!is.null(self$`user`)) {
        RecordRevisionObject[["user"]] <-
          self$`user`
      }
      if (!is.null(self$`reasonForChange`)) {
        RecordRevisionObject[["reasonForChange"]] <-
          self$`reasonForChange`
      }
      if (!is.null(self$`deleted`)) {
        RecordRevisionObject[["deleted"]] <-
          self$`deleted`
      }
      if (!is.null(self$`dateCreated`)) {
        RecordRevisionObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      return(RecordRevisionObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of RecordRevision
    #'
    #' @param input_json the JSON input
    #' @return the instance of RecordRevision
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`recordRevisionId`)) {
        self$`recordRevisionId` <- this_object$`recordRevisionId`
      }
      if (!is.null(this_object$`recordId`)) {
        self$`recordId` <- this_object$`recordId`
      }
      if (!is.null(this_object$`recordOid`)) {
        self$`recordOid` <- this_object$`recordOid`
      }
      if (!is.null(this_object$`recordRevision`)) {
        self$`recordRevision` <- this_object$`recordRevision`
      }
      if (!is.null(this_object$`dataRevision`)) {
        self$`dataRevision` <- this_object$`dataRevision`
      }
      if (!is.null(this_object$`recordStatus`)) {
        self$`recordStatus` <- this_object$`recordStatus`
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
      if (!is.null(this_object$`siteId`)) {
        self$`siteId` <- this_object$`siteId`
      }
      if (!is.null(this_object$`formKey`)) {
        self$`formKey` <- this_object$`formKey`
      }
      if (!is.null(this_object$`intervalId`)) {
        self$`intervalId` <- this_object$`intervalId`
      }
      if (!is.null(this_object$`role`)) {
        self$`role` <- this_object$`role`
      }
      if (!is.null(this_object$`user`)) {
        self$`user` <- this_object$`user`
      }
      if (!is.null(this_object$`reasonForChange`)) {
        self$`reasonForChange` <- this_object$`reasonForChange`
      }
      if (!is.null(this_object$`deleted`)) {
        self$`deleted` <- this_object$`deleted`
      }
      if (!is.null(this_object$`dateCreated`)) {
        self$`dateCreated` <- this_object$`dateCreated`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return RecordRevision in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of RecordRevision
    #'
    #' @param input_json the JSON input
    #' @return the instance of RecordRevision
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`recordRevisionId` <- this_object$`recordRevisionId`
      self$`recordId` <- this_object$`recordId`
      self$`recordOid` <- this_object$`recordOid`
      self$`recordRevision` <- this_object$`recordRevision`
      self$`dataRevision` <- this_object$`dataRevision`
      self$`recordStatus` <- this_object$`recordStatus`
      self$`subjectId` <- this_object$`subjectId`
      self$`subjectOid` <- this_object$`subjectOid`
      self$`subjectKey` <- this_object$`subjectKey`
      self$`siteId` <- this_object$`siteId`
      self$`formKey` <- this_object$`formKey`
      self$`intervalId` <- this_object$`intervalId`
      self$`role` <- this_object$`role`
      self$`user` <- this_object$`user`
      self$`reasonForChange` <- this_object$`reasonForChange`
      self$`deleted` <- this_object$`deleted`
      self$`dateCreated` <- this_object$`dateCreated`
      self
    },

    #' @description
    #' Validate JSON input with respect to RecordRevision and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of RecordRevision
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
# RecordRevision$unlock()
#
## Below is an example to define the print function
# RecordRevision$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# RecordRevision$lock()

