#' Create a new Visit
#'
#' @description
#' Visit Class
#'
#' @docType class
#' @title Visit
#' @description Visit Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field visitId Unique system identifier for the subject visit instance integer [optional]
#' @field intervalId Unique system identifier of the interval definition for this visit integer [optional]
#' @field intervalName Name of the interval (visit) for this visit instance character [optional]
#' @field subjectId Mednet subject ID of the subject integer [optional]
#' @field subjectKey Protocol-assigned subject identifier character [optional]
#' @field startDate Start date of the visit window (in YYYY-MM-DD format) character [optional]
#' @field endDate End date of the visit window (in YYYY-MM-DD format) character [optional]
#' @field dueDate Due date of the visit (in YYYY-MM-DD format), if applicable character [optional]
#' @field visitDate Actual date the visit took place (in YYYY-MM-DD format) character [optional]
#' @field visitDateForm Name of the form used to capture the actual visit date character [optional]
#' @field visitDateQuestion Variable name of the field capturing the actual visit date character [optional]
#' @field deleted Whether the visit instance is marked as deleted character [optional]
#' @field dateCreated Date when this visit record was created character [optional]
#' @field dateModified Date when this visit record was last modified character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Visit <- R6::R6Class(
  "Visit",
  public = list(
    `studyKey` = NULL,
    `visitId` = NULL,
    `intervalId` = NULL,
    `intervalName` = NULL,
    `subjectId` = NULL,
    `subjectKey` = NULL,
    `startDate` = NULL,
    `endDate` = NULL,
    `dueDate` = NULL,
    `visitDate` = NULL,
    `visitDateForm` = NULL,
    `visitDateQuestion` = NULL,
    `deleted` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,

    #' @description
    #' Initialize a new Visit class.
    #'
    #' @param studyKey Unique study key
    #' @param visitId Unique system identifier for the subject visit instance
    #' @param intervalId Unique system identifier of the interval definition for this visit
    #' @param intervalName Name of the interval (visit) for this visit instance
    #' @param subjectId Mednet subject ID of the subject
    #' @param subjectKey Protocol-assigned subject identifier
    #' @param startDate Start date of the visit window (in YYYY-MM-DD format)
    #' @param endDate End date of the visit window (in YYYY-MM-DD format)
    #' @param dueDate Due date of the visit (in YYYY-MM-DD format), if applicable
    #' @param visitDate Actual date the visit took place (in YYYY-MM-DD format)
    #' @param visitDateForm Name of the form used to capture the actual visit date
    #' @param visitDateQuestion Variable name of the field capturing the actual visit date
    #' @param deleted Whether the visit instance is marked as deleted
    #' @param dateCreated Date when this visit record was created
    #' @param dateModified Date when this visit record was last modified
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `visitId` = NULL, `intervalId` = NULL, `intervalName` = NULL, `subjectId` = NULL, `subjectKey` = NULL, `startDate` = NULL, `endDate` = NULL, `dueDate` = NULL, `visitDate` = NULL, `visitDateForm` = NULL, `visitDateQuestion` = NULL, `deleted` = NULL, `dateCreated` = NULL, `dateModified` = NULL, ...) {
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
      }
      if (!is.null(`visitId`)) {
        if (!(is.numeric(`visitId`) && length(`visitId`) == 1)) {
          stop(paste("Error! Invalid data for `visitId`. Must be an integer:", `visitId`))
        }
        self$`visitId` <- `visitId`
      }
      if (!is.null(`intervalId`)) {
        if (!(is.numeric(`intervalId`) && length(`intervalId`) == 1)) {
          stop(paste("Error! Invalid data for `intervalId`. Must be an integer:", `intervalId`))
        }
        self$`intervalId` <- `intervalId`
      }
      if (!is.null(`intervalName`)) {
        if (!(is.character(`intervalName`) && length(`intervalName`) == 1)) {
          stop(paste("Error! Invalid data for `intervalName`. Must be a string:", `intervalName`))
        }
        self$`intervalName` <- `intervalName`
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
      if (!is.null(`startDate`)) {
        if (!(is.character(`startDate`) && length(`startDate`) == 1)) {
          stop(paste("Error! Invalid data for `startDate`. Must be a string:", `startDate`))
        }
        self$`startDate` <- `startDate`
      }
      if (!is.null(`endDate`)) {
        if (!(is.character(`endDate`) && length(`endDate`) == 1)) {
          stop(paste("Error! Invalid data for `endDate`. Must be a string:", `endDate`))
        }
        self$`endDate` <- `endDate`
      }
      if (!is.null(`dueDate`)) {
        if (!(is.character(`dueDate`) && length(`dueDate`) == 1)) {
          stop(paste("Error! Invalid data for `dueDate`. Must be a string:", `dueDate`))
        }
        self$`dueDate` <- `dueDate`
      }
      if (!is.null(`visitDate`)) {
        if (!(is.character(`visitDate`) && length(`visitDate`) == 1)) {
          stop(paste("Error! Invalid data for `visitDate`. Must be a string:", `visitDate`))
        }
        self$`visitDate` <- `visitDate`
      }
      if (!is.null(`visitDateForm`)) {
        if (!(is.character(`visitDateForm`) && length(`visitDateForm`) == 1)) {
          stop(paste("Error! Invalid data for `visitDateForm`. Must be a string:", `visitDateForm`))
        }
        self$`visitDateForm` <- `visitDateForm`
      }
      if (!is.null(`visitDateQuestion`)) {
        if (!(is.character(`visitDateQuestion`) && length(`visitDateQuestion`) == 1)) {
          stop(paste("Error! Invalid data for `visitDateQuestion`. Must be a string:", `visitDateQuestion`))
        }
        self$`visitDateQuestion` <- `visitDateQuestion`
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
    #' @return Visit as a base R list.
    #' @examples
    #' # convert array of Visit (x) to a data frame
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
    #' Convert Visit to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      VisitObject <- list()
      if (!is.null(self$`studyKey`)) {
        VisitObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`visitId`)) {
        VisitObject[["visitId"]] <-
          self$`visitId`
      }
      if (!is.null(self$`intervalId`)) {
        VisitObject[["intervalId"]] <-
          self$`intervalId`
      }
      if (!is.null(self$`intervalName`)) {
        VisitObject[["intervalName"]] <-
          self$`intervalName`
      }
      if (!is.null(self$`subjectId`)) {
        VisitObject[["subjectId"]] <-
          self$`subjectId`
      }
      if (!is.null(self$`subjectKey`)) {
        VisitObject[["subjectKey"]] <-
          self$`subjectKey`
      }
      if (!is.null(self$`startDate`)) {
        VisitObject[["startDate"]] <-
          self$`startDate`
      }
      if (!is.null(self$`endDate`)) {
        VisitObject[["endDate"]] <-
          self$`endDate`
      }
      if (!is.null(self$`dueDate`)) {
        VisitObject[["dueDate"]] <-
          self$`dueDate`
      }
      if (!is.null(self$`visitDate`)) {
        VisitObject[["visitDate"]] <-
          self$`visitDate`
      }
      if (!is.null(self$`visitDateForm`)) {
        VisitObject[["visitDateForm"]] <-
          self$`visitDateForm`
      }
      if (!is.null(self$`visitDateQuestion`)) {
        VisitObject[["visitDateQuestion"]] <-
          self$`visitDateQuestion`
      }
      if (!is.null(self$`deleted`)) {
        VisitObject[["deleted"]] <-
          self$`deleted`
      }
      if (!is.null(self$`dateCreated`)) {
        VisitObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        VisitObject[["dateModified"]] <-
          self$`dateModified`
      }
      return(VisitObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Visit
    #'
    #' @param input_json the JSON input
    #' @return the instance of Visit
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`visitId`)) {
        self$`visitId` <- this_object$`visitId`
      }
      if (!is.null(this_object$`intervalId`)) {
        self$`intervalId` <- this_object$`intervalId`
      }
      if (!is.null(this_object$`intervalName`)) {
        self$`intervalName` <- this_object$`intervalName`
      }
      if (!is.null(this_object$`subjectId`)) {
        self$`subjectId` <- this_object$`subjectId`
      }
      if (!is.null(this_object$`subjectKey`)) {
        self$`subjectKey` <- this_object$`subjectKey`
      }
      if (!is.null(this_object$`startDate`)) {
        self$`startDate` <- this_object$`startDate`
      }
      if (!is.null(this_object$`endDate`)) {
        self$`endDate` <- this_object$`endDate`
      }
      if (!is.null(this_object$`dueDate`)) {
        self$`dueDate` <- this_object$`dueDate`
      }
      if (!is.null(this_object$`visitDate`)) {
        self$`visitDate` <- this_object$`visitDate`
      }
      if (!is.null(this_object$`visitDateForm`)) {
        self$`visitDateForm` <- this_object$`visitDateForm`
      }
      if (!is.null(this_object$`visitDateQuestion`)) {
        self$`visitDateQuestion` <- this_object$`visitDateQuestion`
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
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Visit in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Visit
    #'
    #' @param input_json the JSON input
    #' @return the instance of Visit
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`visitId` <- this_object$`visitId`
      self$`intervalId` <- this_object$`intervalId`
      self$`intervalName` <- this_object$`intervalName`
      self$`subjectId` <- this_object$`subjectId`
      self$`subjectKey` <- this_object$`subjectKey`
      self$`startDate` <- this_object$`startDate`
      self$`endDate` <- this_object$`endDate`
      self$`dueDate` <- this_object$`dueDate`
      self$`visitDate` <- this_object$`visitDate`
      self$`visitDateForm` <- this_object$`visitDateForm`
      self$`visitDateQuestion` <- this_object$`visitDateQuestion`
      self$`deleted` <- this_object$`deleted`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self
    },

    #' @description
    #' Validate JSON input with respect to Visit and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Visit
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
# Visit$unlock()
#
## Below is an example to define the print function
# Visit$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Visit$lock()

