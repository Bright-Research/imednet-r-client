#' Create a new Interval
#'
#' @description
#' Interval Class
#'
#' @docType class
#' @title Interval
#' @description Interval Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field intervalId Unique system identifier for the interval (visit definition) integer [optional]
#' @field intervalName Name of the interval (visit) as defined in the study character [optional]
#' @field intervalDescription Description of the interval (visit) character [optional]
#' @field intervalSequence Sequence number of the interval in the schedule integer [optional]
#' @field intervalGroupId Identifier for the interval group (if intervals are grouped) integer [optional]
#' @field intervalGroupName Name of the interval group character [optional]
#' @field timeline Type of interval visit window (e.g., None, Due Date, Start - End Date, Actual Date) character [optional]
#' @field definedUsingInterval Baseline interval used for calculating this interval’s dates character [optional]
#' @field windowCalculationForm Baseline form (name) from which the calculation date is taken character [optional]
#' @field windowCalculationDate Baseline field (variable name) from which the calculation date is taken character [optional]
#' @field actualDateForm Form used to capture the actual date for this interval character [optional]
#' @field actualDate Field (variable name) used to capture the actual date for this interval character [optional]
#' @field dueDateWillBeIn Number of days from the calculation date when the interval is due integer [optional]
#' @field negativeSlack Number of days before the due date that are allowed (negative window) integer [optional]
#' @field positiveSlack Number of days after the due date that are allowed (positive window) integer [optional]
#' @field eproGracePeriod Number of days of grace period for ePRO completion after due date integer [optional]
#' @field forms List of forms that are scheduled in this interval list(\link{ComponentsSchemasIntervalFormsItem}) [optional]
#' @field disabled Whether the interval is soft-deleted (disabled) character [optional]
#' @field dateCreated Date when this interval was created character [optional]
#' @field dateModified Date when this interval was last modified character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Interval <- R6::R6Class(
  "Interval",
  public = list(
    `studyKey` = NULL,
    `intervalId` = NULL,
    `intervalName` = NULL,
    `intervalDescription` = NULL,
    `intervalSequence` = NULL,
    `intervalGroupId` = NULL,
    `intervalGroupName` = NULL,
    `timeline` = NULL,
    `definedUsingInterval` = NULL,
    `windowCalculationForm` = NULL,
    `windowCalculationDate` = NULL,
    `actualDateForm` = NULL,
    `actualDate` = NULL,
    `dueDateWillBeIn` = NULL,
    `negativeSlack` = NULL,
    `positiveSlack` = NULL,
    `eproGracePeriod` = NULL,
    `forms` = NULL,
    `disabled` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,

    #' @description
    #' Initialize a new Interval class.
    #'
    #' @param studyKey Unique study key
    #' @param intervalId Unique system identifier for the interval (visit definition)
    #' @param intervalName Name of the interval (visit) as defined in the study
    #' @param intervalDescription Description of the interval (visit)
    #' @param intervalSequence Sequence number of the interval in the schedule
    #' @param intervalGroupId Identifier for the interval group (if intervals are grouped)
    #' @param intervalGroupName Name of the interval group
    #' @param timeline Type of interval visit window (e.g., None, Due Date, Start - End Date, Actual Date)
    #' @param definedUsingInterval Baseline interval used for calculating this interval’s dates
    #' @param windowCalculationForm Baseline form (name) from which the calculation date is taken
    #' @param windowCalculationDate Baseline field (variable name) from which the calculation date is taken
    #' @param actualDateForm Form used to capture the actual date for this interval
    #' @param actualDate Field (variable name) used to capture the actual date for this interval
    #' @param dueDateWillBeIn Number of days from the calculation date when the interval is due
    #' @param negativeSlack Number of days before the due date that are allowed (negative window)
    #' @param positiveSlack Number of days after the due date that are allowed (positive window)
    #' @param eproGracePeriod Number of days of grace period for ePRO completion after due date
    #' @param forms List of forms that are scheduled in this interval
    #' @param disabled Whether the interval is soft-deleted (disabled)
    #' @param dateCreated Date when this interval was created
    #' @param dateModified Date when this interval was last modified
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `intervalId` = NULL, `intervalName` = NULL, `intervalDescription` = NULL, `intervalSequence` = NULL, `intervalGroupId` = NULL, `intervalGroupName` = NULL, `timeline` = NULL, `definedUsingInterval` = NULL, `windowCalculationForm` = NULL, `windowCalculationDate` = NULL, `actualDateForm` = NULL, `actualDate` = NULL, `dueDateWillBeIn` = NULL, `negativeSlack` = NULL, `positiveSlack` = NULL, `eproGracePeriod` = NULL, `forms` = NULL, `disabled` = NULL, `dateCreated` = NULL, `dateModified` = NULL, ...) {
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
      if (!is.null(`intervalName`)) {
        if (!(is.character(`intervalName`) && length(`intervalName`) == 1)) {
          stop(paste("Error! Invalid data for `intervalName`. Must be a string:", `intervalName`))
        }
        self$`intervalName` <- `intervalName`
      }
      if (!is.null(`intervalDescription`)) {
        if (!(is.character(`intervalDescription`) && length(`intervalDescription`) == 1)) {
          stop(paste("Error! Invalid data for `intervalDescription`. Must be a string:", `intervalDescription`))
        }
        self$`intervalDescription` <- `intervalDescription`
      }
      if (!is.null(`intervalSequence`)) {
        if (!(is.numeric(`intervalSequence`) && length(`intervalSequence`) == 1)) {
          stop(paste("Error! Invalid data for `intervalSequence`. Must be an integer:", `intervalSequence`))
        }
        self$`intervalSequence` <- `intervalSequence`
      }
      if (!is.null(`intervalGroupId`)) {
        if (!(is.numeric(`intervalGroupId`) && length(`intervalGroupId`) == 1)) {
          stop(paste("Error! Invalid data for `intervalGroupId`. Must be an integer:", `intervalGroupId`))
        }
        self$`intervalGroupId` <- `intervalGroupId`
      }
      if (!is.null(`intervalGroupName`)) {
        if (!(is.character(`intervalGroupName`) && length(`intervalGroupName`) == 1)) {
          stop(paste("Error! Invalid data for `intervalGroupName`. Must be a string:", `intervalGroupName`))
        }
        self$`intervalGroupName` <- `intervalGroupName`
      }
      if (!is.null(`timeline`)) {
        if (!(is.character(`timeline`) && length(`timeline`) == 1)) {
          stop(paste("Error! Invalid data for `timeline`. Must be a string:", `timeline`))
        }
        self$`timeline` <- `timeline`
      }
      if (!is.null(`definedUsingInterval`)) {
        if (!(is.character(`definedUsingInterval`) && length(`definedUsingInterval`) == 1)) {
          stop(paste("Error! Invalid data for `definedUsingInterval`. Must be a string:", `definedUsingInterval`))
        }
        self$`definedUsingInterval` <- `definedUsingInterval`
      }
      if (!is.null(`windowCalculationForm`)) {
        if (!(is.character(`windowCalculationForm`) && length(`windowCalculationForm`) == 1)) {
          stop(paste("Error! Invalid data for `windowCalculationForm`. Must be a string:", `windowCalculationForm`))
        }
        self$`windowCalculationForm` <- `windowCalculationForm`
      }
      if (!is.null(`windowCalculationDate`)) {
        if (!(is.character(`windowCalculationDate`) && length(`windowCalculationDate`) == 1)) {
          stop(paste("Error! Invalid data for `windowCalculationDate`. Must be a string:", `windowCalculationDate`))
        }
        self$`windowCalculationDate` <- `windowCalculationDate`
      }
      if (!is.null(`actualDateForm`)) {
        if (!(is.character(`actualDateForm`) && length(`actualDateForm`) == 1)) {
          stop(paste("Error! Invalid data for `actualDateForm`. Must be a string:", `actualDateForm`))
        }
        self$`actualDateForm` <- `actualDateForm`
      }
      if (!is.null(`actualDate`)) {
        if (!(is.character(`actualDate`) && length(`actualDate`) == 1)) {
          stop(paste("Error! Invalid data for `actualDate`. Must be a string:", `actualDate`))
        }
        self$`actualDate` <- `actualDate`
      }
      if (!is.null(`dueDateWillBeIn`)) {
        if (!(is.numeric(`dueDateWillBeIn`) && length(`dueDateWillBeIn`) == 1)) {
          stop(paste("Error! Invalid data for `dueDateWillBeIn`. Must be an integer:", `dueDateWillBeIn`))
        }
        self$`dueDateWillBeIn` <- `dueDateWillBeIn`
      }
      if (!is.null(`negativeSlack`)) {
        if (!(is.numeric(`negativeSlack`) && length(`negativeSlack`) == 1)) {
          stop(paste("Error! Invalid data for `negativeSlack`. Must be an integer:", `negativeSlack`))
        }
        self$`negativeSlack` <- `negativeSlack`
      }
      if (!is.null(`positiveSlack`)) {
        if (!(is.numeric(`positiveSlack`) && length(`positiveSlack`) == 1)) {
          stop(paste("Error! Invalid data for `positiveSlack`. Must be an integer:", `positiveSlack`))
        }
        self$`positiveSlack` <- `positiveSlack`
      }
      if (!is.null(`eproGracePeriod`)) {
        if (!(is.numeric(`eproGracePeriod`) && length(`eproGracePeriod`) == 1)) {
          stop(paste("Error! Invalid data for `eproGracePeriod`. Must be an integer:", `eproGracePeriod`))
        }
        self$`eproGracePeriod` <- `eproGracePeriod`
      }
      if (!is.null(`forms`)) {
        stopifnot(is.vector(`forms`), length(`forms`) != 0)
        sapply(`forms`, function(x) stopifnot(R6::is.R6(x)))
        self$`forms` <- `forms`
      }
      if (!is.null(`disabled`)) {
        if (!(is.logical(`disabled`) && length(`disabled`) == 1)) {
          stop(paste("Error! Invalid data for `disabled`. Must be a boolean:", `disabled`))
        }
        self$`disabled` <- `disabled`
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
    #' @return Interval as a base R list.
    #' @examples
    #' # convert array of Interval (x) to a data frame
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
    #' Convert Interval to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      IntervalObject <- list()
      if (!is.null(self$`studyKey`)) {
        IntervalObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`intervalId`)) {
        IntervalObject[["intervalId"]] <-
          self$`intervalId`
      }
      if (!is.null(self$`intervalName`)) {
        IntervalObject[["intervalName"]] <-
          self$`intervalName`
      }
      if (!is.null(self$`intervalDescription`)) {
        IntervalObject[["intervalDescription"]] <-
          self$`intervalDescription`
      }
      if (!is.null(self$`intervalSequence`)) {
        IntervalObject[["intervalSequence"]] <-
          self$`intervalSequence`
      }
      if (!is.null(self$`intervalGroupId`)) {
        IntervalObject[["intervalGroupId"]] <-
          self$`intervalGroupId`
      }
      if (!is.null(self$`intervalGroupName`)) {
        IntervalObject[["intervalGroupName"]] <-
          self$`intervalGroupName`
      }
      if (!is.null(self$`timeline`)) {
        IntervalObject[["timeline"]] <-
          self$`timeline`
      }
      if (!is.null(self$`definedUsingInterval`)) {
        IntervalObject[["definedUsingInterval"]] <-
          self$`definedUsingInterval`
      }
      if (!is.null(self$`windowCalculationForm`)) {
        IntervalObject[["windowCalculationForm"]] <-
          self$`windowCalculationForm`
      }
      if (!is.null(self$`windowCalculationDate`)) {
        IntervalObject[["windowCalculationDate"]] <-
          self$`windowCalculationDate`
      }
      if (!is.null(self$`actualDateForm`)) {
        IntervalObject[["actualDateForm"]] <-
          self$`actualDateForm`
      }
      if (!is.null(self$`actualDate`)) {
        IntervalObject[["actualDate"]] <-
          self$`actualDate`
      }
      if (!is.null(self$`dueDateWillBeIn`)) {
        IntervalObject[["dueDateWillBeIn"]] <-
          self$`dueDateWillBeIn`
      }
      if (!is.null(self$`negativeSlack`)) {
        IntervalObject[["negativeSlack"]] <-
          self$`negativeSlack`
      }
      if (!is.null(self$`positiveSlack`)) {
        IntervalObject[["positiveSlack"]] <-
          self$`positiveSlack`
      }
      if (!is.null(self$`eproGracePeriod`)) {
        IntervalObject[["eproGracePeriod"]] <-
          self$`eproGracePeriod`
      }
      if (!is.null(self$`forms`)) {
        IntervalObject[["forms"]] <-
          lapply(self$`forms`, function(x) x$toSimpleType())
      }
      if (!is.null(self$`disabled`)) {
        IntervalObject[["disabled"]] <-
          self$`disabled`
      }
      if (!is.null(self$`dateCreated`)) {
        IntervalObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        IntervalObject[["dateModified"]] <-
          self$`dateModified`
      }
      return(IntervalObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Interval
    #'
    #' @param input_json the JSON input
    #' @return the instance of Interval
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`intervalId`)) {
        self$`intervalId` <- this_object$`intervalId`
      }
      if (!is.null(this_object$`intervalName`)) {
        self$`intervalName` <- this_object$`intervalName`
      }
      if (!is.null(this_object$`intervalDescription`)) {
        self$`intervalDescription` <- this_object$`intervalDescription`
      }
      if (!is.null(this_object$`intervalSequence`)) {
        self$`intervalSequence` <- this_object$`intervalSequence`
      }
      if (!is.null(this_object$`intervalGroupId`)) {
        self$`intervalGroupId` <- this_object$`intervalGroupId`
      }
      if (!is.null(this_object$`intervalGroupName`)) {
        self$`intervalGroupName` <- this_object$`intervalGroupName`
      }
      if (!is.null(this_object$`timeline`)) {
        self$`timeline` <- this_object$`timeline`
      }
      if (!is.null(this_object$`definedUsingInterval`)) {
        self$`definedUsingInterval` <- this_object$`definedUsingInterval`
      }
      if (!is.null(this_object$`windowCalculationForm`)) {
        self$`windowCalculationForm` <- this_object$`windowCalculationForm`
      }
      if (!is.null(this_object$`windowCalculationDate`)) {
        self$`windowCalculationDate` <- this_object$`windowCalculationDate`
      }
      if (!is.null(this_object$`actualDateForm`)) {
        self$`actualDateForm` <- this_object$`actualDateForm`
      }
      if (!is.null(this_object$`actualDate`)) {
        self$`actualDate` <- this_object$`actualDate`
      }
      if (!is.null(this_object$`dueDateWillBeIn`)) {
        self$`dueDateWillBeIn` <- this_object$`dueDateWillBeIn`
      }
      if (!is.null(this_object$`negativeSlack`)) {
        self$`negativeSlack` <- this_object$`negativeSlack`
      }
      if (!is.null(this_object$`positiveSlack`)) {
        self$`positiveSlack` <- this_object$`positiveSlack`
      }
      if (!is.null(this_object$`eproGracePeriod`)) {
        self$`eproGracePeriod` <- this_object$`eproGracePeriod`
      }
      if (!is.null(this_object$`forms`)) {
        self$`forms` <- ApiClient$new()$deserializeObj(this_object$`forms`, "array[ComponentsSchemasIntervalFormsItem]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`disabled`)) {
        self$`disabled` <- this_object$`disabled`
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
    #' @return Interval in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Interval
    #'
    #' @param input_json the JSON input
    #' @return the instance of Interval
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`intervalId` <- this_object$`intervalId`
      self$`intervalName` <- this_object$`intervalName`
      self$`intervalDescription` <- this_object$`intervalDescription`
      self$`intervalSequence` <- this_object$`intervalSequence`
      self$`intervalGroupId` <- this_object$`intervalGroupId`
      self$`intervalGroupName` <- this_object$`intervalGroupName`
      self$`timeline` <- this_object$`timeline`
      self$`definedUsingInterval` <- this_object$`definedUsingInterval`
      self$`windowCalculationForm` <- this_object$`windowCalculationForm`
      self$`windowCalculationDate` <- this_object$`windowCalculationDate`
      self$`actualDateForm` <- this_object$`actualDateForm`
      self$`actualDate` <- this_object$`actualDate`
      self$`dueDateWillBeIn` <- this_object$`dueDateWillBeIn`
      self$`negativeSlack` <- this_object$`negativeSlack`
      self$`positiveSlack` <- this_object$`positiveSlack`
      self$`eproGracePeriod` <- this_object$`eproGracePeriod`
      self$`forms` <- ApiClient$new()$deserializeObj(this_object$`forms`, "array[ComponentsSchemasIntervalFormsItem]", loadNamespace("openapi"))
      self$`disabled` <- this_object$`disabled`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self
    },

    #' @description
    #' Validate JSON input with respect to Interval and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Interval
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
# Interval$unlock()
#
## Below is an example to define the print function
# Interval$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Interval$lock()

