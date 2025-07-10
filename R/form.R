#' Create a new Form
#'
#' @description
#' Form Class
#'
#' @docType class
#' @title Form
#' @description Form Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field formId Mednet Form ID integer [optional]
#' @field formKey User-defined form key character [optional]
#' @field formName Name of the form (eCRF) character [optional]
#' @field formType Type of the form (e.g., Subject or Site) character [optional]
#' @field revision Number of modifications (revisions) of the form metadata integer [optional]
#' @field embeddedLog Whether the form has an embedded log character [optional]
#' @field enforceOwnership Whether the form enforces record ownership character [optional]
#' @field userAgreement Whether the form requires a user agreement character [optional]
#' @field subjectRecordReport Whether the form is marked as a subject record report character [optional]
#' @field unscheduledVisit Whether the form is included in unscheduled visits character [optional]
#' @field otherForms Whether the form is included in Other Forms category character [optional]
#' @field eproForm Whether the form is an ePRO (electronic patient reported outcome) form character [optional]
#' @field allowCopy Whether the form allows copying of data character [optional]
#' @field disabled Whether the form is soft-deleted (disabled) character [optional]
#' @field dateCreated Date when this form was created character [optional]
#' @field dateModified Date when this form was last modified character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Form <- R6::R6Class(
  "Form",
  public = list(
    `studyKey` = NULL,
    `formId` = NULL,
    `formKey` = NULL,
    `formName` = NULL,
    `formType` = NULL,
    `revision` = NULL,
    `embeddedLog` = NULL,
    `enforceOwnership` = NULL,
    `userAgreement` = NULL,
    `subjectRecordReport` = NULL,
    `unscheduledVisit` = NULL,
    `otherForms` = NULL,
    `eproForm` = NULL,
    `allowCopy` = NULL,
    `disabled` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,

    #' @description
    #' Initialize a new Form class.
    #'
    #' @param studyKey Unique study key
    #' @param formId Mednet Form ID
    #' @param formKey User-defined form key
    #' @param formName Name of the form (eCRF)
    #' @param formType Type of the form (e.g., Subject or Site)
    #' @param revision Number of modifications (revisions) of the form metadata
    #' @param embeddedLog Whether the form has an embedded log
    #' @param enforceOwnership Whether the form enforces record ownership
    #' @param userAgreement Whether the form requires a user agreement
    #' @param subjectRecordReport Whether the form is marked as a subject record report
    #' @param unscheduledVisit Whether the form is included in unscheduled visits
    #' @param otherForms Whether the form is included in Other Forms category
    #' @param eproForm Whether the form is an ePRO (electronic patient reported outcome) form
    #' @param allowCopy Whether the form allows copying of data
    #' @param disabled Whether the form is soft-deleted (disabled)
    #' @param dateCreated Date when this form was created
    #' @param dateModified Date when this form was last modified
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `formId` = NULL, `formKey` = NULL, `formName` = NULL, `formType` = NULL, `revision` = NULL, `embeddedLog` = NULL, `enforceOwnership` = NULL, `userAgreement` = NULL, `subjectRecordReport` = NULL, `unscheduledVisit` = NULL, `otherForms` = NULL, `eproForm` = NULL, `allowCopy` = NULL, `disabled` = NULL, `dateCreated` = NULL, `dateModified` = NULL, ...) {
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
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
      if (!is.null(`formName`)) {
        if (!(is.character(`formName`) && length(`formName`) == 1)) {
          stop(paste("Error! Invalid data for `formName`. Must be a string:", `formName`))
        }
        self$`formName` <- `formName`
      }
      if (!is.null(`formType`)) {
        if (!(is.character(`formType`) && length(`formType`) == 1)) {
          stop(paste("Error! Invalid data for `formType`. Must be a string:", `formType`))
        }
        self$`formType` <- `formType`
      }
      if (!is.null(`revision`)) {
        if (!(is.numeric(`revision`) && length(`revision`) == 1)) {
          stop(paste("Error! Invalid data for `revision`. Must be an integer:", `revision`))
        }
        self$`revision` <- `revision`
      }
      if (!is.null(`embeddedLog`)) {
        if (!(is.logical(`embeddedLog`) && length(`embeddedLog`) == 1)) {
          stop(paste("Error! Invalid data for `embeddedLog`. Must be a boolean:", `embeddedLog`))
        }
        self$`embeddedLog` <- `embeddedLog`
      }
      if (!is.null(`enforceOwnership`)) {
        if (!(is.logical(`enforceOwnership`) && length(`enforceOwnership`) == 1)) {
          stop(paste("Error! Invalid data for `enforceOwnership`. Must be a boolean:", `enforceOwnership`))
        }
        self$`enforceOwnership` <- `enforceOwnership`
      }
      if (!is.null(`userAgreement`)) {
        if (!(is.logical(`userAgreement`) && length(`userAgreement`) == 1)) {
          stop(paste("Error! Invalid data for `userAgreement`. Must be a boolean:", `userAgreement`))
        }
        self$`userAgreement` <- `userAgreement`
      }
      if (!is.null(`subjectRecordReport`)) {
        if (!(is.logical(`subjectRecordReport`) && length(`subjectRecordReport`) == 1)) {
          stop(paste("Error! Invalid data for `subjectRecordReport`. Must be a boolean:", `subjectRecordReport`))
        }
        self$`subjectRecordReport` <- `subjectRecordReport`
      }
      if (!is.null(`unscheduledVisit`)) {
        if (!(is.logical(`unscheduledVisit`) && length(`unscheduledVisit`) == 1)) {
          stop(paste("Error! Invalid data for `unscheduledVisit`. Must be a boolean:", `unscheduledVisit`))
        }
        self$`unscheduledVisit` <- `unscheduledVisit`
      }
      if (!is.null(`otherForms`)) {
        if (!(is.logical(`otherForms`) && length(`otherForms`) == 1)) {
          stop(paste("Error! Invalid data for `otherForms`. Must be a boolean:", `otherForms`))
        }
        self$`otherForms` <- `otherForms`
      }
      if (!is.null(`eproForm`)) {
        if (!(is.logical(`eproForm`) && length(`eproForm`) == 1)) {
          stop(paste("Error! Invalid data for `eproForm`. Must be a boolean:", `eproForm`))
        }
        self$`eproForm` <- `eproForm`
      }
      if (!is.null(`allowCopy`)) {
        if (!(is.logical(`allowCopy`) && length(`allowCopy`) == 1)) {
          stop(paste("Error! Invalid data for `allowCopy`. Must be a boolean:", `allowCopy`))
        }
        self$`allowCopy` <- `allowCopy`
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
    #' @return Form as a base R list.
    #' @examples
    #' # convert array of Form (x) to a data frame
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
    #' Convert Form to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      FormObject <- list()
      if (!is.null(self$`studyKey`)) {
        FormObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`formId`)) {
        FormObject[["formId"]] <-
          self$`formId`
      }
      if (!is.null(self$`formKey`)) {
        FormObject[["formKey"]] <-
          self$`formKey`
      }
      if (!is.null(self$`formName`)) {
        FormObject[["formName"]] <-
          self$`formName`
      }
      if (!is.null(self$`formType`)) {
        FormObject[["formType"]] <-
          self$`formType`
      }
      if (!is.null(self$`revision`)) {
        FormObject[["revision"]] <-
          self$`revision`
      }
      if (!is.null(self$`embeddedLog`)) {
        FormObject[["embeddedLog"]] <-
          self$`embeddedLog`
      }
      if (!is.null(self$`enforceOwnership`)) {
        FormObject[["enforceOwnership"]] <-
          self$`enforceOwnership`
      }
      if (!is.null(self$`userAgreement`)) {
        FormObject[["userAgreement"]] <-
          self$`userAgreement`
      }
      if (!is.null(self$`subjectRecordReport`)) {
        FormObject[["subjectRecordReport"]] <-
          self$`subjectRecordReport`
      }
      if (!is.null(self$`unscheduledVisit`)) {
        FormObject[["unscheduledVisit"]] <-
          self$`unscheduledVisit`
      }
      if (!is.null(self$`otherForms`)) {
        FormObject[["otherForms"]] <-
          self$`otherForms`
      }
      if (!is.null(self$`eproForm`)) {
        FormObject[["eproForm"]] <-
          self$`eproForm`
      }
      if (!is.null(self$`allowCopy`)) {
        FormObject[["allowCopy"]] <-
          self$`allowCopy`
      }
      if (!is.null(self$`disabled`)) {
        FormObject[["disabled"]] <-
          self$`disabled`
      }
      if (!is.null(self$`dateCreated`)) {
        FormObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        FormObject[["dateModified"]] <-
          self$`dateModified`
      }
      return(FormObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Form
    #'
    #' @param input_json the JSON input
    #' @return the instance of Form
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`formId`)) {
        self$`formId` <- this_object$`formId`
      }
      if (!is.null(this_object$`formKey`)) {
        self$`formKey` <- this_object$`formKey`
      }
      if (!is.null(this_object$`formName`)) {
        self$`formName` <- this_object$`formName`
      }
      if (!is.null(this_object$`formType`)) {
        self$`formType` <- this_object$`formType`
      }
      if (!is.null(this_object$`revision`)) {
        self$`revision` <- this_object$`revision`
      }
      if (!is.null(this_object$`embeddedLog`)) {
        self$`embeddedLog` <- this_object$`embeddedLog`
      }
      if (!is.null(this_object$`enforceOwnership`)) {
        self$`enforceOwnership` <- this_object$`enforceOwnership`
      }
      if (!is.null(this_object$`userAgreement`)) {
        self$`userAgreement` <- this_object$`userAgreement`
      }
      if (!is.null(this_object$`subjectRecordReport`)) {
        self$`subjectRecordReport` <- this_object$`subjectRecordReport`
      }
      if (!is.null(this_object$`unscheduledVisit`)) {
        self$`unscheduledVisit` <- this_object$`unscheduledVisit`
      }
      if (!is.null(this_object$`otherForms`)) {
        self$`otherForms` <- this_object$`otherForms`
      }
      if (!is.null(this_object$`eproForm`)) {
        self$`eproForm` <- this_object$`eproForm`
      }
      if (!is.null(this_object$`allowCopy`)) {
        self$`allowCopy` <- this_object$`allowCopy`
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
    #' @return Form in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Form
    #'
    #' @param input_json the JSON input
    #' @return the instance of Form
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`formId` <- this_object$`formId`
      self$`formKey` <- this_object$`formKey`
      self$`formName` <- this_object$`formName`
      self$`formType` <- this_object$`formType`
      self$`revision` <- this_object$`revision`
      self$`embeddedLog` <- this_object$`embeddedLog`
      self$`enforceOwnership` <- this_object$`enforceOwnership`
      self$`userAgreement` <- this_object$`userAgreement`
      self$`subjectRecordReport` <- this_object$`subjectRecordReport`
      self$`unscheduledVisit` <- this_object$`unscheduledVisit`
      self$`otherForms` <- this_object$`otherForms`
      self$`eproForm` <- this_object$`eproForm`
      self$`allowCopy` <- this_object$`allowCopy`
      self$`disabled` <- this_object$`disabled`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self
    },

    #' @description
    #' Validate JSON input with respect to Form and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Form
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
# Form$unlock()
#
## Below is an example to define the print function
# Form$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Form$lock()

