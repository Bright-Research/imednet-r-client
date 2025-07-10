#' Create a new Variable
#'
#' @description
#' Variable Class
#'
#' @docType class
#' @title Variable
#' @description Variable Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field variableId Mednet variable ID integer [optional]
#' @field variableType Type of the variable (field type), e.g., RADIO, TEXT, etc. character [optional]
#' @field variableName Name of the variable (question text or label) character [optional]
#' @field sequence Sequence of the variable on the form integer [optional]
#' @field revision Number of modifications of the variable (via form metadata revisions) integer [optional]
#' @field disabled Whether the variable is marked as disabled (deleted) character [optional]
#' @field dateCreated Date when this variable was created character [optional]
#' @field dateModified Date when this variable was last modified character [optional]
#' @field formId ID of the form that this variable belongs to integer [optional]
#' @field variableOid Client-assigned variable OID character [optional]
#' @field deleted Whether the variable is marked as deleted character [optional]
#' @field formKey Form key of the form that this variable belongs to character [optional]
#' @field formName Name of the form that this variable belongs to character [optional]
#' @field label User-defined identifier (field name) for the variable character [optional]
#' @field blinded Whether the variable is flagged as blinded (hidden in certain contexts) character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Variable <- R6::R6Class(
  "Variable",
  public = list(
    `studyKey` = NULL,
    `variableId` = NULL,
    `variableType` = NULL,
    `variableName` = NULL,
    `sequence` = NULL,
    `revision` = NULL,
    `disabled` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,
    `formId` = NULL,
    `variableOid` = NULL,
    `deleted` = NULL,
    `formKey` = NULL,
    `formName` = NULL,
    `label` = NULL,
    `blinded` = NULL,

    #' @description
    #' Initialize a new Variable class.
    #'
    #' @param studyKey Unique study key
    #' @param variableId Mednet variable ID
    #' @param variableType Type of the variable (field type), e.g., RADIO, TEXT, etc.
    #' @param variableName Name of the variable (question text or label)
    #' @param sequence Sequence of the variable on the form
    #' @param revision Number of modifications of the variable (via form metadata revisions)
    #' @param disabled Whether the variable is marked as disabled (deleted)
    #' @param dateCreated Date when this variable was created
    #' @param dateModified Date when this variable was last modified
    #' @param formId ID of the form that this variable belongs to
    #' @param variableOid Client-assigned variable OID
    #' @param deleted Whether the variable is marked as deleted
    #' @param formKey Form key of the form that this variable belongs to
    #' @param formName Name of the form that this variable belongs to
    #' @param label User-defined identifier (field name) for the variable
    #' @param blinded Whether the variable is flagged as blinded (hidden in certain contexts)
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `variableId` = NULL, `variableType` = NULL, `variableName` = NULL, `sequence` = NULL, `revision` = NULL, `disabled` = NULL, `dateCreated` = NULL, `dateModified` = NULL, `formId` = NULL, `variableOid` = NULL, `deleted` = NULL, `formKey` = NULL, `formName` = NULL, `label` = NULL, `blinded` = NULL, ...) {
      if (!is.null(`studyKey`)) {
        if (!(is.character(`studyKey`) && length(`studyKey`) == 1)) {
          stop(paste("Error! Invalid data for `studyKey`. Must be a string:", `studyKey`))
        }
        self$`studyKey` <- `studyKey`
      }
      if (!is.null(`variableId`)) {
        if (!(is.numeric(`variableId`) && length(`variableId`) == 1)) {
          stop(paste("Error! Invalid data for `variableId`. Must be an integer:", `variableId`))
        }
        self$`variableId` <- `variableId`
      }
      if (!is.null(`variableType`)) {
        if (!(`variableType` %in% c("TEXT", "TEXTAREA", "RADIO", "CHECKBOX", "DROPDOWN", "DATE", "NUMBER"))) {
          stop(paste("Error! \"", `variableType`, "\" cannot be assigned to `variableType`. Must be \"TEXT\", \"TEXTAREA\", \"RADIO\", \"CHECKBOX\", \"DROPDOWN\", \"DATE\", \"NUMBER\".", sep = ""))
        }
        if (!(is.character(`variableType`) && length(`variableType`) == 1)) {
          stop(paste("Error! Invalid data for `variableType`. Must be a string:", `variableType`))
        }
        self$`variableType` <- `variableType`
      }
      if (!is.null(`variableName`)) {
        if (!(is.character(`variableName`) && length(`variableName`) == 1)) {
          stop(paste("Error! Invalid data for `variableName`. Must be a string:", `variableName`))
        }
        self$`variableName` <- `variableName`
      }
      if (!is.null(`sequence`)) {
        if (!(is.numeric(`sequence`) && length(`sequence`) == 1)) {
          stop(paste("Error! Invalid data for `sequence`. Must be an integer:", `sequence`))
        }
        self$`sequence` <- `sequence`
      }
      if (!is.null(`revision`)) {
        if (!(is.numeric(`revision`) && length(`revision`) == 1)) {
          stop(paste("Error! Invalid data for `revision`. Must be an integer:", `revision`))
        }
        self$`revision` <- `revision`
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
      if (!is.null(`formId`)) {
        if (!(is.numeric(`formId`) && length(`formId`) == 1)) {
          stop(paste("Error! Invalid data for `formId`. Must be an integer:", `formId`))
        }
        self$`formId` <- `formId`
      }
      if (!is.null(`variableOid`)) {
        if (!(is.character(`variableOid`) && length(`variableOid`) == 1)) {
          stop(paste("Error! Invalid data for `variableOid`. Must be a string:", `variableOid`))
        }
        self$`variableOid` <- `variableOid`
      }
      if (!is.null(`deleted`)) {
        if (!(is.logical(`deleted`) && length(`deleted`) == 1)) {
          stop(paste("Error! Invalid data for `deleted`. Must be a boolean:", `deleted`))
        }
        self$`deleted` <- `deleted`
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
      if (!is.null(`label`)) {
        if (!(is.character(`label`) && length(`label`) == 1)) {
          stop(paste("Error! Invalid data for `label`. Must be a string:", `label`))
        }
        self$`label` <- `label`
      }
      if (!is.null(`blinded`)) {
        if (!(is.logical(`blinded`) && length(`blinded`) == 1)) {
          stop(paste("Error! Invalid data for `blinded`. Must be a boolean:", `blinded`))
        }
        self$`blinded` <- `blinded`
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
    #' @return Variable as a base R list.
    #' @examples
    #' # convert array of Variable (x) to a data frame
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
    #' Convert Variable to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      VariableObject <- list()
      if (!is.null(self$`studyKey`)) {
        VariableObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`variableId`)) {
        VariableObject[["variableId"]] <-
          self$`variableId`
      }
      if (!is.null(self$`variableType`)) {
        VariableObject[["variableType"]] <-
          self$`variableType`
      }
      if (!is.null(self$`variableName`)) {
        VariableObject[["variableName"]] <-
          self$`variableName`
      }
      if (!is.null(self$`sequence`)) {
        VariableObject[["sequence"]] <-
          self$`sequence`
      }
      if (!is.null(self$`revision`)) {
        VariableObject[["revision"]] <-
          self$`revision`
      }
      if (!is.null(self$`disabled`)) {
        VariableObject[["disabled"]] <-
          self$`disabled`
      }
      if (!is.null(self$`dateCreated`)) {
        VariableObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        VariableObject[["dateModified"]] <-
          self$`dateModified`
      }
      if (!is.null(self$`formId`)) {
        VariableObject[["formId"]] <-
          self$`formId`
      }
      if (!is.null(self$`variableOid`)) {
        VariableObject[["variableOid"]] <-
          self$`variableOid`
      }
      if (!is.null(self$`deleted`)) {
        VariableObject[["deleted"]] <-
          self$`deleted`
      }
      if (!is.null(self$`formKey`)) {
        VariableObject[["formKey"]] <-
          self$`formKey`
      }
      if (!is.null(self$`formName`)) {
        VariableObject[["formName"]] <-
          self$`formName`
      }
      if (!is.null(self$`label`)) {
        VariableObject[["label"]] <-
          self$`label`
      }
      if (!is.null(self$`blinded`)) {
        VariableObject[["blinded"]] <-
          self$`blinded`
      }
      return(VariableObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Variable
    #'
    #' @param input_json the JSON input
    #' @return the instance of Variable
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`studyKey`)) {
        self$`studyKey` <- this_object$`studyKey`
      }
      if (!is.null(this_object$`variableId`)) {
        self$`variableId` <- this_object$`variableId`
      }
      if (!is.null(this_object$`variableType`)) {
        if (!is.null(this_object$`variableType`) && !(this_object$`variableType` %in% c("TEXT", "TEXTAREA", "RADIO", "CHECKBOX", "DROPDOWN", "DATE", "NUMBER"))) {
          stop(paste("Error! \"", this_object$`variableType`, "\" cannot be assigned to `variableType`. Must be \"TEXT\", \"TEXTAREA\", \"RADIO\", \"CHECKBOX\", \"DROPDOWN\", \"DATE\", \"NUMBER\".", sep = ""))
        }
        self$`variableType` <- this_object$`variableType`
      }
      if (!is.null(this_object$`variableName`)) {
        self$`variableName` <- this_object$`variableName`
      }
      if (!is.null(this_object$`sequence`)) {
        self$`sequence` <- this_object$`sequence`
      }
      if (!is.null(this_object$`revision`)) {
        self$`revision` <- this_object$`revision`
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
      if (!is.null(this_object$`formId`)) {
        self$`formId` <- this_object$`formId`
      }
      if (!is.null(this_object$`variableOid`)) {
        self$`variableOid` <- this_object$`variableOid`
      }
      if (!is.null(this_object$`deleted`)) {
        self$`deleted` <- this_object$`deleted`
      }
      if (!is.null(this_object$`formKey`)) {
        self$`formKey` <- this_object$`formKey`
      }
      if (!is.null(this_object$`formName`)) {
        self$`formName` <- this_object$`formName`
      }
      if (!is.null(this_object$`label`)) {
        self$`label` <- this_object$`label`
      }
      if (!is.null(this_object$`blinded`)) {
        self$`blinded` <- this_object$`blinded`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Variable in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Variable
    #'
    #' @param input_json the JSON input
    #' @return the instance of Variable
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`variableId` <- this_object$`variableId`
      if (!is.null(this_object$`variableType`) && !(this_object$`variableType` %in% c("TEXT", "TEXTAREA", "RADIO", "CHECKBOX", "DROPDOWN", "DATE", "NUMBER"))) {
        stop(paste("Error! \"", this_object$`variableType`, "\" cannot be assigned to `variableType`. Must be \"TEXT\", \"TEXTAREA\", \"RADIO\", \"CHECKBOX\", \"DROPDOWN\", \"DATE\", \"NUMBER\".", sep = ""))
      }
      self$`variableType` <- this_object$`variableType`
      self$`variableName` <- this_object$`variableName`
      self$`sequence` <- this_object$`sequence`
      self$`revision` <- this_object$`revision`
      self$`disabled` <- this_object$`disabled`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self$`formId` <- this_object$`formId`
      self$`variableOid` <- this_object$`variableOid`
      self$`deleted` <- this_object$`deleted`
      self$`formKey` <- this_object$`formKey`
      self$`formName` <- this_object$`formName`
      self$`label` <- this_object$`label`
      self$`blinded` <- this_object$`blinded`
      self
    },

    #' @description
    #' Validate JSON input with respect to Variable and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Variable
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
# Variable$unlock()
#
## Below is an example to define the print function
# Variable$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Variable$lock()

