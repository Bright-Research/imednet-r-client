#' Create a new ComponentsSchemasRecordCreateRequestItem
#'
#' @description
#' ComponentsSchemasRecordCreateRequestItem Class
#'
#' @docType class
#' @title ComponentsSchemasRecordCreateRequestItem
#' @description ComponentsSchemasRecordCreateRequestItem Class
#' @format An \code{R6Class} generator object
#' @field formKey Form key identifying the eCRF to create or update character
#' @field formId Form ID identifying the eCRF to create or update (alternative to formKey) integer [optional]
#' @field siteName Name of the site where the record should be created (for new subject registration) character [optional]
#' @field siteId Site ID for the record (alternative to siteName) integer [optional]
#' @field subjectKey Subject identifier (display ID) for which to create or update the record character [optional]
#' @field subjectId Subject ID for which to create or update the record (alternative to subjectKey) integer [optional]
#' @field subjectOid Subject OID for which to create or update the record (alternative to subjectKey) character [optional]
#' @field intervalName Name of the interval (visit) for a scheduled record update character [optional]
#' @field intervalId Interval ID for a scheduled record update (alternative to intervalName) integer [optional]
#' @field recordId Record ID for updating an existing unscheduled record (if applicable) integer [optional]
#' @field recordOid Record OID for updating an existing unscheduled record (if applicable) character [optional]
#' @field data Key-value pairs of field names and values for the record data named list(\link{AnyType})
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ComponentsSchemasRecordCreateRequestItem <- R6::R6Class(
  "ComponentsSchemasRecordCreateRequestItem",
  public = list(
    `formKey` = NULL,
    `formId` = NULL,
    `siteName` = NULL,
    `siteId` = NULL,
    `subjectKey` = NULL,
    `subjectId` = NULL,
    `subjectOid` = NULL,
    `intervalName` = NULL,
    `intervalId` = NULL,
    `recordId` = NULL,
    `recordOid` = NULL,
    `data` = NULL,

    #' @description
    #' Initialize a new ComponentsSchemasRecordCreateRequestItem class.
    #'
    #' @param formKey Form key identifying the eCRF to create or update
    #' @param data Key-value pairs of field names and values for the record data
    #' @param formId Form ID identifying the eCRF to create or update (alternative to formKey)
    #' @param siteName Name of the site where the record should be created (for new subject registration)
    #' @param siteId Site ID for the record (alternative to siteName)
    #' @param subjectKey Subject identifier (display ID) for which to create or update the record
    #' @param subjectId Subject ID for which to create or update the record (alternative to subjectKey)
    #' @param subjectOid Subject OID for which to create or update the record (alternative to subjectKey)
    #' @param intervalName Name of the interval (visit) for a scheduled record update
    #' @param intervalId Interval ID for a scheduled record update (alternative to intervalName)
    #' @param recordId Record ID for updating an existing unscheduled record (if applicable)
    #' @param recordOid Record OID for updating an existing unscheduled record (if applicable)
    #' @param ... Other optional arguments.
    initialize = function(`formKey`, `data`, `formId` = NULL, `siteName` = NULL, `siteId` = NULL, `subjectKey` = NULL, `subjectId` = NULL, `subjectOid` = NULL, `intervalName` = NULL, `intervalId` = NULL, `recordId` = NULL, `recordOid` = NULL, ...) {
      if (!missing(`formKey`)) {
        if (!(is.character(`formKey`) && length(`formKey`) == 1)) {
          stop(paste("Error! Invalid data for `formKey`. Must be a string:", `formKey`))
        }
        self$`formKey` <- `formKey`
      }
      if (!missing(`data`)) {
        stopifnot(is.vector(`data`), length(`data`) != 0)
        sapply(`data`, function(x) stopifnot(R6::is.R6(x)))
        self$`data` <- `data`
      }
      if (!is.null(`formId`)) {
        if (!(is.numeric(`formId`) && length(`formId`) == 1)) {
          stop(paste("Error! Invalid data for `formId`. Must be an integer:", `formId`))
        }
        self$`formId` <- `formId`
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
      if (!is.null(`subjectKey`)) {
        if (!(is.character(`subjectKey`) && length(`subjectKey`) == 1)) {
          stop(paste("Error! Invalid data for `subjectKey`. Must be a string:", `subjectKey`))
        }
        self$`subjectKey` <- `subjectKey`
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
      if (!is.null(`intervalName`)) {
        if (!(is.character(`intervalName`) && length(`intervalName`) == 1)) {
          stop(paste("Error! Invalid data for `intervalName`. Must be a string:", `intervalName`))
        }
        self$`intervalName` <- `intervalName`
      }
      if (!is.null(`intervalId`)) {
        if (!(is.numeric(`intervalId`) && length(`intervalId`) == 1)) {
          stop(paste("Error! Invalid data for `intervalId`. Must be an integer:", `intervalId`))
        }
        self$`intervalId` <- `intervalId`
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
    #' @return ComponentsSchemasRecordCreateRequestItem as a base R list.
    #' @examples
    #' # convert array of ComponentsSchemasRecordCreateRequestItem (x) to a data frame
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
    #' Convert ComponentsSchemasRecordCreateRequestItem to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      ComponentsSchemasRecordCreateRequestItemObject <- list()
      if (!is.null(self$`formKey`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["formKey"]] <-
          self$`formKey`
      }
      if (!is.null(self$`formId`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["formId"]] <-
          self$`formId`
      }
      if (!is.null(self$`siteName`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["siteName"]] <-
          self$`siteName`
      }
      if (!is.null(self$`siteId`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["siteId"]] <-
          self$`siteId`
      }
      if (!is.null(self$`subjectKey`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["subjectKey"]] <-
          self$`subjectKey`
      }
      if (!is.null(self$`subjectId`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["subjectId"]] <-
          self$`subjectId`
      }
      if (!is.null(self$`subjectOid`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["subjectOid"]] <-
          self$`subjectOid`
      }
      if (!is.null(self$`intervalName`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["intervalName"]] <-
          self$`intervalName`
      }
      if (!is.null(self$`intervalId`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["intervalId"]] <-
          self$`intervalId`
      }
      if (!is.null(self$`recordId`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["recordId"]] <-
          self$`recordId`
      }
      if (!is.null(self$`recordOid`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["recordOid"]] <-
          self$`recordOid`
      }
      if (!is.null(self$`data`)) {
        ComponentsSchemasRecordCreateRequestItemObject[["data"]] <-
          lapply(self$`data`, function(x) x$toSimpleType())
      }
      return(ComponentsSchemasRecordCreateRequestItemObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of ComponentsSchemasRecordCreateRequestItem
    #'
    #' @param input_json the JSON input
    #' @return the instance of ComponentsSchemasRecordCreateRequestItem
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`formKey`)) {
        self$`formKey` <- this_object$`formKey`
      }
      if (!is.null(this_object$`formId`)) {
        self$`formId` <- this_object$`formId`
      }
      if (!is.null(this_object$`siteName`)) {
        self$`siteName` <- this_object$`siteName`
      }
      if (!is.null(this_object$`siteId`)) {
        self$`siteId` <- this_object$`siteId`
      }
      if (!is.null(this_object$`subjectKey`)) {
        self$`subjectKey` <- this_object$`subjectKey`
      }
      if (!is.null(this_object$`subjectId`)) {
        self$`subjectId` <- this_object$`subjectId`
      }
      if (!is.null(this_object$`subjectOid`)) {
        self$`subjectOid` <- this_object$`subjectOid`
      }
      if (!is.null(this_object$`intervalName`)) {
        self$`intervalName` <- this_object$`intervalName`
      }
      if (!is.null(this_object$`intervalId`)) {
        self$`intervalId` <- this_object$`intervalId`
      }
      if (!is.null(this_object$`recordId`)) {
        self$`recordId` <- this_object$`recordId`
      }
      if (!is.null(this_object$`recordOid`)) {
        self$`recordOid` <- this_object$`recordOid`
      }
      if (!is.null(this_object$`data`)) {
        self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "map(AnyType)", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return ComponentsSchemasRecordCreateRequestItem in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of ComponentsSchemasRecordCreateRequestItem
    #'
    #' @param input_json the JSON input
    #' @return the instance of ComponentsSchemasRecordCreateRequestItem
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`formKey` <- this_object$`formKey`
      self$`formId` <- this_object$`formId`
      self$`siteName` <- this_object$`siteName`
      self$`siteId` <- this_object$`siteId`
      self$`subjectKey` <- this_object$`subjectKey`
      self$`subjectId` <- this_object$`subjectId`
      self$`subjectOid` <- this_object$`subjectOid`
      self$`intervalName` <- this_object$`intervalName`
      self$`intervalId` <- this_object$`intervalId`
      self$`recordId` <- this_object$`recordId`
      self$`recordOid` <- this_object$`recordOid`
      self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "map(AnyType)", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to ComponentsSchemasRecordCreateRequestItem and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `formKey`
      if (!is.null(input_json$`formKey`)) {
        if (!(is.character(input_json$`formKey`) && length(input_json$`formKey`) == 1)) {
          stop(paste("Error! Invalid data for `formKey`. Must be a string:", input_json$`formKey`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for ComponentsSchemasRecordCreateRequestItem: the required field `formKey` is missing."))
      }
      # check the required field `data`
      if (!is.null(input_json$`data`)) {
        stopifnot(is.vector(input_json$`data`), length(input_json$`data`) != 0)
        tmp <- sapply(input_json$`data`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for ComponentsSchemasRecordCreateRequestItem: the required field `data` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of ComponentsSchemasRecordCreateRequestItem
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `formKey` is null
      if (is.null(self$`formKey`)) {
        return(FALSE)
      }

      # check if the required `data` is null
      if (is.null(self$`data`)) {
        return(FALSE)
      }

      TRUE
    },

    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    getInvalidFields = function() {
      invalid_fields <- list()
      # check if the required `formKey` is null
      if (is.null(self$`formKey`)) {
        invalid_fields["formKey"] <- "Non-nullable required field `formKey` cannot be null."
      }

      # check if the required `data` is null
      if (is.null(self$`data`)) {
        invalid_fields["data"] <- "Non-nullable required field `data` cannot be null."
      }

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
# ComponentsSchemasRecordCreateRequestItem$unlock()
#
## Below is an example to define the print function
# ComponentsSchemasRecordCreateRequestItem$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# ComponentsSchemasRecordCreateRequestItem$lock()

