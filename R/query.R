#' Create a new Query
#'
#' @description
#' Query Class
#'
#' @docType class
#' @title Query
#' @description Query Class
#' @format An \code{R6Class} generator object
#' @field studyKey Unique study key character [optional]
#' @field subjectId Mednet subject ID associated with the query (if applicable) integer [optional]
#' @field subjectOid Client-assigned subject OID (if applicable) character [optional]
#' @field annotationType Category/type of the query (e.g., subject, record, question) character [optional]
#' @field annotationId Unique system identifier for the query integer [optional]
#' @field type System text identifier for the query type/location (subject, record, question) character [optional]
#' @field description Description of the query (e.g., reason or context) character [optional]
#' @field recordId Record ID associated with the query (if applicable) integer [optional]
#' @field variable Variable name (field) associated with the query (if applicable) character [optional]
#' @field subjectKey Subject display ID associated with the query (if applicable) character [optional]
#' @field dateCreated Date when the query was created character [optional]
#' @field dateModified Date when the query was last modified character [optional]
#' @field queryComments History of comments/actions on the query list(\link{QueryComment}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Query <- R6::R6Class(
  "Query",
  public = list(
    `studyKey` = NULL,
    `subjectId` = NULL,
    `subjectOid` = NULL,
    `annotationType` = NULL,
    `annotationId` = NULL,
    `type` = NULL,
    `description` = NULL,
    `recordId` = NULL,
    `variable` = NULL,
    `subjectKey` = NULL,
    `dateCreated` = NULL,
    `dateModified` = NULL,
    `queryComments` = NULL,

    #' @description
    #' Initialize a new Query class.
    #'
    #' @param studyKey Unique study key
    #' @param subjectId Mednet subject ID associated with the query (if applicable)
    #' @param subjectOid Client-assigned subject OID (if applicable)
    #' @param annotationType Category/type of the query (e.g., subject, record, question)
    #' @param annotationId Unique system identifier for the query
    #' @param type System text identifier for the query type/location (subject, record, question)
    #' @param description Description of the query (e.g., reason or context)
    #' @param recordId Record ID associated with the query (if applicable)
    #' @param variable Variable name (field) associated with the query (if applicable)
    #' @param subjectKey Subject display ID associated with the query (if applicable)
    #' @param dateCreated Date when the query was created
    #' @param dateModified Date when the query was last modified
    #' @param queryComments History of comments/actions on the query
    #' @param ... Other optional arguments.
    initialize = function(`studyKey` = NULL, `subjectId` = NULL, `subjectOid` = NULL, `annotationType` = NULL, `annotationId` = NULL, `type` = NULL, `description` = NULL, `recordId` = NULL, `variable` = NULL, `subjectKey` = NULL, `dateCreated` = NULL, `dateModified` = NULL, `queryComments` = NULL, ...) {
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
      if (!is.null(`annotationType`)) {
        if (!(is.character(`annotationType`) && length(`annotationType`) == 1)) {
          stop(paste("Error! Invalid data for `annotationType`. Must be a string:", `annotationType`))
        }
        self$`annotationType` <- `annotationType`
      }
      if (!is.null(`annotationId`)) {
        if (!(is.numeric(`annotationId`) && length(`annotationId`) == 1)) {
          stop(paste("Error! Invalid data for `annotationId`. Must be an integer:", `annotationId`))
        }
        self$`annotationId` <- `annotationId`
      }
      if (!is.null(`type`)) {
        if (!(is.character(`type`) && length(`type`) == 1)) {
          stop(paste("Error! Invalid data for `type`. Must be a string:", `type`))
        }
        self$`type` <- `type`
      }
      if (!is.null(`description`)) {
        if (!(is.character(`description`) && length(`description`) == 1)) {
          stop(paste("Error! Invalid data for `description`. Must be a string:", `description`))
        }
        self$`description` <- `description`
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
      if (!is.null(`subjectKey`)) {
        if (!(is.character(`subjectKey`) && length(`subjectKey`) == 1)) {
          stop(paste("Error! Invalid data for `subjectKey`. Must be a string:", `subjectKey`))
        }
        self$`subjectKey` <- `subjectKey`
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
      if (!is.null(`queryComments`)) {
        stopifnot(is.vector(`queryComments`), length(`queryComments`) != 0)
        sapply(`queryComments`, function(x) stopifnot(R6::is.R6(x)))
        self$`queryComments` <- `queryComments`
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
    #' @return Query as a base R list.
    #' @examples
    #' # convert array of Query (x) to a data frame
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
    #' Convert Query to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      QueryObject <- list()
      if (!is.null(self$`studyKey`)) {
        QueryObject[["studyKey"]] <-
          self$`studyKey`
      }
      if (!is.null(self$`subjectId`)) {
        QueryObject[["subjectId"]] <-
          self$`subjectId`
      }
      if (!is.null(self$`subjectOid`)) {
        QueryObject[["subjectOid"]] <-
          self$`subjectOid`
      }
      if (!is.null(self$`annotationType`)) {
        QueryObject[["annotationType"]] <-
          self$`annotationType`
      }
      if (!is.null(self$`annotationId`)) {
        QueryObject[["annotationId"]] <-
          self$`annotationId`
      }
      if (!is.null(self$`type`)) {
        QueryObject[["type"]] <-
          self$`type`
      }
      if (!is.null(self$`description`)) {
        QueryObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`recordId`)) {
        QueryObject[["recordId"]] <-
          self$`recordId`
      }
      if (!is.null(self$`variable`)) {
        QueryObject[["variable"]] <-
          self$`variable`
      }
      if (!is.null(self$`subjectKey`)) {
        QueryObject[["subjectKey"]] <-
          self$`subjectKey`
      }
      if (!is.null(self$`dateCreated`)) {
        QueryObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateModified`)) {
        QueryObject[["dateModified"]] <-
          self$`dateModified`
      }
      if (!is.null(self$`queryComments`)) {
        QueryObject[["queryComments"]] <-
          lapply(self$`queryComments`, function(x) x$toSimpleType())
      }
      return(QueryObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Query
    #'
    #' @param input_json the JSON input
    #' @return the instance of Query
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
      if (!is.null(this_object$`annotationType`)) {
        self$`annotationType` <- this_object$`annotationType`
      }
      if (!is.null(this_object$`annotationId`)) {
        self$`annotationId` <- this_object$`annotationId`
      }
      if (!is.null(this_object$`type`)) {
        self$`type` <- this_object$`type`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`recordId`)) {
        self$`recordId` <- this_object$`recordId`
      }
      if (!is.null(this_object$`variable`)) {
        self$`variable` <- this_object$`variable`
      }
      if (!is.null(this_object$`subjectKey`)) {
        self$`subjectKey` <- this_object$`subjectKey`
      }
      if (!is.null(this_object$`dateCreated`)) {
        self$`dateCreated` <- this_object$`dateCreated`
      }
      if (!is.null(this_object$`dateModified`)) {
        self$`dateModified` <- this_object$`dateModified`
      }
      if (!is.null(this_object$`queryComments`)) {
        self$`queryComments` <- ApiClient$new()$deserializeObj(this_object$`queryComments`, "array[QueryComment]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Query in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Query
    #'
    #' @param input_json the JSON input
    #' @return the instance of Query
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`studyKey` <- this_object$`studyKey`
      self$`subjectId` <- this_object$`subjectId`
      self$`subjectOid` <- this_object$`subjectOid`
      self$`annotationType` <- this_object$`annotationType`
      self$`annotationId` <- this_object$`annotationId`
      self$`type` <- this_object$`type`
      self$`description` <- this_object$`description`
      self$`recordId` <- this_object$`recordId`
      self$`variable` <- this_object$`variable`
      self$`subjectKey` <- this_object$`subjectKey`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateModified` <- this_object$`dateModified`
      self$`queryComments` <- ApiClient$new()$deserializeObj(this_object$`queryComments`, "array[QueryComment]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to Query and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Query
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
# Query$unlock()
#
## Below is an example to define the print function
# Query$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Query$lock()

