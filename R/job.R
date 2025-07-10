#' Create a new Job
#'
#' @description
#' Job Class
#'
#' @docType class
#' @title Job
#' @description Job Class
#' @format An \code{R6Class} generator object
#' @field jobId Unique job identifier character [optional]
#' @field batchId Batch ID associated with the job (useful for linking with record creation requests) character [optional]
#' @field state State of the job (e.g., completed, pending) character [optional]
#' @field dateCreated Timestamp when the job was created character [optional]
#' @field dateStarted Timestamp when the job started processing character [optional]
#' @field dateFinished Timestamp when the job finished processing character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Job <- R6::R6Class(
  "Job",
  public = list(
    `jobId` = NULL,
    `batchId` = NULL,
    `state` = NULL,
    `dateCreated` = NULL,
    `dateStarted` = NULL,
    `dateFinished` = NULL,

    #' @description
    #' Initialize a new Job class.
    #'
    #' @param jobId Unique job identifier
    #' @param batchId Batch ID associated with the job (useful for linking with record creation requests)
    #' @param state State of the job (e.g., completed, pending)
    #' @param dateCreated Timestamp when the job was created
    #' @param dateStarted Timestamp when the job started processing
    #' @param dateFinished Timestamp when the job finished processing
    #' @param ... Other optional arguments.
    initialize = function(`jobId` = NULL, `batchId` = NULL, `state` = NULL, `dateCreated` = NULL, `dateStarted` = NULL, `dateFinished` = NULL, ...) {
      if (!is.null(`jobId`)) {
        if (!(is.character(`jobId`) && length(`jobId`) == 1)) {
          stop(paste("Error! Invalid data for `jobId`. Must be a string:", `jobId`))
        }
        self$`jobId` <- `jobId`
      }
      if (!is.null(`batchId`)) {
        if (!(is.character(`batchId`) && length(`batchId`) == 1)) {
          stop(paste("Error! Invalid data for `batchId`. Must be a string:", `batchId`))
        }
        self$`batchId` <- `batchId`
      }
      if (!is.null(`state`)) {
        if (!(is.character(`state`) && length(`state`) == 1)) {
          stop(paste("Error! Invalid data for `state`. Must be a string:", `state`))
        }
        self$`state` <- `state`
      }
      if (!is.null(`dateCreated`)) {
        if (!(is.character(`dateCreated`) && length(`dateCreated`) == 1)) {
          stop(paste("Error! Invalid data for `dateCreated`. Must be a string:", `dateCreated`))
        }
        self$`dateCreated` <- `dateCreated`
      }
      if (!is.null(`dateStarted`)) {
        if (!(is.character(`dateStarted`) && length(`dateStarted`) == 1)) {
          stop(paste("Error! Invalid data for `dateStarted`. Must be a string:", `dateStarted`))
        }
        self$`dateStarted` <- `dateStarted`
      }
      if (!is.null(`dateFinished`)) {
        if (!(is.character(`dateFinished`) && length(`dateFinished`) == 1)) {
          stop(paste("Error! Invalid data for `dateFinished`. Must be a string:", `dateFinished`))
        }
        self$`dateFinished` <- `dateFinished`
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
    #' @return Job as a base R list.
    #' @examples
    #' # convert array of Job (x) to a data frame
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
    #' Convert Job to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      JobObject <- list()
      if (!is.null(self$`jobId`)) {
        JobObject[["jobId"]] <-
          self$`jobId`
      }
      if (!is.null(self$`batchId`)) {
        JobObject[["batchId"]] <-
          self$`batchId`
      }
      if (!is.null(self$`state`)) {
        JobObject[["state"]] <-
          self$`state`
      }
      if (!is.null(self$`dateCreated`)) {
        JobObject[["dateCreated"]] <-
          self$`dateCreated`
      }
      if (!is.null(self$`dateStarted`)) {
        JobObject[["dateStarted"]] <-
          self$`dateStarted`
      }
      if (!is.null(self$`dateFinished`)) {
        JobObject[["dateFinished"]] <-
          self$`dateFinished`
      }
      return(JobObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Job
    #'
    #' @param input_json the JSON input
    #' @return the instance of Job
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`jobId`)) {
        self$`jobId` <- this_object$`jobId`
      }
      if (!is.null(this_object$`batchId`)) {
        self$`batchId` <- this_object$`batchId`
      }
      if (!is.null(this_object$`state`)) {
        self$`state` <- this_object$`state`
      }
      if (!is.null(this_object$`dateCreated`)) {
        self$`dateCreated` <- this_object$`dateCreated`
      }
      if (!is.null(this_object$`dateStarted`)) {
        self$`dateStarted` <- this_object$`dateStarted`
      }
      if (!is.null(this_object$`dateFinished`)) {
        self$`dateFinished` <- this_object$`dateFinished`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Job in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Job
    #'
    #' @param input_json the JSON input
    #' @return the instance of Job
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`jobId` <- this_object$`jobId`
      self$`batchId` <- this_object$`batchId`
      self$`state` <- this_object$`state`
      self$`dateCreated` <- this_object$`dateCreated`
      self$`dateStarted` <- this_object$`dateStarted`
      self$`dateFinished` <- this_object$`dateFinished`
      self
    },

    #' @description
    #' Validate JSON input with respect to Job and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Job
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
# Job$unlock()
#
## Below is an example to define the print function
# Job$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Job$lock()

