#' Auto-paginate API requests
#'
#' @description
#' Transparently executes sequential requests to retrieve all pages of a dataset.
#'
#' @param api_func The API method to call
#' @param ... Arguments to pass to the API function
#'
#' @return A single list of all data items merged across all pages.
#' @export
auto_paginate <- function(api_func, ...) {
  page <- 0
  size <- 500 # max items per page

  result <- api_func(page = page, size = size, ...)

  if (is.null(result)) {
      return(list())
  }

  all_data <- if (!is.null(result$data)) result$data else list()

  if (!is.null(result$pagination) && !is.null(result$pagination$totalPages)) {
    total_pages <- result$pagination$totalPages

    if (total_pages > 1) {
      for (p in 1:(total_pages - 1)) {
        res <- api_func(page = p, size = size, ...)
        if (!is.null(res$data)) {
          all_data <- c(all_data, res$data)
        }
      }
    }
  }

  return(all_data)
}

#' Get all users
#' @param api_instance AdministrationApi instance
#' @param study_key Study key identifying the study context
#' @param sort Sorting criteria
#' @param include_inactive Whether to include inactive users
#' @param ... Additional arguments
#' @export
get_all_users <- function(api_instance, study_key, sort = NULL, include_inactive = FALSE, ...) {
  auto_paginate(api_instance$ListUsers, study_key = study_key, sort = sort, include_inactive = include_inactive, ...)
}

#' Get all codings
#' @param api_instance CodingsApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_codings <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListCodings, study_key = study_key, sort = sort, filter = filter, ...)
}

#' Get all forms
#' @param api_instance FormsApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_forms <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListForms, study_key = study_key, sort = sort, filter = filter, ...)
}

#' Get all intervals
#' @param api_instance IntervalsApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_intervals <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListIntervals, study_key = study_key, sort = sort, filter = filter, ...)
}

#' Get all queries
#' @param api_instance QueriesApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_queries <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListQueries, study_key = study_key, sort = sort, filter = filter, ...)
}

#' Get all record revisions
#' @param api_instance RecordRevisionsApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_record_revisions <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListRecordRevisions, study_key = study_key, sort = sort, filter = filter, ...)
}

#' Get all records
#' @param api_instance RecordsApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param record_data_filter Record data filter
#' @param ... Additional arguments
#' @export
get_all_records <- function(api_instance, study_key, sort = NULL, filter = NULL, record_data_filter = NULL, ...) {
  auto_paginate(api_instance$ListRecords, study_key = study_key, sort = sort, filter = filter, record_data_filter = record_data_filter, ...)
}

#' Get all sites
#' @param api_instance SitesApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_sites <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListSites, study_key = study_key, sort = sort, filter = filter, ...)
}

#' Get all studies
#' @param api_instance StudiesApi instance
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_studies <- function(api_instance, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListStudies, sort = sort, filter = filter, ...)
}

#' Get all subjects
#' @param api_instance SubjectsApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_subjects <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListSubjects, study_key = study_key, sort = sort, filter = filter, ...)
}

#' Get all variables
#' @param api_instance VariablesApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_variables <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListVariables, study_key = study_key, sort = sort, filter = filter, ...)
}

#' Get all visits
#' @param api_instance VisitsApi instance
#' @param study_key Study key
#' @param sort Sorting criteria
#' @param filter Filter criteria
#' @param ... Additional arguments
#' @export
get_all_visits <- function(api_instance, study_key, sort = NULL, filter = NULL, ...) {
  auto_paginate(api_instance$ListVisits, study_key = study_key, sort = sort, filter = filter, ...)
}
