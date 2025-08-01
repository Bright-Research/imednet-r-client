# Automatically generated by openapi-generator (https://openapi-generator.tech)
# Please update as you see appropriate

context("Test ComponentsSchemasRecordCreateRequestItem")

model_instance <- ComponentsSchemasRecordCreateRequestItem$new()

test_that("formKey", {
  # tests for the property `formKey` (character)
  # Form key identifying the eCRF to create or update

  # uncomment below to test the property
  #expect_equal(model.instance$`formKey`, "EXPECTED_RESULT")
})

test_that("formId", {
  # tests for the property `formId` (integer)
  # Form ID identifying the eCRF to create or update (alternative to formKey)

  # uncomment below to test the property
  #expect_equal(model.instance$`formId`, "EXPECTED_RESULT")
})

test_that("siteName", {
  # tests for the property `siteName` (character)
  # Name of the site where the record should be created (for new subject registration)

  # uncomment below to test the property
  #expect_equal(model.instance$`siteName`, "EXPECTED_RESULT")
})

test_that("siteId", {
  # tests for the property `siteId` (integer)
  # Site ID for the record (alternative to siteName)

  # uncomment below to test the property
  #expect_equal(model.instance$`siteId`, "EXPECTED_RESULT")
})

test_that("subjectKey", {
  # tests for the property `subjectKey` (character)
  # Subject identifier (display ID) for which to create or update the record

  # uncomment below to test the property
  #expect_equal(model.instance$`subjectKey`, "EXPECTED_RESULT")
})

test_that("subjectId", {
  # tests for the property `subjectId` (integer)
  # Subject ID for which to create or update the record (alternative to subjectKey)

  # uncomment below to test the property
  #expect_equal(model.instance$`subjectId`, "EXPECTED_RESULT")
})

test_that("subjectOid", {
  # tests for the property `subjectOid` (character)
  # Subject OID for which to create or update the record (alternative to subjectKey)

  # uncomment below to test the property
  #expect_equal(model.instance$`subjectOid`, "EXPECTED_RESULT")
})

test_that("intervalName", {
  # tests for the property `intervalName` (character)
  # Name of the interval (visit) for a scheduled record update

  # uncomment below to test the property
  #expect_equal(model.instance$`intervalName`, "EXPECTED_RESULT")
})

test_that("intervalId", {
  # tests for the property `intervalId` (integer)
  # Interval ID for a scheduled record update (alternative to intervalName)

  # uncomment below to test the property
  #expect_equal(model.instance$`intervalId`, "EXPECTED_RESULT")
})

test_that("recordId", {
  # tests for the property `recordId` (integer)
  # Record ID for updating an existing unscheduled record (if applicable)

  # uncomment below to test the property
  #expect_equal(model.instance$`recordId`, "EXPECTED_RESULT")
})

test_that("recordOid", {
  # tests for the property `recordOid` (character)
  # Record OID for updating an existing unscheduled record (if applicable)

  # uncomment below to test the property
  #expect_equal(model.instance$`recordOid`, "EXPECTED_RESULT")
})

test_that("data", {
  # tests for the property `data` (map(AnyType))
  # Key-value pairs of field names and values for the record data

  # uncomment below to test the property
  #expect_equal(model.instance$`data`, "EXPECTED_RESULT")
})
