# openapi::ComponentsSchemasRecordCreateRequestItem


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**formKey** | **character** | Form key identifying the eCRF to create or update | 
**formId** | **integer** | Form ID identifying the eCRF to create or update (alternative to formKey) | [optional] 
**siteName** | **character** | Name of the site where the record should be created (for new subject registration) | [optional] 
**siteId** | **integer** | Site ID for the record (alternative to siteName) | [optional] 
**subjectKey** | **character** | Subject identifier (display ID) for which to create or update the record | [optional] 
**subjectId** | **integer** | Subject ID for which to create or update the record (alternative to subjectKey) | [optional] 
**subjectOid** | **character** | Subject OID for which to create or update the record (alternative to subjectKey) | [optional] 
**intervalName** | **character** | Name of the interval (visit) for a scheduled record update | [optional] 
**intervalId** | **integer** | Interval ID for a scheduled record update (alternative to intervalName) | [optional] 
**recordId** | **integer** | Record ID for updating an existing unscheduled record (if applicable) | [optional] 
**recordOid** | **character** | Record OID for updating an existing unscheduled record (if applicable) | [optional] 
**data** | [**map(AnyType)**](AnyType.md) | Key-value pairs of field names and values for the record data | 


