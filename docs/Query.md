# openapi::Query


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**studyKey** | **character** | Unique study key | [optional] 
**subjectId** | **integer** | Mednet subject ID associated with the query (if applicable) | [optional] 
**subjectOid** | **character** | Client-assigned subject OID (if applicable) | [optional] 
**annotationType** | **character** | Category/type of the query (e.g., subject, record, question) | [optional] 
**annotationId** | **integer** | Unique system identifier for the query | [optional] 
**type** | **character** | System text identifier for the query type/location (subject, record, question) | [optional] 
**description** | **character** | Description of the query (e.g., reason or context) | [optional] 
**recordId** | **integer** | Record ID associated with the query (if applicable) | [optional] 
**variable** | **character** | Variable name (field) associated with the query (if applicable) | [optional] 
**subjectKey** | **character** | Subject display ID associated with the query (if applicable) | [optional] 
**dateCreated** | **character** | Date when the query was created | [optional] 
**dateModified** | **character** | Date when the query was last modified | [optional] 
**queryComments** | [**array[QueryComment]**](QueryComment.md) | History of comments/actions on the query | [optional] 


