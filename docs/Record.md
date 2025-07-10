# openapi::Record


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**studyKey** | **character** | Unique study key | [optional] 
**intervalId** | **integer** | Interval ID (visit definition) that this record is associated with | [optional] 
**formId** | **integer** | Form ID of the form this record instance belongs to | [optional] 
**formKey** | **character** | Form key of the form for this record instance | [optional] 
**siteId** | **integer** | Site ID associated with the record | [optional] 
**recordId** | **integer** | Unique record ID | [optional] 
**recordOid** | **character** | Client-assigned record OID | [optional] 
**recordType** | **character** | Type of record (e.g., SUBJECT for subject-related forms) | [optional] 
**recordStatus** | **character** | Current status of the record (e.g., Record Incomplete, Record Complete) | [optional] 
**deleted** | **character** | Whether the record is marked as deleted | [optional] 
**dateCreated** | **character** | Date when this record was created | [optional] 
**dateModified** | **character** | Date when this record was last modified | [optional] 
**subjectId** | **integer** | Mednet subject ID that this record is associated with | [optional] 
**subjectOid** | **character** | Client-assigned subject OID for the subject this record is associated with | [optional] 
**subjectKey** | **character** | Subject display ID that this record is associated with | [optional] 
**visitId** | **integer** | Visit instance ID that this record is associated with (if applicable) | [optional] 
**parentRecordId** | **integer** | Record ID of the parent record if this record is a subrecord (if applicable) | [optional] 
**keywords** | [**array[Keyword]**](Keyword.md) | List of keywords associated with the record | [optional] 
**recordData** | [**map(AnyType)**](AnyType.md) | Data fields and values captured in this record. The structure is dynamic, with keys corresponding to variable names. | [optional] 


