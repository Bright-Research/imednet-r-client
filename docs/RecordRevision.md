# openapi::RecordRevision


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**studyKey** | **character** | Unique study key | [optional] 
**recordRevisionId** | **integer** | Unique system identifier for the record revision | [optional] 
**recordId** | **integer** | Unique system identifier of the related record | [optional] 
**recordOid** | **character** | Client-assigned record OID (if any) | [optional] 
**recordRevision** | **integer** | Revision number of the record (version of the record status) | [optional] 
**dataRevision** | **integer** | Revision number of the data within the record | [optional] 
**recordStatus** | **character** | Status of the record at this revision (user-defined status label) | [optional] 
**subjectId** | **integer** | Mednet subject ID related to the record | [optional] 
**subjectOid** | **character** | Client-assigned subject OID related to the record | [optional] 
**subjectKey** | **character** | Subject display ID related to the record | [optional] 
**siteId** | **integer** | Site ID related to the record | [optional] 
**formKey** | **character** | Form key of the form that the record belongs to | [optional] 
**intervalId** | **integer** | Interval ID (visit definition) related to the record | [optional] 
**role** | **character** | Role name of the user who saved the record revision | [optional] 
**user** | **character** | Username of the user who saved the record revision | [optional] 
**reasonForChange** | **character** | Reason for change, if provided (for audit trail) | [optional] 
**deleted** | **character** | Whether the record was deleted in this revision | [optional] 
**dateCreated** | **character** | Date when this record revision was created | [optional] 


