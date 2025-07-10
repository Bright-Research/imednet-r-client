# openapi::Subject


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**studyKey** | **character** | Unique study key | [optional] 
**subjectId** | **integer** | Mednet subject ID (internal numeric ID) | [optional] 
**subjectOid** | **character** | Client-assigned subject object identifier (OID) | [optional] 
**subjectKey** | **character** | Protocol-assigned subject identifier (display ID) | [optional] 
**subjectStatus** | **character** | Current status of the subject (e.g., Enrolled) | [optional] 
**siteId** | **integer** | Mednet site ID the subject is associated with | [optional] 
**siteName** | **character** | Name of the site the subject is associated with | [optional] 
**deleted** | **character** | Whether the subject is marked as deleted | [optional] 
**enrollmentStartDate** | **character** | Subject’s enrollment start date | [optional] 
**dateCreated** | **character** | Date when this subject record was created | [optional] 
**dateModified** | **character** | Date when this subject record was last modified | [optional] 
**keywords** | [**array[Keyword]**](Keyword.md) | List of keywords associated with the subject | [optional] 


