# openapi::Visit


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**studyKey** | **character** | Unique study key | [optional] 
**visitId** | **integer** | Unique system identifier for the subject visit instance | [optional] 
**intervalId** | **integer** | Unique system identifier of the interval definition for this visit | [optional] 
**intervalName** | **character** | Name of the interval (visit) for this visit instance | [optional] 
**subjectId** | **integer** | Mednet subject ID of the subject | [optional] 
**subjectKey** | **character** | Protocol-assigned subject identifier | [optional] 
**startDate** | **character** | Start date of the visit window (in YYYY-MM-DD format) | [optional] 
**endDate** | **character** | End date of the visit window (in YYYY-MM-DD format) | [optional] 
**dueDate** | **character** | Due date of the visit (in YYYY-MM-DD format), if applicable | [optional] 
**visitDate** | **character** | Actual date the visit took place (in YYYY-MM-DD format) | [optional] 
**visitDateForm** | **character** | Name of the form used to capture the actual visit date | [optional] 
**visitDateQuestion** | **character** | Variable name of the field capturing the actual visit date | [optional] 
**deleted** | **character** | Whether the visit instance is marked as deleted | [optional] 
**dateCreated** | **character** | Date when this visit record was created | [optional] 
**dateModified** | **character** | Date when this visit record was last modified | [optional] 


