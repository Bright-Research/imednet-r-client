# openapi::Interval


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**studyKey** | **character** | Unique study key | [optional] 
**intervalId** | **integer** | Unique system identifier for the interval (visit definition) | [optional] 
**intervalName** | **character** | Name of the interval (visit) as defined in the study | [optional] 
**intervalDescription** | **character** | Description of the interval (visit) | [optional] 
**intervalSequence** | **integer** | Sequence number of the interval in the schedule | [optional] 
**intervalGroupId** | **integer** | Identifier for the interval group (if intervals are grouped) | [optional] 
**intervalGroupName** | **character** | Name of the interval group | [optional] 
**timeline** | **character** | Type of interval visit window (e.g., None, Due Date, Start - End Date, Actual Date) | [optional] 
**definedUsingInterval** | **character** | Baseline interval used for calculating this interval’s dates | [optional] 
**windowCalculationForm** | **character** | Baseline form (name) from which the calculation date is taken | [optional] 
**windowCalculationDate** | **character** | Baseline field (variable name) from which the calculation date is taken | [optional] 
**actualDateForm** | **character** | Form used to capture the actual date for this interval | [optional] 
**actualDate** | **character** | Field (variable name) used to capture the actual date for this interval | [optional] 
**dueDateWillBeIn** | **integer** | Number of days from the calculation date when the interval is due | [optional] 
**negativeSlack** | **integer** | Number of days before the due date that are allowed (negative window) | [optional] 
**positiveSlack** | **integer** | Number of days after the due date that are allowed (positive window) | [optional] 
**eproGracePeriod** | **integer** | Number of days of grace period for ePRO completion after due date | [optional] 
**forms** | [**array[ComponentsSchemasIntervalFormsItem]**](components_schemas_Interval_forms_item.md) | List of forms that are scheduled in this interval | [optional] 
**disabled** | **character** | Whether the interval is soft-deleted (disabled) | [optional] 
**dateCreated** | **character** | Date when this interval was created | [optional] 
**dateModified** | **character** | Date when this interval was last modified | [optional] 


