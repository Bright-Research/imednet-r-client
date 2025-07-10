# openapi::Variable


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**studyKey** | **character** | Unique study key | [optional] 
**variableId** | **integer** | Mednet variable ID | [optional] 
**variableType** | **character** | Type of the variable (field type), e.g., RADIO, TEXT, etc. | [optional] [Enum: [TEXT, TEXTAREA, RADIO, CHECKBOX, DROPDOWN, DATE, NUMBER]] 
**variableName** | **character** | Name of the variable (question text or label) | [optional] 
**sequence** | **integer** | Sequence of the variable on the form | [optional] 
**revision** | **integer** | Number of modifications of the variable (via form metadata revisions) | [optional] 
**disabled** | **character** | Whether the variable is marked as disabled (deleted) | [optional] 
**dateCreated** | **character** | Date when this variable was created | [optional] 
**dateModified** | **character** | Date when this variable was last modified | [optional] 
**formId** | **integer** | ID of the form that this variable belongs to | [optional] 
**variableOid** | **character** | Client-assigned variable OID | [optional] 
**deleted** | **character** | Whether the variable is marked as deleted | [optional] 
**formKey** | **character** | Form key of the form that this variable belongs to | [optional] 
**formName** | **character** | Name of the form that this variable belongs to | [optional] 
**label** | **character** | User-defined identifier (field name) for the variable | [optional] 
**blinded** | **character** | Whether the variable is flagged as blinded (hidden in certain contexts) | [optional] 


