# RecordsApi

All URIs are relative to *https://edc.prod.imednetapi.com/api/v1/edc*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateRecords**](RecordsApi.md#CreateRecords) | **POST** /studies/{studyKey}/records | Add new record or update subject/record data
[**ListRecords**](RecordsApi.md#ListRecords) | **GET** /studies/{studyKey}/records | List records (eCRF instances) in a study


# **CreateRecords**
> RecordJobStatus CreateRecords(study_key, components_schemas_record_create_request_item)

Add new record or update subject/record data

### Example
```R
library(openapi)

# Add new record or update subject/record data
#
# prepare function argument(s)
var_study_key <- "study_key_example" # character | Study key identifying the study context for the request
var_components_schemas_record_create_request_item <- c(components_schemas_RecordCreateRequest_item$new("formKey_example", c(key = TODO), 123, "siteName_example", 123, "subjectKey_example", 123, "subjectOid_example", "intervalName_example", 123, 123, "recordOid_example")) # array[ComponentsSchemasRecordCreateRequestItem] | An array of record objects defining the operation to perform (subject registration, record creation, or scheduled record update).

api_instance <- RecordsApi$new()
# Configure API key authorization: apiKeyAuth
api_instance$api_client$api_keys["x-api-key"] <- Sys.getenv("API_KEY")
# Configure API key authorization: securityKeyAuth
# api_instance$api_client$api_keys["x-imn-security-key"] <- Sys.getenv("API_KEY")
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$CreateRecords(var_study_key, var_components_schemas_record_create_request_itemdata_file = "result.txt")
result <- api_instance$CreateRecords(var_study_key, var_components_schemas_record_create_request_item)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **study_key** | **character**| Study key identifying the study context for the request | 
 **components_schemas_record_create_request_item** | list( [**ComponentsSchemasRecordCreateRequestItem**](components_schemas_RecordCreateRequest_item.md) )| An array of record objects defining the operation to perform (subject registration, record creation, or scheduled record update). | 

### Return type

[**RecordJobStatus**](RecordJobStatus.md)

### Authorization

[apiKeyAuth](../README.md#apiKeyAuth), [securityKeyAuth](../README.md#securityKeyAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **202** | Accepted request for record creation. Returns a job identifier to check status. |  -  |
| **400** | Bad request (malformed or invalid input) |  -  |
| **401** | Authentication failed or was not provided |  -  |
| **403** | Authenticated but not allowed to access the resource |  -  |
| **404** | The requested resource was not found |  -  |
| **429** | Too many requests (rate limit exceeded) |  -  |
| **500** | Internal server error (unexpected condition encountered) |  -  |

# **ListRecords**
> RecordList ListRecords(study_key, page = 0, size = 25, sort = var.sort, filter = var.filter, record_data_filter = var.record_data_filter)

List records (eCRF instances) in a study

### Example
```R
library(openapi)

# List records (eCRF instances) in a study
#
# prepare function argument(s)
var_study_key <- "study_key_example" # character | Study key identifying the study context for the request
var_page <- 0 # integer | Page index to retrieve (0-based) (Optional)
var_size <- 25 # integer | Number of items to return per page (max 500) (Optional)
var_sort <- "sort_example" # character | Sorting criteria in the format `property,ASC` or `property,DESC`. Can be repeated. (Optional)
var_filter <- "filter_example" # character | Optional filter criteria to apply, in the format `attribute==value` with support for >, >=, <, <=, !=. Multiple criteria can be combined with `;` (AND) or `,` (OR). (Optional)
var_record_data_filter <- "record_data_filter_example" # character | Optional filter criteria to apply on recordData fields (question responses). Supports ==, !=, <, <=, >, >=, =~ (contains) and can combine multiple criteria with `;` (AND) or `,` (OR). (Optional)

api_instance <- RecordsApi$new()
# Configure API key authorization: apiKeyAuth
api_instance$api_client$api_keys["x-api-key"] <- Sys.getenv("API_KEY")
# Configure API key authorization: securityKeyAuth
# api_instance$api_client$api_keys["x-imn-security-key"] <- Sys.getenv("API_KEY")
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$ListRecords(var_study_key, page = var_page, size = var_size, sort = var_sort, filter = var_filter, record_data_filter = var_record_data_filterdata_file = "result.txt")
result <- api_instance$ListRecords(var_study_key, page = var_page, size = var_size, sort = var_sort, filter = var_filter, record_data_filter = var_record_data_filter)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **study_key** | **character**| Study key identifying the study context for the request | 
 **page** | **integer**| Page index to retrieve (0-based) | [optional] [default to 0]
 **size** | **integer**| Number of items to return per page (max 500) | [optional] [default to 25]
 **sort** | **character**| Sorting criteria in the format &#x60;property,ASC&#x60; or &#x60;property,DESC&#x60;. Can be repeated. | [optional] 
 **filter** | **character**| Optional filter criteria to apply, in the format &#x60;attribute&#x3D;&#x3D;value&#x60; with support for &gt;, &gt;&#x3D;, &lt;, &lt;&#x3D;, !&#x3D;. Multiple criteria can be combined with &#x60;;&#x60; (AND) or &#x60;,&#x60; (OR). | [optional] 
 **record_data_filter** | **character**| Optional filter criteria to apply on recordData fields (question responses). Supports &#x3D;&#x3D;, !&#x3D;, &lt;, &lt;&#x3D;, &gt;, &gt;&#x3D;, &#x3D;~ (contains) and can combine multiple criteria with &#x60;;&#x60; (AND) or &#x60;,&#x60; (OR). | [optional] 

### Return type

[**RecordList**](RecordList.md)

### Authorization

[apiKeyAuth](../README.md#apiKeyAuth), [securityKeyAuth](../README.md#securityKeyAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful response with list of records |  -  |
| **400** | Bad request (malformed or invalid input) |  -  |
| **401** | Authentication failed or was not provided |  -  |
| **403** | Authenticated but not allowed to access the resource |  -  |
| **404** | The requested resource was not found |  -  |
| **500** | Internal server error (unexpected condition encountered) |  -  |

