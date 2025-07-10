# JobsApi

All URIs are relative to *https://edc.prod.imednetapi.com/api/v1/edc*

Method | HTTP request | Description
------------- | ------------- | -------------
[**GetJobStatus**](JobsApi.md#GetJobStatus) | **GET** /studies/{studyKey}/jobs/{batchId} | Retrieve job status by batch ID


# **GetJobStatus**
> Job GetJobStatus(study_key, batch_id)

Retrieve job status by batch ID

### Example
```R
library(openapi)

# Retrieve job status by batch ID
#
# prepare function argument(s)
var_study_key <- "study_key_example" # character | Study key identifying the study context for the request
var_batch_id <- "batch_id_example" # character | Batch ID of the job to retrieve

api_instance <- JobsApi$new()
# Configure API key authorization: apiKeyAuth
api_instance$api_client$api_keys["x-api-key"] <- Sys.getenv("API_KEY")
# Configure API key authorization: securityKeyAuth
# api_instance$api_client$api_keys["x-imn-security-key"] <- Sys.getenv("API_KEY")
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$GetJobStatus(var_study_key, var_batch_iddata_file = "result.txt")
result <- api_instance$GetJobStatus(var_study_key, var_batch_id)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **study_key** | **character**| Study key identifying the study context for the request | 
 **batch_id** | **character**| Batch ID of the job to retrieve | 

### Return type

[**Job**](Job.md)

### Authorization

[apiKeyAuth](../README.md#apiKeyAuth), [securityKeyAuth](../README.md#securityKeyAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful response with job status information |  -  |
| **400** | Bad request (malformed or invalid input) |  -  |
| **401** | Authentication failed or was not provided |  -  |
| **403** | Authenticated but not allowed to access the resource |  -  |
| **404** | The requested resource was not found |  -  |
| **500** | Internal server error (unexpected condition encountered) |  -  |

