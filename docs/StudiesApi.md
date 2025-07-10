# StudiesApi

All URIs are relative to *https://edc.prod.imednetapi.com/api/v1/edc*

Method | HTTP request | Description
------------- | ------------- | -------------
[**ListStudies**](StudiesApi.md#ListStudies) | **GET** /studies | List studies accessible by API key


# **ListStudies**
> StudyList ListStudies(page = 0, size = 25, sort = var.sort, filter = var.filter)

List studies accessible by API key

### Example
```R
library(openapi)

# List studies accessible by API key
#
# prepare function argument(s)
var_page <- 0 # integer | Page index to retrieve (0-based) (Optional)
var_size <- 25 # integer | Number of items to return per page (max 500) (Optional)
var_sort <- "sort_example" # character | Sorting criteria in the format `property,ASC` or `property,DESC`. Can be repeated. (Optional)
var_filter <- "filter_example" # character | Optional filter criteria to apply, in the format `attribute==value` with support for >, >=, <, <=, !=. Multiple criteria can be combined with `;` (AND) or `,` (OR). (Optional)

api_instance <- StudiesApi$new()
# Configure API key authorization: apiKeyAuth
api_instance$api_client$api_keys["x-api-key"] <- Sys.getenv("API_KEY")
# Configure API key authorization: securityKeyAuth
# api_instance$api_client$api_keys["x-imn-security-key"] <- Sys.getenv("API_KEY")
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$ListStudies(page = var_page, size = var_size, sort = var_sort, filter = var_filterdata_file = "result.txt")
result <- api_instance$ListStudies(page = var_page, size = var_size, sort = var_sort, filter = var_filter)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **page** | **integer**| Page index to retrieve (0-based) | [optional] [default to 0]
 **size** | **integer**| Number of items to return per page (max 500) | [optional] [default to 25]
 **sort** | **character**| Sorting criteria in the format &#x60;property,ASC&#x60; or &#x60;property,DESC&#x60;. Can be repeated. | [optional] 
 **filter** | **character**| Optional filter criteria to apply, in the format &#x60;attribute&#x3D;&#x3D;value&#x60; with support for &gt;, &gt;&#x3D;, &lt;, &lt;&#x3D;, !&#x3D;. Multiple criteria can be combined with &#x60;;&#x60; (AND) or &#x60;,&#x60; (OR). | [optional] 

### Return type

[**StudyList**](StudyList.md)

### Authorization

[apiKeyAuth](../README.md#apiKeyAuth), [securityKeyAuth](../README.md#securityKeyAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful response with list of studies |  -  |
| **400** | Bad request (malformed or invalid input) |  -  |
| **401** | Authentication failed or was not provided |  -  |
| **403** | Authenticated but not allowed to access the resource |  -  |
| **404** | The requested resource was not found |  -  |
| **500** | Internal server error (unexpected condition encountered) |  -  |

