library(vcr)

vcr_configure(
  dir = "fixtures",
  filter_sensitive_data = list(
    "<REDACTED_API_KEY>" = Sys.getenv("IMEDNET_API_KEY"),
    "<REDACTED_BEARER_TOKEN>" = Sys.getenv("IMEDNET_BEARER_TOKEN")
  ),
  filter_request_headers = c("Authorization", "x-api-key", "x-imn-security-key")
)
