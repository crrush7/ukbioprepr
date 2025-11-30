library(testthat)
library(ukbioprepr)
# Increase timeout for download-heavy tests
options(timeout = 600)  # 10 minutes instead of default 60 seconds
test_check("ukbioprepr")
