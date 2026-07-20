library(ipumsr)
if (Sys.getenv("IPUMS_API_KEY") == "") stop("Set the IPUMS_API_KEY environment variable to your IPUMS API key (see README.md)")

# Check extract 6 status using ipumsr internals
cat("Checking extract 6...\n")
info <- get_extract_info(c("cps", 6))
cat("Status:", info$status, "\n")
cat("Class:", paste(class(info), collapse=", "), "\n")

# Check if it's ready
cat("is_extract_ready:", is_extract_ready(info), "\n")

# Try extract 5 too
cat("\nChecking extract 5...\n")
info5 <- get_extract_info(c("cps", 5))
cat("Status:", info5$status, "\n")
cat("is_extract_ready:", is_extract_ready(info5), "\n")
