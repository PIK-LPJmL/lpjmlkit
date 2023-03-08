# Function to get gitlab commit hash of repository path.
#   Via include_url = TRUE url + hash is returned to be called via webbrowser.
get_git_urlhash <- function(path = ".",
                            include_url = TRUE,
                            raise_error = TRUE) {

  # List of bash commands
  inner_commands <- paste0(
    # Filter .git in URL
    "git='.git';",

    # Get origin URL (with .git at ending, not legit for URL)
    "orig_url=$(git config --get remote.origin.url);",

    # Get commit hash
    "hash=$(git rev-parse HEAD);",

    # Some URLs include a cluster extension in before real URL starting with @
    "if [[ $orig_url == *'@'* ]]; then ",
    "orig_url=https://${orig_url##*@}; fi;",

    # Check for github or gitlab
    "if [[ $orig_url == *'github'* ]]; then ",

    # Concatenate URL and hash if required. Github has slightly different
    # routing.
    "url=${orig_url%%$git*}/tree/${hash}; else ",
    "url=${orig_url%%$git*}/-/tree/${hash}; fi;",

    # Replace ":" in URLs if SSH is used to clone repository
    "url=${url//de:/de/}; ",
    "url=${url//com:/com/}; ",
    ifelse(include_url, "echo ${url};", "echo ${hash};")
  )

  # System call
  out <- processx::run(command = "bash",
                       args = c("-c", inner_commands),
                       wd = path,
                       cleanup_tree = TRUE,
                       error_on_status = FALSE)

  # Check output, ignore errors
  if (out$stderr == "") {
    return(sub("\n", "", out$stdout))
  } else {

    if (raise_error) {
      stop("For path \"", path, "\": ", out$stderr, call. = FALSE)
    } else {
      warning("For path \"", path, "\": ", out$stderr, call. = FALSE)
      return("")
    }
  }
}
