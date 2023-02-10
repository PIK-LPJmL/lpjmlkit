# Function to get gitlab commit hash of repository path.
#   Via include_url = TRUE url + hash is returned to be called via webbrowser.
get_git_urlhash <- function(path, include_url = TRUE, raise_error = TRUE) {

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

    ifelse(include_url, "echo ${url//de:/de/};", "echo ${hash};")
  )

  # System call
  out <- processx::run(command = "bash",
                       args = c("-c", inner_commands),
                       wd = path,
                       cleanup_tree = TRUE)

  # Check output, ignore errors
  if (out$stderr == "") {
    return(sub("\n", "", out$stdout))
  } else {

    if (raise_error) {
      stop(paste0("For path \"", path, "\": ", out$stderr), call. = FALSE)
    } else {
      warning(paste0("For path \"", path, "\": ", out$stderr), call. = FALSE)
      return("")
    }
  }
}
