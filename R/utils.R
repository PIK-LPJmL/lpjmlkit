# Collection of small utility function applied across the package

# Function to deprecate a function argument that is replaced by a new one
deprecate_arg <- function(new_arg, deprec_arg, version = "1.0.0") {

  new_name <- deparse(substitute(new_arg))
  deprec_name <- deparse(substitute(deprec_arg))

  # Only if deprecated argument is set
  if (!is.null(deprec_arg)) {

    if (is.null(new_arg)) {
      new_arg <- deprec_arg

    } else {
      warning(
        "Argument `", deprec_name, "` will be ignored in favour of argument `",
        new_name, "`."
      )
    }

    message(
      "Argument `", deprec_name, "` is deprecated as of version ", version, ";",
      " use `", new_name, "` instead."
    )
  }

  new_arg
}


# Utility function to replace missing attribute with default value
default <- function(value, default) {
  if (is.null(value)) {
    return(default)
  } else {
    return(value)
  }
}


# Drop dimensions of length 1 except those that are selected by name
drop_omit <- function(x, omit_dim) {
  dims <- dim(x)
  dims_check <- dims == 1 & !(names(dims) %in% omit_dim)

  abind::adrop(x, dims_check)
}


# Function to get list names recursively
names_recursively <- function(x) {

  # Standard names of list elements
  y <- names(x)

  # Recursion for list elements
  y_append <- sapply(x, function(y) { # nolint:undesirable_function_linter.
    if (is.list(y))
      return(names_recursively(y))
  }) %>% unlist(use.names = FALSE)

  # Only return unique elements
  append(y, y_append) %>%
    unique() %>%
    return()
}


# colorize variable name for messages, warning, stop
col_var <- function(x) {
  col_blue <- "\u001b[34m"
  unset_col <- "\u001b[0m"
  paste0(col_blue, x, unset_col)
}


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


# Function checks and returns whether SLURM is available
is_slurm_available <- function() {
  processx::run(command = "sh",
                args = c("-c", "sinfo"),
                error_on_status = FALSE) %>%
  .$status == 0 %>%
    return()
}


# file_type options supported by read_io
supported_types <- c("raw", "clm", "meta")


# Avoid note for "."...
utils::globalVariables(".") # nolint:undesirable_function_linter
