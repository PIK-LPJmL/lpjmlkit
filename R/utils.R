# Collection of small utility function applied across the package

# Function to deprecate a function argument that is replaced by a new one
deprecate_arg <- function(new_arg,
                          deprec_arg,
                          version = "1.0.0",
                          ignore_new_arg = FALSE) {

  new_name <- deparse(substitute(new_arg))
  deprec_name <- deparse(substitute(deprec_arg))

  # Only if deprecated argument is set
  if (!is.null(deprec_arg)) {

    if (is.null(new_arg) || ignore_new_arg) {
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

# function to merge two lists with priority to first list
# list items from secondary are only preferred if they are NULL or not in priority 
merge_lists <- function(priority, secondary) {
  merged_lists <- priority
  for (item in names(secondary)) {
    if (!is.null(secondary[[item]])) {
      if (is.null(priority[[item]])) {
        merged_lists[[item]] <- secondary[[item]]
      }
    }
  }
  merged_lists
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


# colorize variable name for messages, warning, stop in blue
col_var <- function(x) {
  cli::col_blue(x)
}

# colorize notes in yellow
col_note <- function(x) {
  cli::col_yellow(x)
}

# colorize warnings and important strings in red
col_warn <- function(x) {
  cli::col_red(x)
}

# bold strings, especially for headers of messages
bold_head <- function(x) {
  cli::style_bold(x)
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
  processx::run(
    command = "bash",
    args = c("-c", "sinfo"),
    error_on_status = FALSE
  ) %>%
    .$status == 0 %>%
    return()
}

# Warn if OS is windows and thus not unix-based (ignoring other non unix based
# OS)
warn_runner_os <- function(fun_name) {
  if (is_os_windows()) {
    warning(
      "`", fun_name, "()` is not supported on non-Unix-based operating systems."
    )
  }
}


# Check if OS is Windows
is_os_windows <- function() {
  # Check if operating system is Windows
  ifelse(tolower(Sys.info()[["sysname"]]) == "windows", TRUE, FALSE)
}


# file_type options supported by read_io
supported_types <- c("raw", "clm", "meta", "cdf")

# band_names in reservoir files (special LPJmL file type)
band_names_reservoir <- c(
  "year", "capacity", "area", "inst_cap", "height",
  paste0("purpose", seq_len(5))
)


# Avoid note for "."...
utils::globalVariables(".") # nolint:undesirable_function_linter

# Stop if user has supplied ellipsis argument. Used to test if user tries to set
# active bindings directly.
check_change <- function(self, att, ...) {
  if (...length() > 0) {
    stop(
      sQuote(att, q = FALSE), " attribute cannot be set directly in ",
      class(self)[1],
      " object.",
      call. = FALSE
    )
  }
}

# check and if required transpose longitude and/or latitude axis
transpose_lon_lat <- function(x) {
  if (length(dimnames(x)[["lon"]]) > 1) {
    # check for correct order of longitudes (180°W to 180°E: - to +)
    if (mean(diff(as.numeric(dimnames(x)[["lon"]]))) < 0) {
      x <- subset_array(
        x,
        subset_list = list(
          lon = rev(seq_len(dim(x)[["lon"]]))
        ),
        drop = FALSE
      )
    } else {
      x
    }
  }
  if (length(dimnames(x)[["lat"]]) > 1) {
    # check for correct order of latitudes (90°N to 90°S: + to -)
    if (mean(diff(as.numeric(dimnames(x)[["lat"]]))) > 0) {
      x <- subset_array(
        x,
        subset_list = list(
          lat = rev(seq_len(dim(x)[["lat"]]))
        ),
        drop = FALSE
      )
    }
  }
  x
}