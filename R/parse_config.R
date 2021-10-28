# Function to run cpp precompiler on lpjml.js to parse config.json
#  define from_restart and if further macros required a macro
parse_config <- function(path,
                         from_restart = FALSE,
                         js_filename="lpjml.js",
                         macro = "",
                         test_file = FALSE) {
  if (!test_file) {
       # processx::run kills any occuring subprocesses to avoid fork bombs
       tmp_json <- processx::run(command = "sh",
                                 args = c(
                                   "-c",
                                   paste0("cpp -P ./",
                                          js_filename,
                                          ifelse(from_restart,
                                                 " -DFROM_RESTART ",
                                                 " "),
                                          paste(macro, collapse = " "))
                                 ),
                                 wd = path,
                                 cleanup_tree = TRUE)$stdout %>%
         jsonlite::parse_json(simplify = FALSE)
  } else {
    test_json <- read_config("../testdata/test_config.json")
    return(test_json)
  }
}
