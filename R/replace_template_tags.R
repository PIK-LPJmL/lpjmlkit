
# Function to replace a template tag, convention here: <<tag_name>>. Recursive
#   style to handle multiple tag lists
#   x has to be a character (vector) and tags a list with named tag replacements
replace_template_tags <- function(x, tags) {
  . <- NULL
  x <- tags[1] %>%
    gsub(pattern = paste0("<<", names(.), ">>"),
         replacement = .,
         x = x)
  if (length(tags) > 1) {
    return(replace_template_tags(x, tags[c(-1)]))
  } else {
    return(x)
  }
}
