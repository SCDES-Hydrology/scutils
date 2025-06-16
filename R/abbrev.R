## Function to abbreviate things
#' @export
abbrev <- function(str, pat, rep) {
  stringr::str_replace_all(str, regex(pat, ignore_case=TRUE), rep) }

#' @export
abbreviate_year <- function(year) {
  year %>%
    as.integer() %>%
    round() %>%
    as.character() %>%
    dplyr::if_else(nchar(.)==4,
                   stringr::str_sub(., 3) %>%
                     paste0("'", .),
                   .)
}

# Nick: Take the input year, make it an integer, then round it to the nearest whole number before changing it
# to be a character. From there, if the number of characters is equal to 4 then take the first 3 characters.
# If not equal to 4, then paste a `'` in front of all the characters

# The regex(pat, ignore_case = TRUE) is a an expression that allows you can match patterns in strings and ignore
# instances where there is not a pattern
