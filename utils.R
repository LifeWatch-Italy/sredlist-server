firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))