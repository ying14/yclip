




#' Inspect Clipboard
#'
#' @return
#' @export
#'
#' @examples
clipboard_inspect <- function() {
  if (.Platform$OS.type != "windows") {
    stop("clipboard_inspect() is currently only implemented on Windows.")
  }

  clipboard_inspect_windows()
}



#' Read Raw Data from Clipboard
#'
#' @param format_name the desired clipboard format. E.g., `"Rich Text Format"`
#'
#' @return
#' @export
#'
#' @examples
clipboard_read_raw <- function(format_name) {
  if (.Platform$OS.type != "windows") {
    stop("clipboard_read_raw() is currently only implemented on Windows.")
  }
  clipboard_read_raw_windows(format_name)
}


#' Read Dat from Clipboard
#'
#' @param format_name the desired clipboard format. E.g., `"Rich Text Format"`
#'
#' @return
#' @export
#'
#' @examples
clipboard_read_text <- function(format_name) {

  if (.Platform$OS.type != "windows") {
    stop("clipboard_read_text() is currently only implemented on Windows.")
  }

  if (format_name == "CF_UNICODETEXT") {
    return(clipboard_read_unicode_text_windows())
  }

  x <- clipboard_read_raw(format_name)
  rawToChar(x)
}



#' Read DIB Image Data from Clipboard
#'
#' @return A raw vector containing Windows DIB data.
#' @export
clipboard_read_dib <- function() {

  if (.Platform$OS.type != "windows") {
    stop("clipboard_read_dib() is currently only implemented on Windows.")
  }

  clipboard_read_dib_windows()
}


#' Inspect DIB Image on Clipboard
#'
#' @return A list containing DIB image metadata.
#' @export
clipboard_inspect_dib <- function() {

  if (.Platform$OS.type != "windows") {
    stop("clipboard_inspect_dib() is currently only implemented on Windows.")
  }

  clipboard_inspect_dib_windows()
}


parse_rtf_rows <- function(x) {

  stopifnot(length(x) == 1, is.character(x))

  matches <- stringr::str_locate_all(
    x,
    "\\\\trowd\\b|\\\\cell(?![a-zA-Z])|\\\\row\\b"
  )[[1]]

  if (nrow(matches) == 0) {
    return(list())
  }

  tokens <- stringr::str_sub(
    x,
    matches[, "start"],
    matches[, "end"]
  )

  rows <- list()
  current_row <- character()
  cell_start <- NULL

  for (i in seq_len(nrow(matches))) {

    token <- tokens[i]
    pos   <- matches[i, "start"]
    end   <- matches[i, "end"]

    if (token == "\\trowd") {

      current_row <- character()
      cell_start <- end + 1L

    } else if (token == "\\cell") {

      if (!is.null(cell_start)) {

        txt <- stringr::str_sub(
          x,
          cell_start,
          pos - 1L
        )

        current_row <- c(
          current_row,
          rtf_strip_simple(txt)
        )
      }

      cell_start <- end + 1L

    } else if (token == "\\row") {

      # Add any text between the final \cell and \row,
      # but only if it contains actual content.
      if (!is.null(cell_start)) {

        txt <- stringr::str_sub(
          x,
          cell_start,
          pos - 1L
        )

        txt <- rtf_strip_simple(txt)

        if (nzchar(txt)) {
          current_row <- c(current_row, txt)
        }
      }

      if (length(current_row) > 0) {
        rows[[length(rows) + 1L]] <- current_row
      }

      current_row <- character()
      cell_start <- NULL
    }
  }

  rows
}



parse_epic_tables <- function(rows) {

  stopifnot(is.list(rows))

  tables <- list()

  i <- 1L

  while (i <= length(rows)) {

    # A one-cell row is the table title
    if (length(rows[[i]]) != 1L) {
      i <- i + 1L
      next
    }

    title <- rows[[i]][[1]]

    # Need a following row containing column names
    if (i + 1L > length(rows)) {
      break
    }

    columns <- rows[[i + 1L]]

    # Find the rows belonging to this table.
    j <- i + 2L
    data_rows <- list()

    while (
      j <= length(rows) &&
      length(rows[[j]]) == length(columns)
    ) {
      data_rows[[length(data_rows) + 1L]] <- rows[[j]]
      j <- j + 1L
    }

    # Convert data rows to a data frame
    if (length(data_rows) > 0) {

      data <- do.call(
        rbind,
        data_rows
      )

      data <- as.data.frame(
        data,
        stringsAsFactors = FALSE
      )

      names(data) <- columns

    } else {

      data <- data.frame(
        setNames(
          replicate(
            length(columns),
            character(),
            simplify = FALSE
          ),
          columns
        ),
        stringsAsFactors = FALSE
      )
    }

    tables[[length(tables) + 1L]] <- list(
      title = title,
      data = data
    )

    i <- j
  }

  tables
}

