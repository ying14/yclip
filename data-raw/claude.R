

# cf-html -----------------------------------------------------------------




# Windows' CF_HTML clipboard format wraps an HTML fragment in a plain-text
# header giving exact byte offsets of the whole document and of a
# "fragment" delimited by <!--StartFragment--> / <!--EndFragment--> comments.
# Applications (Word, Excel, browsers) read this header to know what part of
# the clipboard payload to actually paste. See:
# https://learn.microsoft.com/en-us/windows/win32/dataxchg/html-clipboard-format

#' Build a CF_HTML-formatted string from a bare HTML fragment
#'
#' @param fragment A string of HTML (e.g. `<table>...</table>`), without
#'   surrounding `<html>`/`<body>` tags.
#' @return A single string: the CF_HTML header + wrapped fragment, ready to
#'   hand to [write_clipboard_html()].
#' @keywords internal
#' @noRd
build_cf_html <- function(fragment) {
  pre  <- "<html>\n<body>\n<!--StartFragment-->"
  post <- "<!--EndFragment-->\n</body>\n</html>"
  html_body <- paste0(pre, fragment, post)

  header_template <- paste0(
    "Version:1.0\r\n",
    "StartHTML:%010d\r\n",
    "EndHTML:%010d\r\n",
    "StartFragment:%010d\r\n",
    "EndFragment:%010d\r\n"
  )

  # The header's byte length is fixed once the %d fields are zero-padded to
  # 10 digits, so we can compute offsets before knowing the final values.
  header_len <- nchar(sprintf(header_template, 0, 0, 0, 0), type = "bytes")

  start_html     <- header_len
  start_fragment <- start_html + nchar(pre, type = "bytes")
  end_fragment   <- start_fragment + nchar(fragment, type = "bytes")
  end_html       <- start_html + nchar(html_body, type = "bytes")

  header <- sprintf(header_template, start_html, end_html, start_fragment, end_fragment)
  paste0(header, html_body)
}

#' Strip the CF_HTML header back off, returning just the fragment
#'
#' Used on read: what comes back from `Get-Clipboard -TextFormatType Html`
#' is the full CF_HTML string (header + wrapped document), not bare HTML.
#' @keywords internal
#' @noRd
extract_html_fragment <- function(x) {
  if (!grepl("^Version:", x)) {
    return(x)  # already a bare fragment (seen from some non-Windows sources)
  }
  start <- regmatches(x, regexpr("(?<=StartFragment:)[0-9]+", x, perl = TRUE))
  end   <- regmatches(x, regexpr("(?<=EndFragment:)[0-9]+", x, perl = TRUE))
  if (length(start) == 0 || length(end) == 0) {
    stop("Clipboard HTML is missing StartFragment/EndFragment markers.", call. = FALSE)
  }
  bytes <- charToRaw(x)
  rawToChar(bytes[(as.integer(start) + 1):as.integer(end)])
}




# parse -------------------------------------------------------------------

# Converting raw clipboard payloads into R objects.

#' @keywords internal
#' @noRd
html_to_object <- function(html, as = c("auto", "gt", "tibble", "character")) {
  as <- match.arg(as)
  if (as == "character") return(html)

  fragment <- extract_html_fragment(html)
  doc <- rvest::read_html(fragment)
  tbls <- rvest::html_elements(doc, "table")

  if (length(tbls) == 0) {
    if (as == "gt") {
      stop("No <table> found in clipboard HTML; can't build a gt table.", call. = FALSE)
    }
    return(rvest::html_text2(doc))
  }

  df <- rvest::html_table(tbls[[1]], header = TRUE, fill = TRUE)

  if (as %in% c("gt", "auto") && requireNamespace("gt", quietly = TRUE)) {
    return(gt::gt(df))
  }
  if (as == "gt") {
    stop("Package 'gt' is required for as = \"gt\".", call. = FALSE)
  }
  tibble::as_tibble(df)
}

#' @keywords internal
#' @noRd
csv_to_object <- function(raw, as = c("auto", "tibble", "character")) {
  as <- match.arg(as)
  if (as == "character") return(raw)
  readr::read_csv(I(raw), show_col_types = FALSE)
}

#' @keywords internal
#' @noRd
rtf_to_text <- function(raw) {
  # Structural RTF table parsing (\trowd, \cellx, ...) has no mature R
  # implementation. We fall back to stripping formatting and returning text;
  # prefer format = "html" for tables, which is present alongside RTF for
  # anything copied from Word/Excel.
  if (requireNamespace("striprtf", quietly = TRUE)) {
    return(striprtf::read_rtf(textConnection(raw)))
  }
  raw
}

#' @keywords internal
#' @noRd
pick_best_format <- function(avail) {
  if (any(grepl("html", avail, ignore.case = TRUE))) return("html")
  if (any(grepl("rich text", avail, ignore.case = TRUE))) return("rtf")
  if (any(grepl("^csv$", avail, ignore.case = TRUE))) return("csv")
  "text"
}

#' @keywords internal
#' @noRd
object_to_html <- function(x) {
  if (inherits(x, "gt_tbl")) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Package 'gt' is required to write gt tables.", call. = FALSE)
    }
    return(gt::as_raw_html(x, inline_css = TRUE))
  }
  if (is.data.frame(x)) {
    if (requireNamespace("gt", quietly = TRUE)) {
      return(gt::as_raw_html(gt::gt(x), inline_css = TRUE))
    }
    if (requireNamespace("knitr", quietly = TRUE)) {
      return(knitr::kable(x, format = "html"))
    }
    stop("Writing a data frame requires either 'gt' or 'knitr'.", call. = FALSE)
  }
  if (is.character(x)) return(paste0("<p>", x, "</p>"))
  stop("Don't know how to convert an object of class ",
       paste(class(x), collapse = "/"), " to HTML.", call. = FALSE)
}


# windows-clipboard -------------------------------------------------------

# Low-level Windows clipboard access, shelled out to PowerShell / .NET.
#
# Base R's utils::readClipboard()/writeClipboard() only reach CF_TEXT and
# file-drop lists. Rich formats (CF_HTML, RTF) require either the raw Win32
# API or .NET's System.Windows.Forms.Clipboard class - we use the latter via
# PowerShell so the package stays free of compiled code.

is_windows <- function() {
  .Platform$OS.type == "windows"
}

#' Run a PowerShell command and return its stdout as a single string
#' @keywords internal
#' @noRd
ps_run <- function(command) {
  out <- suppressWarnings(
    system2("powershell",
            c("-NoProfile", "-NonInteractive", "-Command", command),
            stdout = TRUE, stderr = TRUE)
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("PowerShell call failed:\n", paste(out, collapse = "\n"), call. = FALSE)
  }
  paste(out, collapse = "\n")
}

#' List the clipboard formats currently available, without dumping content
#' @export
clip_formats_available <- function() {
  if (!is_windows()) {
    stop("clip_formats_available() currently only supports Windows.", call. = FALSE)
  }
  ps <- paste(
    "Add-Type -AssemblyName System.Windows.Forms;",
    "$d = [System.Windows.Forms.Clipboard]::GetDataObject();",
    "if ($null -eq $d) { '' } else { $d.GetFormats() -join ',' }"
  )
  out <- ps_run(ps)
  if (identical(out, "")) return(character(0))
  strsplit(out, ",", fixed = TRUE)[[1]]
}

#' Read a specific text-based clipboard format
#' @keywords internal
#' @noRd
read_clipboard_raw <- function(format = c("Html", "Rtf", "Text",
                                          "UnicodeText", "CommaSeparatedValue")) {
  format <- match.arg(format)
  if (!is_windows()) {
    stop("Rich clipboard formats are currently Windows-only.", call. = FALSE)
  }
  code <- sprintf("Get-Clipboard -TextFormatType %s -Raw", format)
  cli::cli_inform(code)
  ps_run(code)
}

#' Write a plain-text string to the clipboard
#' @keywords internal
#' @noRd
write_clipboard_text <- function(text) {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines(text, tmp, useBytes = TRUE)
  ps <- sprintf(
    "Set-Clipboard -Value ([IO.File]::ReadAllText('%s'))",
    normalizePath(tmp, winslash = "\\")
  )
  ps_run(ps)
  invisible(TRUE)
}

#' Write a CF_HTML-enveloped fragment to the clipboard
#' @keywords internal
#' @noRd
write_clipboard_html <- function(html_fragment) {
  cf_html <- build_cf_html(html_fragment)
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines(cf_html, tmp, useBytes = TRUE)
  ps <- sprintf(
    paste(
      "Add-Type -AssemblyName System.Windows.Forms;",
      "[System.Windows.Forms.Clipboard]::SetText([IO.File]::ReadAllText('%s'),",
      "[System.Windows.Forms.TextDataFormat]::Html)"
    ),
    normalizePath(tmp, winslash = "\\")
  )
  ps_run(ps)
  invisible(TRUE)
}




# clip --------------------------------------------------------------------

#' Read the clipboard into an R object
#'
#' Inspects what's actually on the clipboard and parses it into the
#' requested (or best-guess) R representation. HTML tables become `gt`
#' tables when the gt package is available; RTF is a lossy fallback.
#'
#' @param format One of `"auto"`, `"html"`, `"rtf"`, `"text"`, `"csv"`.
#'   `"auto"` inspects available formats and picks the richest usable one.
#' @param as Target R representation: `"auto"`, `"gt"`, `"tibble"`,
#'   `"character"`.
#' @return A `gt_tbl`, a tibble, or a character vector, depending on `as`
#'   and what was found on the clipboard.
#' @export
#' @examples
#' \dontrun{
#' # Copy a table from Excel, then:
#' tbl <- clip_read()
#' }
clip_read <- function(format = c("auto", "html", "rtf", "text", "csv"),
                      as = c("auto", "gt", "tibble", "character")) {
  format <- match.arg(format)
  as <- match.arg(as)

  if (format == "auto") {
    format <- pick_best_format(clip_formats_available())
  }

  raw <- switch(format,
                html = read_clipboard_raw("Html"),
                rtf  = read_clipboard_raw("Rtf"),
                csv  = read_clipboard_raw("CommaSeparatedValue"),
                text = read_clipboard_raw("Text")
  )

  switch(format,
         html = html_to_object(raw, as = as),
         csv  = csv_to_object(raw, as = as),
         rtf  = {
           warning("RTF structural parsing isn't supported; returning stripped plain text. ",
                   "If the source app also puts HTML on the clipboard (Word/Excel do), ",
                   "try format = \"html\" instead.", call. = FALSE)
           rtf_to_text(raw)
         },
         text = if (as == "character") raw else tibble::tibble(text = raw)
  )
}

#' Write an R object to the clipboard in a rich format
#'
#' @param x A `gt` table, data frame, or character string.
#' @param format `"auto"`, `"html"`, or `"text"`. `"auto"` writes HTML for
#'   gt tables and data frames, plain text otherwise.
#' @return `x`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' clip_write(mtcars)          # paste into Word/Excel with formatting
#' clip_write(gt::gt(mtcars))  # same, from an already-styled gt table
#' }
clip_write <- function(x, format = c("auto", "html", "text")) {
  format <- match.arg(format)

  if (format == "auto") {
    format <- if (inherits(x, "gt_tbl") || is.data.frame(x)) "html" else "text"
  }

  if (format == "text") {
    write_clipboard_text(as.character(x))
  } else {
    write_clipboard_html(object_to_html(x))
  }
  invisible(x)
}

