




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



#' Write Raw Text to Clipboard
#'
#' @param data Data to write.
#' @param format_name Clipboard format.
#' @export
clipboard_write_raw <- function(data, format_name) {
  if (.Platform$OS.type != "windows") {
    stop("clipboard_write_raw() is currently only implemented on Windows.")
  }
  clipboard_write_raw_windows(data, format_name)
}



#' Write Multiple Formats to the Windows Clipboard
#'
#' @param ... Named raw vectors, with the names being clipboard
#'   format names.
#'
#' @return Invisibly returns NULL.
#' @export
clipboard_write_formats <- function(...) {
  if (.Platform$OS.type != "windows") {
    stop("clipboard_write_formats() is currently only implemented on Windows.")
  }
  data <- list(...)
  if (length(data) == 0) {
    stop("At least one clipboard format must be supplied.")
  }
  if (is.null(names(data)) || any(names(data) == "")) {
    stop("Clipboard formats must be supplied as named arguments.")
  }
  if (!all(vapply(data, is.raw, logical(1)))) {
    stop("Each clipboard format must be supplied as a raw vector.")
  }
  clipboard_write_formats_windows(data)
  invisible(NULL)
}



#' Converts DIB raw to BMP magick object
#'
#' @param dib dib raw object
#'
#' @return
#' @export
#'
#' @examples
dib_to_bmp <- function(dib) {
  if (!is.raw(dib)) {
    stop("'dib' must be a raw vector.")
  }
  if (length(dib) < 40L) {
    stop("DIB is too short to contain a BITMAPINFOHEADER.")
  }
  # Helper: read unsigned 16-bit little-endian integer
  read_uint16_le <- function(x, offset) {
    as.integer(x[offset]) + 256L * as.integer(x[offset + 1L])
  }
  # Helper: read unsigned 32-bit little-endian integer
  read_uint32_le <- function(x, offset) {
    b <- as.integer(x[offset:(offset + 3L)])
    sum(b * 256^(0:3))
  }
  # BITMAPINFOHEADER fields
  header_size <- read_uint32_le(dib, 1L)
  if (!(header_size %in% c(40L, 52L, 56L, 108L, 124L))) {
    stop("Unsupported DIB header size: ",header_size," bytes.")
  }
  width <- read_uint32_le(dib, 5L)
  height_raw <- read_uint32_le(dib, 9L)
  planes <- read_uint16_le(dib, 13L)
  bit_count <- read_uint16_le(dib, 15L)
  compression <- read_uint32_le(dib, 17L)
  size_image <- read_uint32_le(dib, 21L)
  clr_used <- read_uint32_le(dib, 33L)
  if (planes != 1L) {
    stop("Unsupported DIB: biPlanes must be 1.")
  }
  # Compression constants
  BI_RGB <- 0L
  BI_RLE8 <- 1L
  BI_RLE4 <- 2L
  BI_BITFIELDS <- 3L
  BI_JPEG <- 4L
  BI_PNG <- 5L
  if (compression %in% c(BI_RLE8, BI_RLE4, BI_JPEG, BI_PNG)) {
    stop(
      "DIB uses unsupported compression type: ",
      compression,
      "."
    )
  }
  # ------------------------------------------------------------
  # Determine where the pixel data begins in the DIB.
  #
  # DIB layout:
  #
  #   BITMAPINFOHEADER
  #   color masks (for BI_BITFIELDS, if applicable)
  #   color table (if applicable)
  #   pixel data
  # ------------------------------------------------------------
  pixel_offset_dib <- header_size
  # For BITMAPV2/V3/V4/V5-style headers, masks may already
  # be incorporated into the header. For a 40-byte
  # BITMAPINFOHEADER with BI_BITFIELDS, the masks follow it.
  if (compression == BI_BITFIELDS && header_size == 40L) {
    pixel_offset_dib <- pixel_offset_dib + 12L
  }
  # Determine whether a color table is present.
  #
  # For <= 8 bits/pixel, a palette normally follows the header
  # (and any masks).
  if (bit_count <= 8L) {
    if (clr_used != 0L) {
      n_colors <- clr_used
    } else {
      n_colors <- 2^bit_count
    }
    pixel_offset_dib <- pixel_offset_dib + 4L * n_colors
  }
  # Validate that the calculated pixel offset is plausible.
  if (pixel_offset_dib >= length(dib)) {
    stop(
      "Calculated pixel-data offset is outside the DIB."
    )
  }
  # ------------------------------------------------------------
  # BMP BITMAPFILEHEADER
  #
  # Offset  Size  Field
  # 0       2     bfType       ("BM")
  # 2       4     bfSize
  # 6       2     bfReserved1
  # 8       2     bfReserved2
  # 10      4     bfOffBits
  # ------------------------------------------------------------
  file_size <- 14L + length(dib)
  # Pixel offset in the BMP file is:
  #   14-byte BMP file header
  #   + pixel offset within the DIB
  off_bits <- 14L + pixel_offset_dib
  # Helper: encode unsigned integer as little-endian raw bytes
  uint16_le <- function(x) {
    x <- as.integer(x)
    as.raw(c(
      bitwAnd(x, 0xFF),
      bitwAnd(bitwShiftR(x, 8), 0xFF)
    ))
  }
  uint32_le <- function(x) {
    # Avoid bitwShiftR problems with values above 2^31
    x <- as.double(x)
    b1 <- x %% 256
    x <- floor(x / 256)
    b2 <- x %% 256
    x <- floor(x / 256)
    b3 <- x %% 256
    x <- floor(x / 256)
    b4 <- x %% 256
    as.raw(c(b1, b2, b3, b4))
  }
  bmp_header <- c(
    # bfType = "BM"
    charToRaw("BM"),
    # bfSize
    uint32_le(file_size),
    # bfReserved1
    uint16_le(0),
    # bfReserved2
    uint16_le(0),
    # bfOffBits
    uint32_le(off_bits)
  )
  stopifnot(length(bmp_header) == 14L)
  # Construct the BMP
  bmp <- c(bmp_header, dib)
  bmp
}

parse_rtf_rows <- function(x) {
  stopifnot(length(x) == 1, is.character(x))
  matches <- stringr::str_locate_all(x,"\\\\trowd\\b|\\\\cell(?![a-zA-Z])|\\\\row\\b")[[1]]
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
        txt <- stringr::str_sub(x,cell_start,pos - 1L)
        current_row <- c(current_row,rtf_strip_simple(txt))
      }
      cell_start <- end + 1L
    } else if (token == "\\row") {
      # Add any text between the final \cell and \row,
      # but only if it contains actual content.
      if (!is.null(cell_start)) {
        txt <- stringr::str_sub(x,cell_start,pos - 1L)
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

