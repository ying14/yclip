

# Overall -----------------------------------------------------------------





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
#' @param format_name the desired clipboard format. E.g., `"Rich Text Format"` or the ID number
#'
#' @return
#' @export
#'
#' @examples
clipboard_read_raw <- function(format) {
  if (.Platform$OS.type != "windows") {
    cli::cli_abort("clipboard_read_raw() is currently only implemented on Windows.")
  }
  if (is.character(format)) {
    if (length(format) != 1L || is.na(format)) {
      cli::cli_abort("`format` must be a single non-missing character string or integer clipboard format ID.")
    }
  } else if (is.numeric(format)) {
    if (length(format) != 1L || is.na(format) || format != as.integer(format) || format < 1) {
      cli::cli_abort("`format` must be a single positive integer clipboard format ID.")
    }
    format <- as.integer(format)
  } else {
    cli::cli_abort("`format` must be a character string or integer clipboard format ID.")
  }
  clipboard_read_raw_windows(format)
}




#' Read Data from Clipboard
#'
#' @param format_name the desired clipboard format. E.g., `"Rich Text Format"`
#'
#' @return
#' @export
#'
#' @examples
clipboard_read_text <- function(format_name) {
  if (.Platform$OS.type != "windows") {
    cli::cli_abort("clipboard_read_text() is currently only implemented on Windows.")
  }
  if (format_name == "CF_UNICODETEXT") {
    return(clipboard_read_unicode_text_windows())
  }
  if (format_name == "HTML Format") {
    return(clipboard_decode_html(clipboard_read_raw(format_name)))
  }
  # other formats including RTF
  x <- clipboard_read_raw(format_name)
  rawToChar(x)
}



#' Write One or More Formats to the Windows Clipboard
#'
#' @param ... Named raw vectors or character strings, with the names being
#'   clipboard format names. Character strings are encoded according to the
#'   clipboard format. Raw vectors are written unchanged.
#'
#' @return Invisibly returns NULL.
#' @export
clipboard_write_formats <- function(...) {
  if (.Platform$OS.type != "windows") {
    cli::cli_abort("clipboard_write_formats() is currently only implemented on Windows.")
  }
  data <- list(...)
  if (length(data) == 0) {
    cli::cli_abort("At least one clipboard format must be supplied.")
  }
  if (is.null(names(data)) || any(names(data) == "")) {
    cli::cli_abort("Clipboard formats must be supplied as named arguments.")
  }
  data <- purrr::map2(data,names(data),clipboard_encode_data)
  clipboard_write_formats_windows(data)
  invisible(NULL)
}


#' Convert data to raw
#'
#' Internal function that converts data to raw.
#' Similar to `charToRaw()`
#'
#' We need to do modifications depending on the format.
#' For `CF_TEXT` and `CF_UNICODETEXT`, we need to add bytes at the end,
#' And for `CF_UNICODETEXT` we need UTF-16LE.
#' @param data
#' @param format_name Clipboard image format to be encoded.
#' @return raw data
clipboard_encode_text <- function(data, format_name) {
  if (is.raw(data)) {
    return(data)
  }
  if (!is.character(data)) {
    cli::cli_abort("Data for clipboard format {.val {format_name}} must be a raw vector or character vector.")
  }
  if (length(data) != 1L || is.na(data)) {
    cli::cli_abort("Character data for clipboard format {.val {format_name}} must be a single non-missing string.")
  }
  switch(
    format_name,
    "CF_TEXT" = {
      c(charToRaw(data),as.raw(0))
    },
    "CF_UNICODETEXT" = {
      bytes <- iconv(data,from = "UTF-8",to = "UTF-16LE",toRaw = TRUE)[[1]]
      if (is.null(bytes)) {
        cli::cli_abort("Could not convert character data to UTF-16LE.")
      }
      c(bytes,as.raw(c(0, 0)))
    },
    "HTML Format" = clipboard_encode_html(data),
    # probably RTF
    charToRaw(data)
  )
}


#' Prepare Data for Clipboard Copy
#'
#' Converts data and prepares it for the clipboard.
#' @param data data to be prepared
#' @param format_name Clipboard image format to be prepared.
#' @return Converted data, as raw vector.
#'
#' @examples
clipboard_encode_data <- function(data, format_name) {
  if (is.raw(data)) {
    return(data)
  }
  if (inherits(data, "flextable")) {
    return(clipboard_encode_flextable(data, format_name))
  }
  if (inherits(data, "magick-image")) {
    return(clipboard_encode_image(data, format_name))
  }
  if (is.character(data)) {
    return(clipboard_encode_text(data, format_name))
  }
  cli::cli_abort(
    "Data for clipboard format {.val {format_name}} must be a raw vector, character string, flextable, or {.cls magick-image}."
  )
}



#' Isolate Clipboard Elements
#'
#' @param ... Formats to be isolated. Can be ID (integer) or name (character).
#' These are listed with [clipboard_inspect()]
#' @export
clipboard_isolate <- function(...) {
  if (.Platform$OS.type != "windows") {
    cli::cli_abort("clipboard_isolate() is currently only implemented on Windows.")
  }
  formats <- list(...)
  if (length(formats) == 0L) {
    cli::cli_abort("At least one clipboard format must be specified.")
  }
  formats <- purrr::map(
    formats,
    function(format) {
      if (is.character(format)) {
        if (length(format) != 1L || is.na(format)) {
          cli::cli_abort(
            "Each clipboard format must be a single non-missing character string or positive integer ID."
          )
        }
        return(format)
      }
      if (is.numeric(format)) {
        if (length(format) != 1L || is.na(format) ||
            format != as.integer(format) || format < 1) {
          cli::cli_abort(
            "Each clipboard format must be a single non-missing character string or positive integer ID."
          )
        }
        return(as.integer(format))
      }
      cli::cli_abort("Each clipboard format must be a character string or positive integer ID.")
    }
  )
  clipboard_isolate_windows(formats)
  cli::cli_alert_info("Isolated format{?s} in clipboard: {.pkg {formats}}")
  invisible(NULL)
}
browseURL()

# HTML --------------------------------------------------------------------


#' Decode Windows HTML Clipboard data
#'
#' Windows stores HTML on the clipboard using the `HTML Format` clipboard
#' format. The stored data consists of an ASCII header containing byte
#' offsets such as `StartFragment` and `EndFragment`, followed by the HTML
#' data itself.
#'
#' This function extracts the HTML fragment identified by those offsets and
#' returns it as a character string. It therefore converts the raw Windows
#' clipboard representation into the HTML content that can be used directly
#' by R or an HTML renderer.
#'
#' In contrast, [clipboard_read_raw()] returns the complete `HTML Format`
#' representation, including the Windows clipboard header. This function is
#' used internally by [clipboard_read_text()] when `format_name` is
#' `"HTML Format"`.
#'
#' @param x A raw vector containing the complete Windows `HTML Format`
#'   clipboard representation.
#'
#' @return A character string containing the HTML fragment between the
#'   `StartFragment` and `EndFragment` byte offsets.
#'
#' @keywords internal
clipboard_decode_html <- function(x) {
  if (!is.raw(x)) {
    cli::cli_abort("HTML clipboard data must be a raw vector.")
  }
  header <- rawToChar(x[seq_len(min(length(x), 4096L))])
  start_fragment <- stringr::str_match(
    header,
    "(?m)^StartFragment:(\\d+)\\r?$"
  )[, 2]
  end_fragment <- stringr::str_match(
    header,
    "(?m)^EndFragment:(\\d+)\\r?$"
  )[, 2]
  if (is.na(start_fragment) || is.na(end_fragment)) {
    cli::cli_abort(
      "HTML clipboard data does not contain valid StartFragment and EndFragment offsets."
    )
  }
  start_fragment <- as.integer(start_fragment)
  end_fragment <- as.integer(end_fragment)
  if (start_fragment < 0 || end_fragment <= start_fragment) {
    cli::cli_abort(
      "HTML clipboard data contains invalid fragment offsets."
    )
  }
  if (end_fragment > length(x)) {
    cli::cli_abort(
      "HTML clipboard data contains fragment offsets beyond the end of the data."
    )
  }
  rawToChar(x[(start_fragment + 1L):end_fragment])
}



clipboard_encode_html <- function(html) {
  if (!is.character(html) || length(html) != 1L || is.na(html)) {
    cli::cli_abort("HTML must be a single non-NA character string.")
  }
  html <- enc2utf8(html)
  prefix <- paste0(
    "<html>\r\n",
    "<body>\r\n",
    "<!--StartFragment-->\r\n"
  )
  suffix <- paste0(
    "\r\n<!--EndFragment-->\r\n",
    "</body>\r\n",
    "</html>\r\n"
  )
  header_template <- paste0(
    "Version:0.9\r\n",
    "StartHTML:%010d\r\n",
    "EndHTML:%010d\r\n",
    "StartFragment:%010d\r\n",
    "EndFragment:%010d\r\n"
  )
  header <- sprintf(header_template, 0L, 0L, 0L, 0L)
  header_bytes <- charToRaw(header)
  prefix_bytes <- charToRaw(prefix)
  html_bytes <- charToRaw(html)
  suffix_bytes <- charToRaw(suffix)
  start_html <- length(header_bytes)
  start_fragment <- start_html + length(prefix_bytes)
  end_fragment <- start_fragment + length(html_bytes)
  end_html <- end_fragment + length(suffix_bytes)
  header <- sprintf(
    header_template,
    start_html,
    end_html,
    start_fragment,
    end_fragment
  )
  c(
    charToRaw(header),
    prefix_bytes,
    html_bytes,
    suffix_bytes
  )
}


# Flextable ---------------------------------------------------------------




#' Fix flextable RTF for clipboard pasting into Epic
#'
#' flextable::save_as_rtf() produces valid RTF, but its cell paragraphs do
#' not explicitly contain `\\pard\\intbl`. Epic appears to require these
#' paragraph/table markers when interpreting RTF directly from the clipboard.
#' WordPad adds them when the RTF is pasted and copied again, which explains
#' why the same RTF works after passing through WordPad.
#'
#' This inserts `\\pard\\intbl` immediately after the final `\\cellx` in
#' each row, identified by a `\\cellx` control word followed immediately by
#' `\\sl`. The pattern is intentionally specific to the structure produced
#' by flextable rather than modifying arbitrary RTF.
#'
#' @param x A single character string containing RTF generated by flextable.
#'
#' @return The modified RTF string.
#'
#' @noRd
clipboard_fix_flextable_rtf <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("Flextable RTF must be a single non-missing character string.")
  }
  stringr::str_replace_all(
    x,
    "(\\\\cellx[0-9]+)(?=\\\\sl)",
    "\\1\\\\pard\\\\intbl"
  )
}



#' Encode a flextable as RTF for the Windows clipboard
#'
#' flextable::save_as_rtf() produces RTF that works well in Word and WordPad,
#' but Epic does not correctly interpret the table structure unless
#' `\\pard\\intbl` is present at the beginning of each cell paragraph.
#' `clipboard_fix_flextable_rtf()` adds these control words before the RTF
#' is placed on the clipboard.
#'
#' @param data A flextable object.
#'
#' @return A raw vector containing the RTF clipboard data.
#'
#' @noRd
clipboard_encode_flextable_rtf <- function(data) {
  con <- textConnection("rtf", "w", local = TRUE)
  flextable::save_as_rtf(
    data,
    path = con,
    pr_section = officer::prop_section()
  )
  close(con)
  rtf <- paste0(rtf, collapse = "\n")
  rtf <- clipboard_fix_flextable_rtf(rtf)
  charToRaw(rtf)
}





clipboard_encode_flextable <- function(data, format_name) {
  if (format_name=="Rich Text Format") {
    con <- textConnection("rtf", "w", local = TRUE)
    flextable::save_as_rtf(
      data,
      path = con,
      pr_section = officer::prop_section()
    )
    close(con)
    rtf <- paste0(rtf, collapse = "\n")
    rtf <- clipboard_fix_flextable_rtf(rtf)
    return(charToRaw(rtf))
  }
  if (format_name=="HTML Format") {
    html <- officer::to_html(ft_tab)
    return(clipboard_encode_html(html))
  }
  cli::cli_abort("Flextable objects cannot currently be encoded as {.val {format_name}}.")
}



# Image ------------------------------------------------------------------


clipboard_encode_image <- function(data, format_name) {
  switch(
    format_name,
    "PNG" = magick::image_write(data,format = "png"),
    "JFIF" = magick::image_write(data,format = "jpeg"),
    "GIF" = magick::image_write(data,format = "gif"),
    "CF_DIB" = magick_to_cf_dib(data),
    "CF_DIBV5" = magick_to_cf_dibv5(data),
    cli::cli_abort("Clipboard format {.val {format_name}} is not a supported image format.")
  )
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


#' Convert a magick image to an Office-compatible CF_DIB
#'
#' Converts a single `magick-image` object to the raw representation of a
#' Windows `CF_DIB` clipboard format. The DIB uses a 32-bit
#' `BITMAPINFOHEADER` with `BI_BITFIELDS` RGB masks and bottom-up BGR0 pixel
#' data. This representation is compatible with Microsoft Office
#' applications such as Word and PowerPoint.
#'
#' This is an internal helper used by [clipboard_write_image()] and is not
#' intended to be called directly by users.
#'
#' @param img A single `magick-image` object.
#'
#' @return A raw vector containing the `CF_DIB` data, suitable for use with
#'   [clipboard_write_formats()].
#'
#' @export
#' @keywords internal
magick_to_cf_dib <- function(img) {
  if (!inherits(img, "magick-image")) {
    stop("`img` must be a magick-image.", call. = FALSE)
  }

  info <- magick::image_info(img)

  if (nrow(info) != 1L) {
    stop("`img` must contain exactly one image.", call. = FALSE)
  }

  width  <- info$width
  height <- info$height

  # Get uncompressed RGBA pixels.
  rgba <- as.integer(
    magick::image_write(img, format = "rgba")
  )

  expected <- width * height * 4L

  if (length(rgba) != expected) {
    stop(
      sprintf(
        "Unexpected RGBA data length: got %d, expected %d.",
        length(rgba), expected
      ),
      call. = FALSE
    )
  }

  # RGBA -> BGR0.
  pixels <- matrix(rgba, ncol = 4L, byrow = TRUE)
  pixels <- pixels[, c(3L, 2L, 1L, 4L), drop = FALSE]

  # CF_DIB structure we are reproducing uses BGR0.
  pixels[, 4L] <- 0L

  # Reshape into:
  #   width x height x 4
  #
  # and reverse the rows because a positive BITMAPINFOHEADER
  # height specifies bottom-up pixel storage.
  pixels <- array(pixels, dim = c(width, height, 4L))
  pixels <- pixels[, height:1L, , drop = FALSE]

  pixel_data <- as.raw(aperm(pixels, c(3L, 1L, 2L)))

  # ---------------------------------------------------------------
  # BITMAPINFOHEADER -- 40 bytes
  # ---------------------------------------------------------------

  put_u16 <- function(value) {
    c(
      bitwAnd(value, 0xFF),
      bitwAnd(bitwShiftR(value, 8L), 0xFF)
    )
  }

  put_u32 <- function(value) {
    value <- as.numeric(value)

    c(
      bitwAnd(value, 0xFF),
      bitwAnd(floor(value / 256), 0xFF),
      bitwAnd(floor(value / 256^2), 0xFF),
      bitwAnd(floor(value / 256^3), 0xFF)
    )
  }

  header <- c(
    put_u32(40),             # biSize
    put_u32(width),          # biWidth
    put_u32(height),         # biHeight
    put_u16(1),              # biPlanes
    put_u16(32),             # biBitCount
    put_u32(3),              # biCompression = BI_BITFIELDS
    put_u32(length(pixel_data)), # biSizeImage
    put_u32(0),              # biXPelsPerMeter
    put_u32(0),              # biYPelsPerMeter
    put_u32(0),              # biClrUsed
    put_u32(0)               # biClrImportant
  )

  stopifnot(length(header) == 40L)

  # ---------------------------------------------------------------
  # RGB masks -- 12 bytes
  # ---------------------------------------------------------------

  masks <- as.raw(c(
    # Red   = 0x00FF0000
    0x00, 0x00, 0xFF, 0x00,

    # Green = 0x0000FF00
    0x00, 0xFF, 0x00, 0x00,

    # Blue  = 0x000000FF
    0xFF, 0x00, 0x00, 0x00
  ))

  c(as.raw(header), masks, pixel_data)
}



#' Convert a magick image to a CF_DIBV5 clipboard representation
#'
#' Converts a single `magick-image` object to the raw data used by the
#' Windows `CF_DIBV5` clipboard format. The image is written by ImageMagick
#' as a BMP file and the 14-byte BMP file header is removed, leaving the
#' `BITMAPV5HEADER` and associated pixel data required by `CF_DIBV5`.
#'
#' The resulting raw vector can be supplied to [clipboard_write_formats()].
#'
#' This is an internal helper used by [clipboard_write_image()] and is not
#' intended to be called directly by users.
#'
#' @param img A single `magick-image` object.
#'
#' @return A raw vector containing the `CF_DIBV5` data.
#'
#' @export
#' @keywords internal
magick_to_cf_dibv5 <- function(img) {
  if (!inherits(img, "magick-image")) {
    stop("`img` must be a magick-image.", call. = FALSE)
  }

  info <- magick::image_info(img)

  if (nrow(info) != 1L) {
    stop("`img` must contain exactly one image.", call. = FALSE)
  }

  bmp <- magick::image_write(img, format = "bmp")

  # Remove the 14-byte BITMAPFILEHEADER.
  bmp[-seq_len(14L)]
}




#' Read an Image from the Windows Clipboard
#'
#' @param format_name Clipboard image format to read. If `NULL`, the first
#'   available format from the preferred image-format order is used.
#'
#' @return A `magick-image` object.
#' @export
clipboard_read_image <- function(format_name = NULL) {
  if (.Platform$OS.type != "windows") {
    cli::cli_abort(
      "clipboard_read_image() is currently only implemented on Windows."
    )
  }
  if (is.null(format_name)) {
    format_name <- clipboard_choose_image_format()
  }
  clipboard_decode_image(format_name)
}


clipboard_choose_image_format <- function() {
  formats <- clipboard_inspect()
  preferred_formats <- c(
    "PNG",
    "JFIF",
    "GIF",
    "CF_DIBV5",
    "CF_DIB"
  )
  available_formats <- formats$name
  format_name <- preferred_formats[
    preferred_formats %in% available_formats
  ]
  if (length(format_name) == 0) {
    cli::cli_abort("No supported image format is available on the clipboard.")
  }
  format_name[[1]]
}



clipboard_decode_image <- function(format_name) {
  supported_formats <- c("PNG", "JFIF", "GIF", "CF_DIB", "CF_DIBV5")
  if (!format_name %in% supported_formats) {
    cli::cli_abort("Clipboard format {.val {format_name}} is not a supported image format.")
  }
  data <- clipboard_read_raw(format_name)
  if (format_name %in% c("CF_DIB", "CF_DIBV5")) {
    data <- dib_to_bmp(data)
  }
  magick::image_read(data)
}





# Miscellaneous -----------------------------------------------------------




#' Convert RTF text to HTML
#'
#' @param rtf character RTF data
#' @return character HTML data
#'
#' @export
word_convert_rtf_to_html <- function(rtf) {
  if (!requireNamespace("RDCOMClient", quietly = TRUE)) {
    stop("Package 'RDCOMClient' is required for this function. ",
         "Install it from https://github.com/omegahat/RDCOMClient.", call. = FALSE)
  }
  # RDCOMClient's C callbacks do a search-path lookup rather than a
  # namespace-aware one, so requireNamespace() alone isn't enough --
  # it has to actually be attached, not just loaded.
  if (!"package:RDCOMClient" %in% search()) {
    attachNamespace("RDCOMClient")
  }

  temp.rtf.infile <- tempfile("yclip_",fileext=".rtf")
  temp.html.outfile <- tempfile("yclip_",fileext=".html")
  write_lines(rtf,file=temp.rtf.infile)
  wd <- RDCOMClient::COMCreate("Word.Application")
  wd[["Visible"]] <- FALSE
  doc <- wd$Documents()$Open(normalizePath(temp.rtf.infile))
  doc$SaveAs2(normalizePath(temp.html.outfile, mustWork = FALSE), FileFormat = 8)
  doc$Close()
  wd$Quit()
  html <- read_lines(temp.html.outfile)

  tempfiles <- c(temp.rtf.infile,temp.html.outfile)
  # temp pic files - will be erased
  temp.html.additional.dir <- str_replace(temp.html.outfile,"\\.html","_files")
  if (dir.exists(temp.html.additional.dir)) {
    extra.files <- list.files(temp.html.additional.dir,full.names=TRUE)
    tempfiles <- c(tempfiles,extra.files)
  }
  html <- read_lines(temp.html.outfile)
  unlink(tempfiles)
  return(html)
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

