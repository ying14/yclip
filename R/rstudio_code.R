


# R Code Source, Syntax Highlighting, Pandoc Styles -----------------------------------------------------


#' Get CSS classes from an HTML tag
#'
#' Extracts the values of the `class` attribute from an HTML tag and
#' returns them as a character vector.
#'
#' @param tag A single HTML tag.
#'
#' @return A character vector of CSS class names. Returns an empty
#'   character vector if the tag does not have a `class` attribute.
#'
#' @noRd
rstudio_html_classes <- function(tag) {
  classes <- stringr::str_match(
    tag,
    'class="([^"]*)"'
  )[, 2]
  if (is.na(classes)) {
    return(character())
  }
  stringr::str_split(
    classes,
    "\\s+"
  )[[1]]
}


#' Read an RStudio theme file
#'
#' Reads an RStudio `.rstheme` file and extracts its metadata and CSS
#' rules. The resulting object can be supplied to
#' [rstudio_html_restyle()] to translate RStudio-generated HTML to the
#' colors of the selected theme.
#'
#' @param path Path to an RStudio `.rstheme` file.
#'
#' @return A list with components:
#' \describe{
#'   \item{path}{The normalized path to the theme file.}
#'   \item{name}{The theme name, if specified in the theme file.}
#'   \item{dark}{A logical value indicating whether the theme is dark,
#'     if specified in the theme file.}
#'   \item{rules}{A tibble containing the CSS selectors, properties,
#'     and values extracted from the theme.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' theme <- read_rstudio_theme(
#'   "C:/Program Files/RStudio/resources/app/resources/themes/dracula.rstheme"
#' )
#' theme$name
#' theme$dark
#' }
read_rstudio_theme <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    cli::cli_abort("`path` must be a single non-missing character string.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("Theme file {.file {path}} does not exist.")
  }
  x <- paste(readLines(path,warn = FALSE,encoding = "UTF-8"),collapse = "\n")
  theme_name <- stringr::str_match(x,"(?i)/\\*\\s*rs-theme-name:\\s*([^*]+?)\\s*\\*/")[, 2]
  is_dark <- stringr::str_match(x,"(?i)/\\*\\s*rs-theme-is-dark:\\s*(TRUE|FALSE)\\s*\\*/")[, 2]
  if (is.na(theme_name)) {
    theme_name <- NA_character_
  }
  if (is.na(is_dark)) {
    is_dark <- NA
  } else {
    is_dark <- toupper(is_dark) == "TRUE"
  }
  x <- stringr::str_remove_all(x,"(?s)/\\*.*?\\*/")
  blocks <- stringr::str_match_all(x,"(?s)([^{}]+)\\{([^{}]*)\\}")[[1]]
  if (nrow(blocks) == 0L) {
    cli::cli_abort("No CSS rules were found in {.file {path}}.")
  }
  rules <- purrr::map_dfr(
    seq_len(nrow(blocks)),
    function(i) {
      selectors <- stringr::str_split(blocks[i, 2],",",simplify = FALSE)[[1]]
      declarations <- stringr::str_split(blocks[i, 3],";",simplify = FALSE)[[1]]
      declarations <- purrr::map_dfr(
        declarations,
        function(declaration) {
          match <- stringr::str_match(declaration,"^\\s*([A-Za-z-]+)\\s*:\\s*(.*?)\\s*$")
          if (is.na(match[1, 2])) {
            return(tibble::tibble(
              property = character(),
              value = character()
            ))
          }
          tibble::tibble(
            property = match[1, 2],
            value = match[1, 3]
          )
        }
      )
      purrr::map_dfr(
        stringr::str_trim(selectors),
        function(selector) {
          if (!nzchar(selector)) {
            return(tibble::tibble(
              selector = character(),
              property = character(),
              value = character()
            ))
          }
          dplyr::mutate(
            declarations,
            selector = selector,
            .before = 1
          )
        }
      )
    }
  )
  rules <- dplyr::select(rules,selector,property,value)
  class_rules <- dplyr::filter(
    rules,
    stringr::str_detect(selector,"^\\s*(?:\\.[A-Za-z0-9_-]+)+\\s*$")
  )
  class_rules <- dplyr::mutate(
    class_rules,
    classes = purrr::map(
      selector,
      function(selector) {
        stringr::str_remove(
          stringr::str_extract_all(selector,"\\.[A-Za-z0-9_-]+")[[1]],"^\\."
        )
      }
    ),
    specificity = purrr::map_int(classes,length)
  )

  list(path = normalizePath(path,winslash = "/",mustWork = TRUE),
       name = theme_name,
       dark = is_dark,
       rules = rules,
       class_rules = class_rules)
}

#' Get a CSS property from an RStudio theme
#'
#' Looks up an exact CSS selector and property in an RStudio theme
#' returned by [read_rstudio_theme()].
#'
#' @param theme An RStudio theme object returned by
#'   [read_rstudio_theme()].
#' @param selector A single CSS selector, such as `".xtermColor2"` or
#'   `".terminal"`.
#' @param property A single CSS property, such as `"color"` or
#'   `"background-color"`.
#'
#' @return The CSS value as a character string, or `NULL` if the
#'   selector/property combination is not present in the theme.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' theme <- read_rstudio_theme("dracula.rstheme")
#' rstudio_theme_rule(theme, ".xtermColor2", "color")
#' rstudio_theme_rule(theme, ".terminal", "background-color")
#' }
rstudio_theme_rule <- function(theme, selector, property) {
  if (!is.list(theme) || !all(c("name", "dark", "rules") %in% names(theme))) {
    cli::cli_abort("`theme` must be an object returned by `read_rstudio_theme()`.")
  }
  if (!is.character(selector) || length(selector) != 1L || is.na(selector)) {
    cli::cli_abort("`selector` must be a single non-missing character string.")
  }
  if (!is.character(property) || length(property) != 1L || is.na(property)) {
    cli::cli_abort("`property` must be a single non-missing character string.")
  }
  x <- dplyr::filter(
    theme$rules,
    selector == .env$selector,
    property == .env$property
  )
  if (nrow(x) == 0L) {
    return(NULL)
  }
  x$value[[1]]
}


#' Restyle an HTML tag according to an RStudio theme
#' @param tag A single HTML tag.
#' @param theme An object returned by [read_rstudio_theme()].
#' @return The restyled HTML tag.
#' @noRd
rstudio_html_restyle_tag <- function(tag, theme) {
  classes <- rstudio_html_classes(tag)
  if (length(classes) == 0L) {
    return(tag)
  }
  color <- rstudio_theme_style(
    theme,
    classes,
    property = "color"
  )
  if (is.null(color)) {
    return(tag)
  }
  if (!stringr::str_detect(tag, 'style="[^"]*color\\s*:')) {
    return(tag)
  }
  stringr::str_replace(
    tag,
    "color\\s*:\\s*[^;\"']+",
    paste0("color: ", color)
  )
}




#' Restyle RStudio-generated HTML using another theme
#'
#' Translates RStudio console HTML to the colors defined by a
#' destination RStudio theme. The HTML is expected to have been
#' generated by RStudio and to contain RStudio terminal and
#' syntax-highlighting classes.
#'
#' The console foreground and background colors are obtained from the
#' destination theme's `.terminal` rule. Syntax-highlighted text is
#' restyled using the CSS class rules in the destination theme,
#' including `xtermColor` and Ace editor classes.
#'
#' The source theme does not need to be specified because the HTML
#' contains the classes needed to identify the corresponding
#' destination colors.
#'
#' @param html A single character string containing RStudio-generated
#'   HTML.
#' @param theme An RStudio theme object returned by
#'   [read_rstudio_theme()].
#'
#' @return A character string containing the restyled HTML.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' html <- yclip::clipboard_read_text("HTML Format")
#' dracula <- read_rstudio_theme("dracula.rstheme")
#' html_dracula <- rstudio_html_restyle(html, dracula)
#' }
rstudio_html_restyle <- function(html, theme) {
  if (!is.character(html) || length(html) != 1L || is.na(html)) {
    cli::cli_abort(
      "`html` must be a single non-missing character string."
    )
  }
  if (!is.list(theme) || !all(c("name", "dark", "rules") %in% names(theme))) {
    cli::cli_abort(
      "`theme` must be an object returned by `read_rstudio_theme()`."
    )
  }
  stringr::str_replace_all(
    html,
    "<[^>]+>",
    function(tag) {
      if (stringr::str_detect(tag, "^<pre\\b")) {
        tag <- rstudio_html_restyle_pre(
          tag,
          theme
        )
      }
      rstudio_html_restyle_tag(
        tag,
        theme
      )
    }
  )
}




#' Find a theme style applicable to a set of HTML classes
#'
#' Finds the most specific class-based CSS rule applicable to a set of
#' HTML classes.
#'
#' @param theme An object returned by [read_rstudio_theme()].
#' @param classes Character vector of HTML class names.
#' @param property CSS property to retrieve.
#'
#' @return The value of the most specific matching CSS rule, or `NULL`
#'   if no matching rule is found.
#'
#' @noRd
rstudio_theme_style <- function(theme, classes, property = "color") {
  if (!is.list(theme) || !all(c("name", "dark", "rules", "class_rules") %in% names(theme))) {
    cli::cli_abort("`theme` must be an object returned by `read_rstudio_theme()`.")
  }
  if (!is.character(classes) || length(classes) == 0L || anyNA(classes)) {
    cli::cli_abort("`classes` must be a non-empty character vector.")
  }
  if (!is.character(property) || length(property) != 1L || is.na(property)) {
    cli::cli_abort("`property` must be a single non-missing character string.")
  }
  classes <- stringr::str_remove(classes, "^\\.")
  rules <- dplyr::filter(
    theme$class_rules,
    property == .env$property
  )
  if (nrow(rules) == 0L) {
    return(NULL)
  }
  matches <- purrr::map_lgl(
    rules$classes,
    function(selector_classes) {
      all(selector_classes %in% classes)
    }
  )
  rules <- rules[matches, , drop = FALSE]
  if (nrow(rules) == 0L) {
    return(NULL)
  }
  rules <- rules[order(rules$specificity, seq_len(nrow(rules))), , drop = FALSE]
  rules$value[[nrow(rules)]]
}


#' Restyle an RStudio console `<pre>` tag
#'
#' Replaces the foreground and background colors of an RStudio console
#' `<pre>` tag using the `.terminal` rule from a destination RStudio
#' theme.
#'
#' @param tag A single HTML `<pre>` tag.
#' @param theme An RStudio theme object returned by
#'   [read_rstudio_theme()].
#'
#' @return The restyled HTML tag.
#'
#' @noRd
rstudio_html_restyle_pre <- function(tag, theme) {
  foreground <- rstudio_theme_rule(
    theme,
    ".terminal",
    "color"
  )
  background <- rstudio_theme_rule(
    theme,
    ".terminal",
    "background-color"
  )
  if (!is.null(foreground)) {
    tag <- stringr::str_replace(
      tag,
      "color\\s*:\\s*[^;\"']+",
      paste0("color: ", foreground)
    )
  }
  if (!is.null(background)) {
    tag <- stringr::str_replace(
      tag,
      "background-color\\s*:\\s*[^;\"']+",
      paste0("background-color: ", background)
    )
  }
  tag
}



# R Terminal Console Output -------------------------------------------------------



#' Apply syntax highlighting to source code using Pandoc
#'
#' Converts source code to syntax-highlighted HTML using Pandoc's
#' syntax-highlighting engine. The resulting HTML contains the CSS
#' generated by Pandoc and the highlighted code block, making it suitable
#' for writing to the Windows clipboard as an HTML clipboard format.
#'
#' @param text_code A single character string containing the source code
#'   to be highlighted.
#' @param language A single character string specifying the programming
#'   language of `text_code`, using a language identifier recognized by
#'   Pandoc.
#' @param style A single character string specifying the Pandoc syntax
#'   highlighting style. Defaults to `"pygments"`. Other styles supported
#'   by the installed version of Pandoc can be supplied.
#'   Run `system2(find_pandoc(), "--list-highlight-styles", stdout = TRUE)`
#'   to list available styles.
#' @param background A single character string specifying the CSS
#'   background color for the highlighted code block. Defaults to
#'   `"#eeeeee"`. If `NULL`, no background color is added.
#' @param additional_args A character vector of additional command-line
#'   arguments passed directly to Pandoc. This provides an escape hatch
#'   for Pandoc options not directly exposed by this function. For example,
#'   `c("-V monofont=Consolas")` can be used to specify the monospace
#'   font.
#'
#' @return A character string containing HTML with the Pandoc-generated
#'   CSS and syntax-highlighted code block.
#'
#' @examples
#' code <- paste(
#'   "x <- 1:10",
#'   "mean(x)",
#'   sep = "\n"
#' )
#'
#' html <- syntax_highlighting(
#'   code,
#'   language = "r"
#' )
#'
#' html2 <- syntax_highlighting(
#'   code,
#'   language = "r",
#'   style = "breezeDark",
#'   additional_args = "-V fontsize=21pt"
#' )
#' @export
syntax_highlighting <- function(
    text_code,
    language,
    style = "pygments",
    background = "#eeeeee",
    additional_args = character()
) {
  if (!is.character(text_code) || length(text_code) != 1L || is.na(text_code)) {
    cli::cli_abort("`text_code` must be a single non-missing character string.")
  }
  if (!is.character(language) || length(language) != 1L || is.na(language)) {
    cli::cli_abort("`language` must be a single non-missing character string.")
  }
  if (!nzchar(language)) {
    cli::cli_abort("`language` must not be empty.")
  }
  if (!is.character(style) || length(style) != 1L || is.na(style)) {
    cli::cli_abort("`style` must be a single non-missing character string.")
  }
  if (!is.null(background)) {
    if (!is.character(background) || length(background) != 1L || is.na(background)) {
      cli::cli_abort("`background` must be `NULL` or a single non-missing character string.")
    }
    if (!nzchar(background)) {
      cli::cli_abort("`background` must not be empty.")
    }
  }
  if (!is.character(additional_args) || anyNA(additional_args)) {
    cli::cli_abort("`additional_args` must be a character vector with no missing values.")
  }
  pandoc <- find_pandoc()
  markdown <- paste0(
    "```{.", language, "}\n",
    text_code,
    "\n```\n"
  )
  args <- c(
    "-f markdown",
    "-t html",
    "--standalone",
    "--metadata", "title=Code",
    paste0("--highlight-style=", style),
    additional_args
  )
  result <- system2(
    pandoc,
    args,
    input = markdown,
    stdout = TRUE
  )
  html <- paste(result, collapse = "\n")
  if (!is.null(background)) {
    # CHANGE: pre.sourceCode { margin: 0; }
    # TO:     pre.sourceCode { margin: 0;  background-color: #eeeeee;}
    html <- stringr::str_replace(
      html,
      "(pre\\.sourceCode\\s*\\{[^}]*)(\\})",
      paste0("\\1 background-color: ",background,";\\2")
    )
  }
  style_block <- stringr::str_extract(html,"(?s)<style[^>]*>.*?</style>")
  code_block <- stringr::str_extract(html,"(?s)<div class=\"sourceCode\".*?</div>")
  if (is.na(style_block) || is.na(code_block)) {
    cli::cli_abort("Pandoc did not produce the expected syntax-highlighted HTML.")
  }
  paste0(style_block, "\n", code_block)
}




#' Convert R Code on Clipboard to Rich Text With Syntax Highlighting
#' @export
read.clipboard.rcode <- function(code = NULL) {
  if (is.null(code)) {
    cli::cli_alert_info("Reading in R code from clipboard...")
    code <- clipboard_read_text("CF_UNICODETEXT")
  }
  cli::cli_alert_info("Generating Pandoc-formatted HTML with syntax highlighting...")
  html <- syntax_highlighting(code, language = "r")
  clipboard_write_formats(
    "CF_TEXT" = code,
    "CF_UNICODETEXT" = code,
    "HTML Format" = html
  )
  cli::cli_alert_info("Wrote formatted code to clipboard.")
  invisible(html)
}

#' Convert R Terminal Output to Rich Text
#' @export
read.clipboard.rterminal <- function(background = NULL) {

  cli::cli_alert_info("Reading in R terminal output text...")
  console_html <- clipboard_read_text("HTML Format")
  textmate <- read_rstudio_theme("C:/Program Files/RStudio/resources/app/resources/themes/textmate.rstheme")

  cli::cli_alert_info("Re-styling text to Textmate...")
  console_html_converted <- rstudio_html_restyle(console_html,textmate)
  if (!is.null(background)) {
    cli::cli_alert_info("Adding background color...")

    if (!is.character(background) || length(background) != 1L || is.na(background)) {
      cli::cli_abort(
        "`background` must be `NULL` or a single non-missing character string."
      )
    }
    if (!nzchar(background)) {
      cli::cli_abort("`background` must not be empty.")
    }
    console_html_converted <- stringr::str_replace(
      console_html_converted,
      "(<pre[^>]*id=\"rstudio_console_output\"[^>]*style=\"[^\"]*background-color:\\s*)[^;]+",
      paste0("\\1", background)
    )
  }
  cli::cli_alert_info("Writing R terminal output text to clipboard.")
  clipboard_write_formats("HTML Format" = console_html_converted)
  invisible(console_html_converted)
}



# Knitr rendering ---------------------------------------------------------




#' Convert R Code to Formatted HTML
#'
#' @param code
#' @param options
#'
#' @return
#' @export
#'
#' @examples
knit_code <- function(code, options = list()) {
  workdir <- tempfile()
  dir.create(workdir)
  on.exit(unlink(workdir, recursive = TRUE), add = TRUE)
  old_opts <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$set(old_opts), add = TRUE)
  figdir <- file.path(workdir, "figure")
  dir.create(figdir)
  knitr::opts_chunk$set(
    comment = "#>",
    echo = TRUE,
    fig.path = paste0(figdir, "/")
  )
  if (length(options) > 0L) {
    knitr::opts_chunk$set(options)
  }
  text <- c("```{r}", code, "```")
  md <- file.path(workdir, "output.md")
  knitr::knit(
    text = text,
    output = md,
    envir = globalenv()
  )
  md_text <- readLines(md)
  md_text <- stringr::str_replace_all(
    md_text,
    stringr::fixed(workdir),
    "."
  )
  writeLines(md_text, md)
  pandoc <- yclip::find_pandoc()
  html <- file.path(workdir, "output.html")
  system2(
    pandoc,
    c(
      "-f", "markdown",
      "-t", "html",
      "--standalone",
      "--embed-resources",
      "--resource-path", dirname(md),
      "-o", html,
      md
    )
  )
  readr::read_file(html)
}



#' @export
read.rcode.make.knitr.html <- function() {
  code <- clipboard_read_text("CF_UNICODETEXT")
  html <- knit_code(code)
  clipboard_write_formats("HTML Format"=html)
  invisible(html)
}



# Miscellaneous -----------------------------------------------------------



#' Find Pandoc Executable
#' @export
find_pandoc <- function() {
  pandoc <- Sys.which("pandoc")
  if (nzchar(pandoc)) {
    return(pandoc)
  }
  rstudio_pandoc <- Sys.getenv("RSTUDIO_PANDOC")
  if (nzchar(rstudio_pandoc)) {
    pandoc <- file.path(
      rstudio_pandoc,
      "pandoc.exe"
    )
    if (file.exists(pandoc)) {
      return(pandoc)
    }
  }
  quarto <- Sys.which("quarto")
  if (nzchar(quarto)) {
    quarto_dir <- dirname(quarto)
    pandoc <- list.files(
      quarto_dir,
      recursive = TRUE,
      pattern = "^pandoc\\.exe$",
      full.names = TRUE
    )
    if (length(pandoc) > 0L) {
      return(pandoc[[1]])
    }
  }
  cli::cli_abort(
    "Could not find {.file pandoc.exe}. Install Pandoc or Quarto."
  )
}






