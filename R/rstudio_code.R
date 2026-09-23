


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






#' Render code to a self-contained HTML fragment, optionally executing it
#'
#' Converts a block of source code to an HTML fragment suitable for writing to
#' the clipboard as `"HTML Format"`. When `execute = TRUE` and `language` is one
#' knitr can run (currently R), the code is executed and its output and figures
#' are captured (as in a Quarto/R Markdown chunk). When `execute = FALSE`, the
#' code is only syntax-highlighted via Pandoc, which allows any language Pandoc
#' can highlight (e.g. `"rust"`, `"python"`) even if knitr cannot run it.
#'
#' The result is a fragment (Pandoc-generated `<style>` blocks plus content),
#' not a full HTML document. Figures are embedded as data URIs.
#'
#' @param code Character vector of source lines, or a single string.
#' @param language Language identifier. When executing, must be a language
#'   knitr can run (currently R).
#' @param execute Whether to run the code and capture output/figures.
#' @param style Pandoc highlight style, e.g. `"pygments"`.
#' @param background CSS background color for the source-code block, or `NULL`.
#' @param collapse Logical; if `TRUE` (the default), interleave source code and
#'   printed output in one code block using knitr's `collapse` option. Output
#'   lines are prefixed with `#>`.
#' @param knitr_opts Named list of knitr chunk options (used only when
#'   executing), e.g. `list(fig.width = 6, dpi = 150)`. Values here override the
#'   defaults set internally (including `fig.cap`). To list all
#'   options, run `knitr::opts_chunk$get()`. These are described at:
#'   (https://yihui.org/knitr/options/)
#' @param pandoc_opts Character vector of extra Pandoc arguments.
#' @param envir Environment in which to evaluate `code` when `execute = TRUE`.
#'   Defaults to the calling environment.
#' @return A character string containing a self-contained HTML fragment.
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' code <- '
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   labs(
#'     title = "Fuel economy by vehicle weight",
#'     x = "Weight (1000 lbs)",
#'     y = "Miles per gallon",
#'     color = "Cylinders"
#'   ) +
#'   theme_minimal()
#' '
#'
#' # Render code, plot, and any printed output as an HTML fragment. The plot
#' # is rendered at 10 x 5.625 inches (16:9) and 300 dpi before being embedded
#' # in the returned HTML.
#' html <- render_code_html(
#'   code,
#'   knitr_opts = list(
#'     fig.width = 10,
#'     fig.height = 5.625,
#'     dpi = 150,
#'     dev = "jpg"
#'   )
#' )
#'
#' view_html(html)
#'
#' clipboard_write_formats(
#'   "HTML Format" = html
#' )
#'
#' # Highlight code without evaluating it. Any language recognized by Pandoc
#' # can be used when execute = FALSE.
#' python_html <- render_code_html(
#'   "x = [1, 2, 3]\nprint(sum(x))",
#'   language = "python",
#'   execute = FALSE
#' )
#' }
#' @export
render_code_html <- function(code,
                             language = "r",
                             execute = TRUE,
                             style = "pygments",
                             background = "#eeeeee",
                             collapse = TRUE,
                             knitr_opts = list(),
                             pandoc_opts = character(),
                             envir = parent.frame()) {
  language <- stringr::str_to_lower(language)
  code <- stringr::str_c(code, collapse = "\n")
  if (!is.environment(envir)) {
    cli::cli_abort("{.arg envir} must be an environment.")
  }
  if (execute && language != "r") {
    cli::cli_warn("Execution is currently only supported for R; highlighting {.val {language}} without execution.")
    execute <- FALSE
  }

  if (!execute) {
    markdown <- stringr::str_c("```{.", language, "}\n", code, "\n```\n")
    args <- c(
      "-f", "markdown",
      "-t", "html",
      "--standalone",
      stringr::str_c("--highlight-style=", style),
      pandoc_opts
    )
    html <- run_pandoc(args, input = markdown)
    html <- add_sourcecode_css(html, background = background)
    return(html_extract_source_code_fragment(html))
  }

  # ---- execute path ----
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
    collapse = collapse,
    fig.cap = NA_character_,
    fig.path = stringr::str_c(figdir, "/")
  )
  if (length(knitr_opts) > 0L) {
    knitr::opts_chunk$set(knitr_opts)
  }

  text <- c("```{r}", code, "```")
  md <- file.path(workdir, "output.md")
  knitr::knit(text = text, output = md, envir = envir)
  md_text <- readLines(md) |>
    stringr::str_replace_all(stringr::fixed(workdir), ".")
  writeLines(md_text, md)

  html_file <- file.path(workdir, "output.html")
  args <- c(
    "-f", "markdown",
    "-t", "html",
    "--standalone",
    "--embed-resources",
    "--resource-path", dirname(md),
    stringr::str_c("--highlight-style=", style),
    pandoc_opts,
    "-o", html_file,
    md
  )
  run_pandoc_to_file(args, html_file)

  html <- readr::read_file(html_file)
  html <- add_sourcecode_css(html, background = background)
  html_document_to_fragment(html)
}

#' Add yclip presentation CSS for Pandoc source-code blocks
#' @noRd
add_sourcecode_css <- function(html,
                               background = "#eeeeee",
                               padding = "0.6em 0.8em",
                               border_radius = "4px",
                               border = NULL) {
  if (is.null(background)) {
    return(html)
  }
  css_parts <- c(
    "div.sourceCode {",
    stringr::str_c("background-color: ", background, ";"),
    stringr::str_c("padding: ", padding, ";"),
    stringr::str_c("border-radius: ", border_radius, ";"),
    if (!is.null(border)) stringr::str_c("border: ", border, ";"),
    "}"
  )
  css <- stringr::str_c(
    "<style>\n",
    stringr::str_c(css_parts,collapse = "\n"),
    "\n</style>"
  )
  stringr::str_c(html,"\n",css)
}

#' @rdname render_code_html
#' @export
syntax_highlighting <- function(text_code,
                                language,
                                style = "pygments",
                                background = "#eeeeee",
                                pandoc_opts = character()) {
  if (!is.character(text_code) || length(text_code) != 1L || is.na(text_code)) {
    cli::cli_abort("{.arg text_code} must be a single non-missing character string.")
  }
  if (!is.character(language) || length(language) != 1L || is.na(language) || !nzchar(language)) {
    cli::cli_abort("{.arg language} must be a single non-missing, non-empty character string.")
  }
  if (!is.character(style) || length(style) != 1L || is.na(style)) {
    cli::cli_abort("{.arg style} must be a single non-missing character string.")
  }
  if (!is.null(background) && (!is.character(background) || length(background) != 1L || is.na(background) || !nzchar(background))) {
    cli::cli_abort("{.arg background} must be {.code NULL} or a single non-missing, non-empty character string.")
  }
  if (!is.character(pandoc_opts) || anyNA(pandoc_opts)) {
    cli::cli_abort("{.arg pandoc_opts} must be a character vector with no missing values.")
  }
  render_code_html(
    code = text_code,
    language = language,
    execute = FALSE,
    style = style,
    background = background,
    pandoc_opts = pandoc_opts
  )
}


#' @export
read.clipboard.rcode.html <- function() {
  code <- clipboard_read_text("CF_UNICODETEXT")
  html <- render_code_html(code)
  clipboard_write_formats("HTML Format" = html)
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


#' Run Pandoc, returning captured stdout as a single string
#' @noRd
run_pandoc <- function(args, input = NULL) {
  pandoc <- find_pandoc()
  result <- system2(pandoc, args, input = input, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) {
    cli::cli_abort(c(
      "Pandoc failed (status {status}).",
      "i" = "{stringr::str_c(result, collapse = '\n')}"
    ))
  }
  stringr::str_c(result, collapse = "\n")
}

#' Run Pandoc that writes to an output file (args already include -o <file>)
#' @noRd
run_pandoc_to_file <- function(args, output_file) {
  pandoc <- find_pandoc()
  out <- system2(pandoc, args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if ((!is.null(status) && status != 0L) || !file.exists(output_file)) {
    cli::cli_abort(c(
      "Pandoc failed to produce {.file {output_file}}.",
      "i" = "{stringr::str_c(out, collapse = '\n')}"
    ))
  }
  invisible(output_file)
}





#' Extract the <style> + first source-code block (unexecuted highlighting)
#' @noRd
html_extract_source_code_fragment <- function(html) {
  if (!is.character(html) || length(html) != 1L || is.na(html)) {
    cli::cli_abort("{.arg html} must be a single non-missing character string.")
  }
  style_blocks <- stringr::str_extract_all(
    html,
    stringr::regex(
      "<style\\b[^>]*>.*?</style\\s*>",
      dotall = TRUE,
      ignore_case = TRUE
    )
  )[[1]]
  code_block <- stringr::str_extract(
    html,
    stringr::regex(
      "<div class=\"sourceCode\".*?</div\\s*>",
      dotall = TRUE,
      ignore_case = TRUE
    )
  )
  if (length(style_blocks) == 0L || is.na(code_block)) {
    cli::cli_abort("Pandoc did not produce the expected syntax-highlighted HTML.")
  }
  stringr::str_c(
    stringr::str_c(style_blocks,collapse = "\n"),
    "\n",
    code_block
  )
}

#' Extract a self-contained fragment: all <style> blocks + inner <body>
#' @noRd
html_document_to_fragment <- function(html) {
  if (!is.character(html) || length(html) != 1L || is.na(html)) {
    cli::cli_abort("{.arg html} must be a single non-missing character string.")
  }
  style_blocks <- stringr::str_extract_all(
    html,
    stringr::regex("<style\\b[^>]*>.*?</style\\s*>", dotall = TRUE, ignore_case = TRUE)
  )[[1]]
  body <- stringr::str_match(
    html,
    stringr::regex("<body\\b[^>]*>(.*?)</body\\s*>", dotall = TRUE, ignore_case = TRUE)
  )[, 2]
  if (is.na(body)) {
    cli::cli_abort("Pandoc did not produce an HTML {.field <body>} element.")
  }
  styles <- stringr::str_c(style_blocks, collapse = "\n")
  stringr::str_c(styles, "\n", stringr::str_trim(body))
}




#' View HTML in Viewer
#'
#' @param html character vector with HTML
#' @param title
#'
#' @return
#' @export
#'
#' @examples
view_html <- function(html,title = "yclip HTML") {
  if (!is.character(html) || length(html) != 1L || is.na(html)) {
    cli::cli_abort("{.arg html} must be a single non-missing character string.")
  }
  document <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$title(title)
    ),
    htmltools::tags$body(
      htmltools::HTML(html)
    )
  )
  htmltools::html_print(
    document,
    viewer = getOption("viewer",utils::browseURL)
  )
  invisible(html)
}

