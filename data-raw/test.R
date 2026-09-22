
library(yclip)
library(yingtools2)
library(tidyverse)
rm(list=ls())

# word RTF ----------------------------------------------------------------

shell.exec("epic_test.rtf")
clipboard_inspect()
# word

rtf <- clipboard_read_text("Rich Text Format")
html <- clipboard_read_text("HTML Format")
text <- clipboard_read_text("CF_TEXT")
unicode <- clipboard_read_text("CF_UNICODETEXT")

clipboard_write_formats(
  "Rich Text Format" = rtf,
  "HTML Format" = html,
  "CF_TEXT" = text,
  "CF_UNICODETEXT" = unicode
)
clipboard_write_formats("Rich Text Format" = rtf)
clipboard_write_formats("HTML Format" = html)
clipboard_write_formats("CF_UNICODETEXT" = unicode)
clipboard_write_formats("CF_TEXT" = text)
clipboard_isolate("Rich Text Format")


# read in as raw (but CF_TEXT and CF_UNICODETEXT can have issues)
raw_rtf <- clipboard_read_raw("Rich Text Format")
raw_html <- clipboard_read_raw("HTML Format")
raw_text <- clipboard_read_raw("CF_TEXT")
raw_unicode <- clipboard_read_raw("CF_UNICODETEXT")

clipboard_write_formats(
  "Rich Text Format" = raw_rtf,
  "HTML Format" = raw_html,
  "CF_TEXT" = raw_text,
  "CF_UNICODETEXT" = raw_unicode
)
clipboard_write_formats("Rich Text Format" = raw_rtf)
clipboard_write_formats("HTML Format"=raw_html)
clipboard_write_formats("CF_TEXT"=raw_text)
clipboard_write_formats("CF_UNICODETEXT"=raw_unicode)


# these have issues:
clipboard_write_formats("CF_TEXT"=charToRaw("plain text here"))
clipboard_write_formats("CF_UNICODETEXT"=charToRaw("unicode text here"))


# bitmaps -----------------------------------------------------------------

# CF_DIB
# CF_BITMAP doesn't work yet
# CF_DIBV5
# PNG
# JFIF
# GIF
shell.exec("epic_test.rtf")
#copy photo
clipboard_inspect()

jpg <- clipboard_read_image("JFIF")
gif <- clipboard_read_image("GIF")
png <- clipboard_read_image("PNG")
dib <- clipboard_read_image("CF_DIB")
dibv5 <- clipboard_read_image("CF_DIBV5")
# CF_BITMAP - old Windows bitmap object clipboard format (awkward)
# CF_ENHMETAFILE - WMF format, old
# CF_METAFILEPICT - EMF vector graphics
# CF_METAFILEPICT - old format

# choose from available formats by default
img <- clipboard_read_image()

img2 <- image_resize(img, "50x")

clipboard_write_formats(
  "CF_DIB" = img2,
  "CF_DIBV5" = img2,
  "PNG" = img2,
  "JFIF" = img2,
  "GIF" = img2
)
clipboard_write_formats("PNG" = img2)
clipboard_write_formats("JFIF" = img2)
clipboard_write_formats("GIF" = img2)
clipboard_write_formats("CF_DIB" = img2)
clipboard_write_formats("CF_DIBV5" = img2)

# direct raw
raw_dib <- clipboard_read_raw("CF_DIB")
raw_dibv5 <- clipboard_read_raw("CF_DIBV5")
raw_png <- clipboard_read_raw("PNG")
raw_jfif<- clipboard_read_raw("JFIF")
raw_gif <- clipboard_read_raw("GIF")
clipboard_write_formats(
  "CF_DIB" = raw_dib,
  "CF_DIBV5" = raw_dibv5,
  "PNG" = raw_png,
  "JFIF" = raw_jfif,
  "GIF" = raw_gif
)




# ggplot2 -----------------------------------------------------------------



pdf2 <- function(filename,
                 width = 7,
                 height = 7,
                 bg = "transparent",
                 ...) {
  # To avoid embedded font issue:
  #   - cairo_pdf()
  #   - ggsave(..., type = cairo_pdf)
  # To allow transparent/alpha
  #   - cairo_pdf(..., bg = "transparent")
  #   - pdf(..., bg = "transparent")
  #   - ggsave()
  cairo_pdf(filename = filename,
            width = width,
            height = height,
            bg = bg,
            ...)
}

png2 <- function(filename,
                 width = 7,
                 height = 7,
                 units = "in",
                 res = 300,
                 bg = "transparent",
                 type = "cairo",
                 ...) {
  # Ideal: png(..., bg = "transparent", type="cairo")
  #   - png with default 'type=' prevents alpha
  #   - type="cairo-pdf" also fine
  #   - ggsave(pngfile) leads to different symbols (mu), and sometimes warnings
  png(filename = filename,
      width = width,
      height = height,
      bg = bg,
      units = units,
      res = res,
      type = type,
      ...)
}


copy.ggplot <- function(plot = get_last_plot(),
                        width = 7,
                        height = 7,
                        units = "in",
                        res = 300,
                        bg = "transparent",
                        ...) {
  dim <- grDevices:::.geometry(width, height, units, res)
  img <- image_graph(height = dim$height,
                     width = dim$width,
                     res = res,
                     bg = bg,
                     ...)
  print(plot)
  dev.off()
  clipboard_write_formats(
    "CF_DIB" = img,
    "CF_DIBV5" = img,
    "PNG" = img,
    "JFIF" = img,
    "GIF" = img
  )
  img
}

g <- ggplot(mtcars, aes(x=mpg, y=hp, color=factor(cyl), size=wt)) +
  geom_point(alpha=0.7) +
  scale_x_continuous(name = bquote("Measurement"~(mu*g/L))) +
  theme(plot.background = element_rect(fill='transparent', color="black"), #transparent plot bg
        panel.background = element_rect(fill=alpha("gray",0.5), color=NA)) +
  labs(subtitle = bquote("Normal text"~(mu*g/L)~(over(mu*g, L))~sqrt(italic(x))),
       y = bquote(R[adj]^2==0.41))
g


pdf2("test1.pdf",height=5,width=8)
g + ggtitle("PDF")
dev.off()
img1 <- image_read_pdf("test1.pdf")

png2("test2.png",height=5,width=8)
g + ggtitle("PNG")
dev.off()
img2 <- image_read("test2.png")

img3 <- copy.ggplot(g + ggtitle("magick"),height=5,width=8)
img1
img2
img3



# word RTF convert to HTML ------------------------------------------------


# Requires RDCOMClient
yingtools2::shell.exec("epic_test.rtf")
# copy all
clipboard_inspect()
rtf <- clipboard_read_text("Rich Text Format")
html <- clipboard_read_text("HTML Format")

# convert
html2 <- word_convert_rtf_to_html(rtf)
write_lines(html2,"asdf.html")


html %>% htmltools::HTML() %>% htmltools::browsable()
html2 %>% htmltools::HTML() %>% htmltools::browsable()


# test paste --------------------------------------------------------------


text <- "CF_TEXT"
unicode <- "CF_UNICODETEXT"
rtf <- "{\\rtf1\\ansi{\\colortbl;\\red255\\green0\\blue0;}\\cf1 Rich Text Format}"
html <- 'Version:0.9
StartHTML:00000097
EndHTML:00000208
StartFragment:00000129
EndFragment:00000176
<html><body><!--StartFragment--><span style="color:blue">HTML Format here</span><!--EndFragment--></body></html>'
make_test_img <- function(txt,color) {
  img <- image_graph()
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  rect(
    xleft = 0.2, ybottom = 0.35,
    xright = 0.8, ytop = 0.65
  )
  text(0.5, 0.5, txt, cex = 5, col = color)
  dev.off()
  return(img)
}
png <- make_test_img("PNG","#00AA00")
gif <- make_test_img("GIF","#7A5980")
jfif <- make_test_img("JFIF","#4D8ABC")
dib <- make_test_img("CF_DIB","#AD3E01")
dibv5 <- make_test_img("CF_DIBV5","#B98E24")


clipboard_write_formats(
  # order of preference for word
  "HTML Format" = html,
  "Rich Text Format" = rtf,
  "PNG" = png,
  "JFIF" = jfif,
  "GIF" = gif,
  "CF_UNICODETEXT" = unicode,
  "CF_TEXT" = text,
  # whatever order left
  "CF_DIBV5" = dibv5,
  "CF_DIB" = dib
)

clipboard_inspect()

clipboard_write_formats("CF_TEXT" = text)
clipboard_write_formats("CF_UNICODETEXT" = unicode)
clipboard_write_formats("Rich Text Format" = rtf)
clipboard_write_formats("HTML Format" = html)
clipboard_write_formats("PNG" = png)
clipboard_write_formats("GIF" = gif)
clipboard_write_formats("JFIF" = jfif)
clipboard_write_formats("CF_DIB" = dib)
clipboard_write_formats("CF_DIBV5" = dibv5)


clipboard_write_formats(
  # "CF_DIBV5" = dibv5,
  # "CF_DIB" = dib

  "HTML Format" = html,
  "Rich Text Format" = rtf,
  "PNG" = png,
  "JFIF" = jfif,
  "GIF" = gif,
  "CF_UNICODETEXT" = unicode,
  "CF_TEXT" = text,
  # whatever order left

)




# flextable and gt --------------------------------------------------------------------
library(yclip)
library(flextable)
library(gt)
library(officer)
library(gtExtras)
library(htmltools)
library(tidyverse)

# gt_tab <- iris %>%
#   gt_plt_summary() %>%
#   tab_style(style=list(cell_text(style="italic"),
#                        cell_fill(color="blue",alpha=0.25)),
#             locations=cells_column_labels(columns=SD))

gt_tab <- iris %>% group_by(Species) %>%
  slice(1:4) %>% ungroup() %>% gt() %>%
  tab_spanner(label = "Sepal", columns = c(Sepal.Length, Sepal.Width)) %>%
  tab_spanner(label = "Petal", columns = c(Petal.Length, Petal.Width)) %>%
  cols_label(Sepal.Length = "Length", Sepal.Width = "Width",
             Petal.Length = "Length", Petal.Width = "Width",
             Species = "Species") %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_spanners()) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) %>%
  tab_style(style = cell_text(style = "italic"),
            locations = cells_column_labels(columns = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))) %>%
  tab_style(style = cell_fill(color = "yellow"),
            locations = cells_body(columns = Species, rows = Species == "versicolor")) %>%
  data_color(columns = Sepal.Length,
             fn = scales::col_numeric(palette = c("wheat", "red"),domain = range(iris$Sepal.Length)))

ft_tab <- iris %>%
  group_by(Species) %>% slice(1:4) %>% ungroup() %>% flextable() %>%
  separate_header(split=".", opts = c("span-top","center-hspan"), fixed=TRUE) %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>% italic(i = 2, part = "header") %>%
  highlight(i = ~Species=="versicolor", j = "Species", color="yellow") %>%
  bg(j = "Sepal.Length", bg = scales::col_numeric(
    palette = c("wheat", "red"),
    domain = range(iris$Sepal.Length))) %>% autofit()

ft_tab
gt_tab
# 4 tables
gt_html <- gt::as_raw_html(gt_tab)
gt_rtf <- gt::as_rtf(gt_tab)
ft_html <- officer::to_html(ft_tab)
con <- textConnection("ft_rtf", "w", local = TRUE)
flextable::save_as_rtf(ft_tab, path = con, pr_section = prop_section())
close(con)
ft_rtf <- paste0(ft_rtf, collapse = "\n")


writeLines(gt_html,"gt.html") # good
writeLines(ft_html,"ft.html") # good
writeLines(gt_rtf,"gt.rtf") # unformatted in word, unreadable wordpad
writeLines(ft_rtf,"ft.rtf") # good

clipboard_write_formats("Rich Text Format" = gt_rtf)
## pasting into:
# word - unformatted
# wordpad - unreadable
# epic - unformatted

clipboard_write_formats("HTML Format" = gt_html) # looks ok, has bold headers and border formatting, but no cell shading
# word - good
# wordpad - N/A
# epic - good

clipboard_write_formats("HTML Format" = ft_html)
# word - lost colors and borders, kept bold headers
# wordpad - N/A
# epic - kept colors+bold, lost borders

clipboard_write_formats("Rich Text Format" = ft_rtf) # excellent
# word - good
# wordpad - good
# epic - lost table structure

ft_rtf_fixed <- yclip:::clipboard_fix_flextable_rtf(ft_rtf)
clipboard_write_formats("Rich Text Format" = ft_rtf_fixed)

# word - good
# wordpad - good
# epic - good

clipboard_write_formats("Rich Text Format" = ft_tab)
# word - good
# wordpad - good
# epic - good




# R coding: code blocks through pandoc ---------------------------------------------------


code <- "# Load data and calculate summary statistics\nlibrary(dplyr)\ndata <- data.frame(\n  name = c(\"Alice\", \"Bob\", \"Carol\", \"David\"),\n  score = c(91, 84, 97, NA)\n)\nresult <- data %>%\n  filter(score >= 85) %>%\n  summarise(mean_score = mean(score))\nprint(result)"

html <- syntax_highlighting(code, language = "r")
clipboard_write_formats("HTML Format" = html)
html %>% htmltools::HTML() %>% htmltools::browsable()

# OR,
clipboard_write_formats("CF_UNICODETEXT"=code)
read.clipboard.rcode()

# huge example
examples <- list(r = "# Load data and calculate summary statistics\nlibrary(dplyr)\ndata <- data.frame(\n  name = c(\"Alice\", \"Bob\", \"Carol\", \"David\"),\n  score = c(91, 84, 97, NA)\n)\nresult <- data %>%\n  filter(score >= 85) %>%\n  summarise(mean_score = mean(score))\nprint(result)",
                 python = "# Load data and calculate summary statistics\nimport pandas as pd\ndata = pd.DataFrame({\n    \"name\": [\"Alice\", \"Bob\", \"Carol\", \"David\"],\n    \"score\": [91, 84, 97, 76]\n})\nresult = data[data[\"score\"] >= 85]\nmean_score = result[\"score\"].mean()\nprint(f\"Mean score: {mean_score:.1f}\")",
                 javascript = "// Filter users and calculate their average age\nconst users = [\n  { name: \"Alice\", age: 32, active: true },\n  { name: \"Bob\", age: 41, active: false },\n  { name: \"Carol\", age: 28, active: true }\n];\nconst active = users.filter(user => user.active);\nconst meanAge = active.reduce((sum, user) => sum + user.age, 0) / active.length;\nconsole.log(`Mean age: ${meanAge.toFixed(1)}`);",
                 typescript = "interface User {\n  name: string;\n  age: number;\n  active: boolean;\n}\nconst users: User[] = [\n  { name: \"Alice\", age: 32, active: true },\n  { name: \"Bob\", age: 41, active: false }\n];\nconst activeUsers = users.filter(user => user.active);\nconsole.log(activeUsers.length);",
                 sql = "-- Find high-scoring users and calculate their average\nSELECT\n    department,\n    COUNT(*) AS n_users,\n    AVG(score) AS mean_score\nFROM users\nWHERE score >= 85\nGROUP BY department\nHAVING COUNT(*) > 2\nORDER BY mean_score DESC;",
                 bash = "# Find recently modified R files\nfor file in *.R; do\n    if [ -f \"$file\" ]; then\n        echo \"Processing $file\"\n        wc -l \"$file\"\n    fi\ndone\necho \"Finished processing files\"\nmkdir -p results\ncp *.csv results/",
                 json = "{\n  \"experiment\": {\n    \"name\": \"microbiome study\",\n    \"version\": 2,\n    \"active\": true,\n    \"samples\": [\"A01\", \"A02\", \"B01\"],\n    \"metadata\": {\n      \"organism\": \"human\",\n      \"temperature\": 37.5\n    }\n  }\n}",
                 cpp = "#include <iostream>\n#include <vector>\n#include <numeric>\n\nint main() {\n    std::vector<int> values = {1, 2, 3, 4, 5};\n    int total = std::accumulate(values.begin(), values.end(), 0);\n    std::cout << \"Total: \" << total << std::endl;\n    return 0;\n}",
                 rust = "fn main() {\n    let values = vec![1, 2, 3, 4, 5];\n    let total: i32 = values.iter().sum();\n    let doubled: Vec<i32> = values\n        .iter()\n        .map(|x| x * 2)\n        .collect();\n    println!(\"Total: {}\", total);\n    println!(\"Values: {:?}\", doubled);\n}",
                 java = "public class Example {\n    public static void main(String[] args) {\n        int[] values = {1, 2, 3, 4, 5};\n        int total = 0;\n        for (int value : values) {\n            total += value;\n        }\n        System.out.println(\"Total: \" + total);\n    }\n}")

html_examples <- purrr::imap(examples, ~{
  html <- syntax_highlighting(.x, language = .y, background = "#eeeeee")
  paste0("<h2>",.y,"</h2>",html)
}) %>% paste(collapse="\n")

html_examples %>% htmltools::HTML() %>% htmltools::browsable()
clipboard_write_formats("HTML Format" = html_examples)


# R coding: console output  ----------------------------------------------------------------

library(tidyverse)
library(yclip)
cli::ansi_palette_show()
glimpse(starwars[,1:5])
# copy output

clipboard_inspect()

html_cobalt_correct <- clipboard_read_text("HTML Format")
# html_textmate_correct <- clipboard_read_text("HTML Format")
# html_dracula_correct <- clipboard_read_text("HTML Format")
# html_solarizedlight_correct <- clipboard_read_text("HTML Format")

# clipboard_write_formats("HTML Format"=html_cobalt_correct)
# clipboard_write_formats("HTML Format"=html_dracula_correct)
# clipboard_write_formats("HTML Format"=html_textmate_correct)
# clipboard_write_formats("HTML Format"=html_solarizedlight_correct)
html_cobalt_correct %>% htmltools::HTML() %>% htmltools::browsable()

dracula <- read_rstudio_theme("C:/Program Files/RStudio/resources/app/resources/themes/dracula.rstheme")
textmate <- read_rstudio_theme("C:/Program Files/RStudio/resources/app/resources/themes/textmate.rstheme")
cobalt <- read_rstudio_theme("C:/Program Files/RStudio/resources/app/resources/themes/cobalt.rstheme")
solarizedlight <- read_rstudio_theme("C:/Program Files/RStudio/resources/app/resources/themes/solarized_light.rstheme")

html_dracula_converted <- rstudio_html_restyle(html_cobalt_correct, dracula)
html_textmate_converted <- rstudio_html_restyle(html_cobalt_correct, textmate)
html_solarizedlight_converted <- rstudio_html_restyle(html_cobalt_correct, solarizedlight)

html_dracula_converted %>% htmltools::HTML() %>% htmltools::browsable()
html_textmate_converted %>% htmltools::HTML() %>% htmltools::browsable()
html_solarizedlight_converted %>% htmltools::HTML() %>% htmltools::browsable()

system.time({
  textmate <- read_rstudio_theme("C:/Program Files/RStudio/resources/app/resources/themes/textmate.rstheme")
  console <- clipboard_read_text("HTML Format")
  converted <- rstudio_html_restyle(console, textmate)
  clipboard_write_formats("HTML Format"=converted)
})


system.time({
  dracula <- read_rstudio_theme("C:/Program Files/RStudio/resources/app/resources/themes/dracula.rstheme")
  console <- clipboard_read_text("HTML Format")
  converted <- rstudio_html_restyle(console, dracula)
  clipboard_write_formats("HTML Format"=converted)
})


#copy some output text
html <- read.clipboard.rterminal()
html %>% htmltools::HTML() %>% htmltools::browsable()


# R coding: knitr ---------------------------------------------------------

# run this
library(yclip)
library(knitr)
library(ggplot2)
library(yingtools2)
df <- mtcars



code <- '
ggplot(df, aes(x=wt, y=mpg, color=disp, size=hp)) + geom_point()
pillar::glimpse(df)
'
html <- knit_code(code)
html %>% htmltools::HTML() %>% htmltools::browsable()


# copy this part
if (FALSE) {
  ggplot(df, aes(x=wt, y=mpg, color=disp, size=hp)) + geom_point()
  pillar::glimpse(df)
  cli::ansi_palette_show()
  cli::cli_alert_success("great")
  cli::cli_alert_danger("oh no")
  info(mtcars)
}

ggplot(df, aes(x=wt, y=mpg, color=disp, size=hp)) + geom_point()
pillar::glimpse(df)

read.rcode.make.knitr.html()






