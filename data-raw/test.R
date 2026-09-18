
library(yclip)
library(yingtools2)
library(tidyverse)
library(magick)
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
clipboard_write_formats(
  "Rich Text Format" = rtf
)
clipboard_write_formats(
  "HTML Format" = html
)
clipboard_write_formats(
  "CF_UNICODETEXT" = unicode
)
clipboard_write_formats(
  "CF_TEXT" = text
)

# read in as raw (but CF_TEXT and CF_UNICODETEXT can have issues)
raw_rtf <- clipboard_read_raw("Rich Text Format")
raw_html <- clipboard_read_raw("HTML Format")
raw_text <- clipboard_read_raw("CF_TEXT")
raw_unicode <- clipboard_read_raw("CF_UNICODETEXT")

clipboard_write_formats("Rich Text Format" = raw_rtf)
clipboard_write_formats("HTML Format"=raw_html)
clipboard_write_formats("CF_TEXT"=raw_text)
clipboard_write_formats("CF_UNICODETEXT"=raw_unicode)
# these have issues:
clipboard_write_formats("CF_TEXT"=charToRaw("plain text here"))
clipboard_write_formats("CF_UNICODETEXT"=charToRaw("unicode text here"))

# pandoc convert ----------------------------------------------------------

phtml <- rmarkdown::pandoc_convert(docx.file, to = "html")



write_lines(phtml,"out.html")

write_lines



# word RTF convert to HTML ------------------------------------------------

library(yclip)


yingtools2::shell.exec("epic_test.rtf")
clipboard_inspect()

rtf <- clipboard_read_text("Rich Text Format")
html <- clipboard_read_text("HTML Format")

html2 <- word_convert_rtf_to_html(rtf)

write_lines(html2,"asdf.html")
html2


# bitmaps -----------------------------------------------------------------

# CF_DIB
# CF_BITMAP doesn't work yet
# CF_DIBV5
# PNG
# JFIF
# GIF
shell.exec("powerpoint.pptx")
clipboard_inspect()

jpg <- clipboard_read_image("JFIF")
gif <- clipboard_read_image("GIF")
png <- clipboard_read_image("PNG")
dib <- clipboard_read_image("CF_DIB")
dibv5 <- clipboard_read_image("CF_DIBV5")
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
clipboard_write_formats(
  "PNG" = img2
)
clipboard_write_formats(
  "JFIF" = img2
)
clipboard_write_formats(
  "GIF" = img2
)
clipboard_write_formats(
  "CF_DIB" = img2
)
clipboard_write_formats(
  "CF_DIBV5" = img2
)


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
  labs(title = bquote("Normal text"~(mu*g/L)~(over(mu*g, L))~sqrt(italic(x))),
       y = bquote(R[adj]^2==0.41))
g


pdf2("test1.pdf",height=5,width=8)
g
dev.off()

png2("test2.png",height=5,width=8)
g
dev.off()

copy.ggplot(g,height=5,width=8)

img1 <- image_read_pdf("test1.pdf")
img2 <- image_read("test2.png")
img3 <- copy.ggplot(g,height=5,width=8)


img1
img2
img3


# convert rtf -------------------------------------------------------------


# library(officer)
# Cannonymous/rdcomclient
library(RDCOMClient)
library(yclip)

infile <- "C:/Users/Ying/R/yclip/epic_test.rtf"
shell.exec(infile)
clipboard_inspect()
rtf <- clipboard_read_text("Rich Text Format")
temp.rtf.infile <- tempfile("yclip_",fileext=".rtf")
temp.html.outfile <- tempfile("yclip_",fileext=".html")
write_lines(rtf,file=temp.rtf.infile)
# shell.exec(temp.rtf.infile)

wd <- COMCreate("Word.Application")
wd[["Visible"]] <- FALSE
doc <- wd$Documents()$Open(normalizePath(temp.rtf.infile))
doc$SaveAs2(normalizePath(temp.html.outfile, mustWork = FALSE), FileFormat = 8)
doc$Close()
wd$Quit()
shell.exec(temp.html.outfile)
html <- read_lines(temp.html.outfile)
html


word_convert_rtf_to_html <- function(rtf) {
  temp.rtf.infile <- tempfile("yclip_",fileext=".rtf")
  temp.html.outfile <- tempfile("yclip_",fileext=".html")
  write_lines(rtf,file=temp.rtf.infile)
  wd <- COMCreate("Word.Application")
  wd[["Visible"]] <- FALSE
  doc <- wd$Documents()$Open(normalizePath(temp.rtf.infile))
  doc$SaveAs2(normalizePath(temp.html.outfile, mustWork = FALSE), FileFormat = 8)
  doc$Close()
  wd$Quit()
  html <- read_lines(temp.html.outfile)
  unlink(temp.rtf.infile)
  unlink(temp.html.outfile)
  return(html)
}


html <- word_convert_rtf_to_html(rtf)


word_convert <- function(text, inext = "rtf", format = 8) {
  # wdSaveFormat enum: docx=16, doc97=0, RTF=6, filtered HTML=10, HTML=8, ODT=18, PDF=17, unicode=7

  inext <- str_replace(inext,"^[.]*",".")
  temp.infile <- tempfile("yclip_",fileext=inext)


  temp.html.outfile <- tempfile("yclip_",fileext=".html")
  write_lines(rtf,file=temp.rtf.infile)
  # shell.exec(temp.rtf.infile)

  wd <- COMCreate("Word.Application")
  wd[["Visible"]] <- FALSE
  doc <- wd$Documents()$Open(normalizePath(temp.rtf.infile))
  doc$SaveAs2(normalizePath(temp.html.outfile, mustWork = FALSE), FileFormat = 8)
  doc$Close()
  wd$Quit()
  shell.exec(temp.html.outfile)
  html <- read_lines(temp.html.outfile)
  html

}


infile <- "C:/Users/Ying/R/yclip/epic_test.rtf"
outfile <- "C:/Users/Ying/R/yclip/epic_test.html"


word_convert(infile,outfile,format=8)


wd <- COMCreate("Word.Application")
wd[["Visible"]] <- FALSE
doc <- wd$Documents()$Open(normalizePath(infile))
doc$SaveAs2(normalizePath(outfile, mustWork = FALSE), FileFormat = 8)
doc$Close()
wd$Quit()



infile <- "C:/Users/Ying/R/yclip/epic_test.rtf"
outfile <- "C:/Users/Ying/R/yclip/epic_test.html"
wd <- COMCreate("Word.Application")
wd[["Visible"]] <- FALSE
doc <- wd$Documents()$Open(normalizePath(infile))
doc$SaveAs2(normalizePath(outfile, mustWork = FALSE), FileFormat = 8)
doc$Close()
wd$Quit()










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

clipboard_write_formats("CF_TEXT" = text)
clipboard_write_formats("CF_UNICODETEXT" = unicode)
clipboard_write_formats("Rich Text Format" = rtf)
clipboard_write_formats("HTML Format" = html)
clipboard_write_formats("PNG" = png)
clipboard_write_formats("GIF" = gif)
clipboard_write_formats("JFIF" = jfif)
clipboard_write_formats("CF_DIB" = dib)
clipboard_write_formats("CF_DIBV5" = dibv5)


image_write()

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

