


library(yclip)
library(yingtools2)
library(tidyverse)


clipboard_inspect()



get_clipboard_format_id

# word
rtf <- clipboard_read_text("Rich Text Format")
html <- clipboard_read_text("HTML Format")
# text
text <- clipboard_read_text("CF_TEXT")
unicode <- clipboard_read_text("CF_UNICODETEXT")
# photos
dib <- clipboard_read_raw("CF_DIB")
clipboard_inspect_dib()

yclip::clipboard_inspect()
yclip:::clipboard_inspect_windows()


# bitmaps: PNG, GIF, CF_DIB

# devtools::document()
# devtools::load_all

clipboard_read_text("")


library(Rcpp)
devtools::load_all()
Rcpp::sto
show_shapes()


clip_formats_available()
gts <- clip_read("html")
txt <- clip_read("text")
html <- clip_read("html")

clipboard_read_raw("html")

yclip:::clipboard_read_raw("HTML Format")
yclip:::clipboard_read_raw("Text")

yclip:::clipboard_read_raw_windows("html")
yclip:::clipboard_read_raw_windows("HTML Format")
clipboard_read_raw("Html")

clipboard_read_raw(format_name)



yclip:::clipboard_read_raw(format_name = "Html")

html <- clipboard_read_raw("Html")
clipboard_read_raw("Html")


read_clipboard_raw("Html")


yclip:::clipboard_read_raw_windows("Html")
yclip:::clipboard_read_raw_windows("html")
html

html %>% class


clip_write(html,"html")





clip_write(html,"text")
# clip_write(html,"rtf")



