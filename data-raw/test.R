


library(yclip)
library(yingtools2)
library(tidyverse)


# pkgbuild::unload_dll("yclip")

clipboard_inspect()

# clipboard_read_raw("Rich Text Format")

rtf <- clipboard_read_text("Rich Text Format")
rtf


show_shapes()


clip_formats_available()

gts <- clip_read("html")

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



