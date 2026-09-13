



library(yclip)
library(yingtools2)
library(tidyverse)
library(magick)



# word RTF ----------------------------------------------------------------

clipboard_inspect()
# word

rtf <- clipboard_read_text("Rich Text Format")
html <- clipboard_read_text("HTML Format")
text <- clipboard_read_text("CF_TEXT")
unicode <- clipboard_read_text("CF_UNICODETEXT")

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

clipboard_inspect()
rtf2 <- clipboard_read_text("Rich Text Format")
html2 <- clipboard_read_text("HTML Format")
text2 <- clipboard_read_text("CF_TEXT")
unicode2 <- clipboard_read_text("CF_UNICODETEXT")



# bitmaps -----------------------------------------------------------------

# CF_DIB
# CF_BITMAP doesn't work yet
# CF_DIBV5
# PNG
# JFIF
# GIF
shell.exec("powerpoint.pptx")
clipboard_inspect()
raw_dib <- clipboard_read_raw("CF_DIB")
raw_dibv5 <- clipboard_read_raw("CF_DIBV5")
raw_png <- clipboard_read_raw("PNG")
raw_jfif<- clipboard_read_raw("JFIF")
raw_gif <- clipboard_read_raw("GIF")

# raw_bitmap <- clipboard_read_raw("CF_BITMAP")

clipboard_write_formats(
  "CF_DIB" = raw_dib,
  "CF_DIBV5" = raw_dibv5,
  "PNG" = raw_png,
  "JFIF" = raw_jfif,
  "GIF" = raw_gif
)
clipboard_inspect()


# img <- image_read("pic.jpg")
# print(img)
jpg <- image_read(raw_jfif)
gif <- image_read(raw_gif)
png <- image_read(raw_png)
dib <- image_read(dib_to_bmp(raw_dib))
dibv5 <- image_read(dib_to_bmp(raw_dibv5))

print(jpg)
print(gif)
print(png)
print(dib)
print(dibv5)

png2 <- image_resize(png, "50x")
jpg2 <- image_resize(jpg, "50x")
gif2 <- image_resize(gif, "50x")
dib2 <- image_resize(dib, "50x")
dibv52 <- image_resize(dibv5, "50x")

print(png2)
print(jpg2)
print(gif2)
print(dib2)
print(dibv52)


raw_png2 <- image_write(png2, format = "png")
raw_jfif2 <- image_write(jpg2, format = "jpg")
raw_gif2 <- image_write(gif2, format = "gif")
raw_dib2 <- magick_to_cf_dib(dib2)
raw_dibv52 <- magick_to_cf_dibv5(dibv52)


clipboard_write_formats(
  "PNG" = raw_png2,
  "JFIF" = raw_jfif2,
  "GIF" = raw_gif,
  "CF_DIB" = raw_dib2,
  "CF_DIBV5" = raw_dibv52
)

clipboard_write_formats(
  "PNG" = raw_png2
)
clipboard_write_formats(
  "JFIF" = raw_jfif2
)
clipboard_write_formats(
  "GIF" = raw_gif
)
clipboard_write_formats(
  "CF_DIB" = raw_dib2
)
clipboard_write_formats(
  "CF_DIBV5" = raw_dibv52
)


bmp <- dib_to_bmp(raw_dib)
img <- image_read(bmp)
image_info(img)



# ggplot2 -----------------------------------------------------------------



g <- ggplot(mtcars,aes(x=mpg,y=hp)) + geom_point() +
  theme(
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )
g

# writing file would have been:
# png("gg.png",units="in",res=150,width=4,height=3,bg="transparent")
# g
# dev.off()

img <- image_graph(bg = "transparent")
g
dev.off()

png_raw <- image_write(img, format = "png")
clipboard_write_formats(
  "PNG" = png_raw
)









