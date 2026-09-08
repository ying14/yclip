


library(yclip)
library(yingtools2)
library(tidyverse)


# pkgbuild::unload_dll("yclip")

hello world
clipboard_inspect()
clipboard_inspect() %>% copy.to.clipboard()
getClipboardFormats()


# clipboard_read_raw("Rich Text Format")

rtf <- clipboard_read_text("Rich Text Format")
rtf
writeLines(rtf, "epic_table.rtf")

rtf %>% copy.to.clipboard()

# rtf  %>% copy.to.clipboard()

library(yingtools2)

show_shapes()

rows <- parse_rtf_rows(rtf)
rows %>% length()



tables <- parse_epic_tables(rows)

tables


rows <- parse_rtf_rows(rtf)
rows
tables <- parse_epic_tables(rows)
tables


tables
length(rows)
sapply(rows, length)

tables <- parse_rtf_tables(rtf)


tables


length(tables)

tables

rows
# I'd expect:

purrr::iwalk(rows, \(x, i) {
  cat("\n--- ROW", i, "---\n")
  print(x)
})

stringr::str_extract_all(
  rtf,
  "\\\\cell\\b"
)[[1]][1:20]

stringr::str_extract_all(
  rtf,
  "\\\\cell\\b|\\\\cellx[0-9]+"
)[[1]][1:20]





