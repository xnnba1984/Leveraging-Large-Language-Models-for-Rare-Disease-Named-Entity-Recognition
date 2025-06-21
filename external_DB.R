library(httr)
library(xml2)
library(dplyr)
library(arrow)
library(stringr)

#######################################################
# Orpha
#######################################################
# 1. download the 20 MB XML (~4 250 diseases)
url   <- "https://www.orphadata.com/data/xml/en_product1.xml"
dest  <- tempfile(fileext = ".xml")
GET(url, write_disk(dest, overwrite = TRUE))

## 2 ─ parse XML --------------------------------------------------------------
doc   <- read_xml(dest)
items <- xml_find_all(doc, ".//Disorder")

orpha_df <- tibble(
  #orpha_id = as.integer(xml_text(xml_find_first(items, "./OrphaNumber"))),
  name     = xml_text(xml_find_first(items, "./Name")),
  snippet  = xml_text(xml_find_first(items, ".//Contents"))
)

orpha_df <- orpha_df[complete.cases(orpha_df),]
orpha_df <- orpha_df %>%       
  mutate(content = paste(name, snippet, sep = ": "))
orpha_df <- orpha_df %>%                               # your data frame
  mutate(
    n_tokens = str_count(content, "\\S+")        # counts 'words'
  )

hist(orpha_df$n_tokens)

write.csv(orpha_df, 'data/orpha.csv', row.names = F)
