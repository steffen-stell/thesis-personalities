library(tidyverse)
library(tidytext)
library(tm)

short <- tibble(publication = c("BILD Bund", "Der Tagesspiegel", "Die Welt", 
                       "Frankfurter Rundschau", "taz, die tageszeitung"), 
  pub = c("bild", "ts", "welt", "fr", "taz"))

tx <- "data/lexis/lexis_frame.rds" |> 
  read_rds() |> 
  left_join(short) |>
  replace_na(list(page_section_prefix = "", subsection = "")) |> 
  mutate(
    sec = case_when(
      subsection != "" ~ str_c(section, "-", subsection),
      page_section_prefix != "" ~ str_c(section, "-", page_section_prefix),
      T ~ section
      )
    ) |> 
  select(pub, sec, text, date, id) 

tx |> 
  unnest_tokens(sent, text, "sentences", to_lower = F)

tx |> 
  count(sent, sort = T) 

tx |> 
  filter(
  str_detect(sent, r"(\(?http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+)"
             )
  ) |> View()
 

crps <- tx$text |> 
  VectorSource() |> 
  Corpus()

crps |> 
  tree()

tx$text |> 
  str_remove_all(r"(https?://[^)\]\s]+(?=[)\]\s]))") |> 
  na.omit()
