library(tidyverse)
library(officer)
library(readODS)
library(furrr)
library(lubridate)
library(janitor)

plan(multisession, workers = parallel::detectCores()) 

# Parse LexisNexis docx files ------------------------------------------------

# inital import and docx parsing. Slow!
lex <- list.files(
  path = "data/lexis", 
  pattern = "DOCX$",
  full.names = T
  ) |> 
  future_map(
    \(path) path |> 
      read_docx() |> 
      docx_summary() |> 
      as_tibble() |> 
      filter(!str_detect(text, "^\\s*$")) # Omit empty or whitespace only lines
    )

# Separate text body and meta data
l <- 
  lex |>
  bind_rows() |> 
  select(text) |>
  mutate(id = cumsum(text == "End of Document")) |> 
  filter(!text %in% c("End of Document", "Link zum PDF-Dokument")) |> # Separate Documents
  group_by(id) |>
  mutate(tx_type = case_when( # Extract data section headers
    row_number() == 1 ~ "title", 
    row_number() == 4 ~ "meta",
    text == "Body" ~ "body", 
    text == "Graphic" ~ "graphic", 
    text == "Classification" ~ "meta"
  )) |> 
  filter("body" %in% tx_type) |> # remove article without body
  fill(tx_type) |>
  filter(!text %in% c("Body", "Graphic", "Classification")) |> # remove data sections lines
  filter(tx_type != "graphic") |> # Don't need graphic section
  slice(-4) %>% # Remove copyright line
  split(.$tx_type)

# Header with unnamed meta data
l$title <- l$title |> 
  mutate(var = c("title", "publication", "date")) |> 
  select(-tx_type) |> 
  pivot_wider(names_from = "var", values_from = "text") |> 
  ungroup() |> 
  mutate(
    date = date |> 
      str_remove("^[^\\d]+") |> # Remove leading weekday
      dmy(locale = "de_DE.utf8") # locale may need to be changed depending availability on OS
    )

# Named meta data
l$meta <- l$meta |>
  select(-tx_type) |> 
  mutate(flag = (!str_detect(text, "^[^:]+$")) |> cumsum()) |> 
  group_by(id, flag) |> 
  summarize(text = str_c(text, collapse = "; ")) |> # Multi-author bylines
  select(-flag) |> 
  separate(text, c("var", "val"), sep = ":", extra = "merge") |>
  mutate(val = str_squish(val)) |> 
  pivot_wider(names_from = "var", values_from = "val") |> 
  ungroup() |> 
  clean_names() |> 
  mutate(
    length = str_extract(length, "^\\d+") |> 
      as.integer(),
    load_date = mdy(load_date)
    )

# Text body
l$body <- 
  l$body |> 
  select(-tx_type) |> 
  filter(!text %in% c("Original Gesamtseiten-PDF")) |> 
  summarize(text = str_c(text, collapse = "\n"))


## Parse section variable --------------------------------------------------

# Structure of section information depending on the number of elements separated by semicolon
section_structure <- list(
  c("section", "page"), 
  c("section", "page", "issue"),
  c("section", "subsection", "page", "issue")
) |> 
  enframe("section_elements", "section_var_names")

lex1 <- l |> 
  reduce(inner_join, by = "id") |> 
  mutate(section_elements = str_count(section, ";")) |> # Section Structure varies with Semicolon count
  nest(data = -section_elements) |> 
  left_join(section_structure, by = "section_elements") |> 
  mutate(data = map2(data, section_var_names, # Separate depending on element count
                     \(dat, snm) separate(dat, section, snm, sep = ";\\s"))) |> 
  select(-section_var_names, section_elements) |> 
  unnest(data) |> 
  mutate(
    page_section_prefix = str_extract(page, "[A-Za-z]+(?=\\d+$)"), # Some page indices have leading section indicators
    page = page |> 
      str_extract("\\d+$") |> 
      as.integer(),
    section = section |> 
      str_remove("\\sFQT:(FRÜH|SPÄT)$") |> 
      str_to_title()
    ) |>
  relocate(page_section_prefix, .before = page) |> 
  select(-section_elements)|> 
  arrange(publication, date) 


# To Do: Subset Based on Sections -----------------------------------------

# To Do: For some of these papers, look at the physical issue to judge wheater to include section

lex1 |>
  count(publication, section, subsection, page_section_prefix, name = "subsec_n") |> 
  group_by(publication, section) |>
  mutate(sec_n = sum(subsec_n)) |> 
  arrange(publication, -sec_n, -subsec_n) |> 
  write_ods("data/lexis/sections.ods")

sections_manual_assessment <- "data/lexis/sections_manual_assessment.ods" |> 
  read_ods() |> 
  as_tibble() 

sections_manual_assessment |> 
  group_by(pol) |> 
  summarize(n_pol = sum(subsec_n))

sections_manual_assessment |> 
  group_by(pol) |> 
  summarize(n_pol = sum(subsec_n)) |> 
  mutate(p = n_pol/sum(n_pol))

sections_manual_assessment |> 
  group_by(publication, pol) |> 
  summarize(n_pol = sum(subsec_n)) |> 
  mutate(p = (n_pol/sum(n_pol)) |> round(2))


# To Do: Explore Text Body -------------------------------------------------------

lex1 |> 
  mutate(text = str_extract_all(text, "^[^\n]+(?=\n)|(?<=\n)[^\n]+$")) |> 
  unnest(text) |> 
  select(id, text, publication) |> 
  group_by(id) |>
  mutate(pos = c("first", "last")) |>
  ungroup() |> 
  count(publication, pos, text, sort = T) |> View()


lex1 |> 
  filter(str_detect(text, "Mittwoch, 21. Juli, 18.30 Uhr$")) |> View()


