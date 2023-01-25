library(tidyverse)
library(haven)
library(lubridate)

list.files("data/", full.names = T) 
# politbarometer
pb <- read_dta("data/politbarometer/ZA7856_v1-0-0.dta")


# GLES Panel --------------------------------------------------------------

# Import gles panel data
gp <- 
  list.files(
    path = "data/gles-panel", 
    pattern = "^\\d{2}-", 
    full.names = T
  ) |>
  map(read_dta)

# Field periods by wave
gp_field <- 
  gp |>
    set_names(16:20) |> 
    map(
      \(df) df |> 
        select(starts_with("field")) |> 
        distinct()
    ) |> 
    bind_rows(.id = "wave")|> 
  mutate_at(-1, as_date)
    
# Join frames for all waves by participant id
gp2 <- gp |> 
  map(
    \(df) df |> 
      select(-(study:sample), -matches("^lfdn\\d+"))
  ) |> 
  reduce(full_join)



## Repair Wave 17 ----------------------------------------------------------

# Variables with conflicting types across waves. Type conflict vars have no labels at all
gp_type_conflict <- map_chr(gp2[-1], class) |> 
  enframe() |> 
  separate(name, c("wave", "var"), extra = "merge", sep = "_") |>
  group_by(var) |> 
  distinct(value) |> 
  count(var) |> 
  filter(n > 1) |> 
  pull(var)

# All 4045 vars have the same labels. Same goes for smartphone and tablet
gp[-2] |>
  map(select, contains("4045")) |> # insert smartphone or tablet here to check for those
  flatten() |>
  map(attr, "labels") |>
  unique()

# Use wave 16 labels for repair
gp3 <- gp2 |> 
  mutate(
    across(contains("kp17_4045"),
           \(vec) vec %>%
             labelled(
               labels = attr(gp2$kp16_4045a, "labels"),
               label = attr(., "label")
               ) |> 
             (`attr<-`)("format.stata", "%31.0g")),
    kp17_smartphone = kp17_smartphone %>% 
      labelled(labels = attr(gp2$kp16_smartphone, "labels"),
               label = attr(., "label")) |> 
      (`attr<-`)("format.stata", "%31.0g"),
    kp17_tablet = kp17_tablet %>%
      labelled(labels = attr(gp2$kp16_tablet, "labels"),
               label = attr(., "label")) |> 
      (`attr<-`)("format.stata", "%31.0g"),
    )

# Repair personal popularity scalometer
gp3 <- gp3 |> 
  mutate(
    across(
      contains("kp17_650"),
      \(lbl_vec) lbl_vec |> 
        (`attr<-`)("labels", attr(gp3$kp16_650a, "labels")) # Replace person scalometer labels in w17 with labels from w16
    )
  )


# Reshape GLES ------------------------------------------------------------

gp_rs_warn <- gp3 |> 
  #mutate(across(contains(gp_type_conflict), as.character)) |> 
  quietly(pivot_longer)(
    col = starts_with("kp"),
    names_pattern = "kp(\\d+)_(.*)",
    names_to = c("wave", ".value"),
    names_repair = \(nm) str_c("v", nm)
    )

gp_rs_warn2 <- gp_rs_warn$warnings |>
  enframe("i", "msg") |> 
  mutate(vars = str_extract_all(msg, "`[^\\s]+`") |>
           do.call(what=rbind) |>
           as_tibble(.name_repair = \(x) c("v1", "v2", "vlbl")),
         values = str_extract(msg, "(?<=Values: ).+$") |> 
           str_extract_all("\\d+") |> 
           map(as.integer)
         ) |> 
  unnest(vars) |> 
  mutate(across(v1:v2, str_remove_all, "`")) |> 
  select(v1, v2, values)

gp_rs_warn2 |> 
  mutate(
    across(v1:v2, 
           \(v) map(v, \(w) gp3 |>
                      pull(w) |> 
                      attr("labels") |> 
                      enframe(w, "val")
               ) |> 
             set_names(v)
         )
    ) |> 
  mutate(x = map2(v1,v2, 
                  \(e1,e2) full_join(e1, e2, by = "val") |>
                    filter(val >= 0)
                    #filter(nm.x != nm.y | is.na(nm.x) | is.na(nm.y))
                  )
         ) |> 
  pull(x)

gp_vars_lbl_mismatch <- gp_rs_warn2 |> 
  select(v1:v2) |> 
  unlist() |> 
  unname() |> 
  str_remove("^kp\\d+_") |> 
  unique() |> 
  sort()

gp_vars_lbl_mismatch |> 
  enframe("i", "var") |> 
  mutate(lab = map(var, 
                   \(v) gp3 |> 
                     select(contains(v)) |> 
                     map_chr(attr, "label") |> 
                     unique()))

#' Different labellings schemes across waves
#' @param var_lab common part of variable names
#' @return a frame showing the distribution of different labelling schemes across waves
label_variation <- \(var_lab) gp3 |> 
  select(contains(var_lab)) |> 
  map(attr, "labels") |> 
  enframe() |> 
  mutate(wave = str_extract(name, "(?<=kp)\\d{2}"),
         pers = str_extract(name, "[a-z]\\d*$"),
         value = value |> as.character() %>% factor(label = seq_along(unique(.)))) |> 
  count(wave, value)


label_variation("650")
label_variation("221")


gp3 |> 
  select(contains("650")) |> 
  map(attr, "labels") |> 
  unique() |> 
  map(\(nmd_vec) nmd_vec |> 
        enframe() |> 
        filter(value >= 0)) 




gp_rs_warn$result |> 
  select(vwave, contains("650")) |> 
  pivot_longer(-vwave, names_to = "politician", values_to = "popularity") |> 
  group_by(politician,vwave) |> 
  filter(popularity >0) |> 
  summarize(popularity = mean(popularity, na.rm = T)) |> 
  ungroup() |> 
  mutate(vwave = as.integer(vwave)) |> 
  #filter(politician == "v650a") #|> 
  ggplot(aes(vwave, 
             popularity, 
             color = politician
             )) +
  geom_line()

# -------------------------------------------------------------------------


gp_resp_time <- gp3 |> 
  select(lfdn, wave, vdatetime) |> 
  left_join(gp_field[1:2]) |> 
  mutate(vdatetime = as_date(vdatetime), 
         response_time = vdatetime - field_start) 


gp_resp_time |> 
  ggplot(aes(response_time)) + 
  geom_bar()

gp_resp_time |> 
  count(response_time) |> 
  slice(-n()) |> 
  mutate(p = (n/sum(n)) |> round(2))



