library(nousutils)
library(DBI)
library(tidyverse)

dawn <- connect_dawn()
anzsic_4_abs <- dbGetQuery(dawn, "select * from common.d_anzsic_4_abs") %>%
  as_tibble() %>%
  mutate(anzsic_class = fct_reorder(anzsic_class, as.numeric(anzsic_class_code)),
         anzsic_group = fct_reorder(anzsic_group, as.numeric(anzsic_group_code)),
         subdivision = fct_reorder(subdivision, as.numeric(subdivision_code)),
         division = fct_reorder(division, as.numeric(factor(division_code, levels = LETTERS)))) %>%
  select_at(vars(-ends_with("_id")))


save(anzsic_4_abs, file = "pkg/data/anzsic_4_abs.rda")
