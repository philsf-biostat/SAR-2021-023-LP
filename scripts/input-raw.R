# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
library(readxl)
# library(haven)
# library(foreign)
# library(lubridate)
library(labelled)

# data loading ------------------------------------------------------------
set.seed(42)
data.raw <- tibble(id=gl(2, 10), group = gl(2, 10), outcome = rnorm(20))
data.raw <- read_excel("dataset/atq septuagenarios DESCONTADOS.xlsx") %>%
  janitor::clean_names()

Nvar_orig <- data.raw %>% ncol
Nobs_orig <- data.raw %>% nrow

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  # select() %>%
  mutate(
    # converter comorbidades para booleano
    has = !is.na(has),
    dm = !is.na(dm),
    tabagismo = !is.na(tabagismo),
    ex_tabagista = !is.na(ex_tabagista),
    lateralidade = toupper(lateralidade),
    cor = str_replace(cor, "PRETO", "PRETA"),
  ) %>%
  filter()

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    prontuario = factor(prontuario), # or as.character
  ) %>%
  mutate(
    # anemia - qualquer
    comp_anemia = str_detect(complicacoes, regex("anemia", ignore_case = TRUE)),
    # tep / embolia
    comp_tep = str_detect(complicacoes, regex("tep|embol", ignore_case = TRUE)),
    # tev
    comp_tev = str_detect(complicacoes, regex("tev", ignore_case = TRUE)),
    # tvp
    comp_tvp = str_detect(complicacoes, regex("tvp|trombo|venose", ignore_case = TRUE)),
    # instabilidade
    comp_inst = str_detect(complicacoes, regex("instab|luxa|soltura", ignore_case = TRUE)),
    # deiscência
    comp_deisc = str_detect(complicacoes, regex("deisc", ignore_case = TRUE)),
    # infeccção
    comp_infec = str_detect(complicacoes, regex("infec", ignore_case = TRUE)),
    # disturbios hidro eletroliticos
    comp_hidro = str_detect(complicacoes, regex("hidro|eletr[oó]l[íi]t", ignore_case = TRUE)),
    # delirium
    comp_delir = str_detect(complicacoes, regex("delir", ignore_case = TRUE)),
    # óbito
    comp_obito = str_detect(complicacoes, regex("[óo]bito|morte", ignore_case = TRUE)),
    # ITU
    comp_itu = str_detect(complicacoes, regex("itu", ignore_case = TRUE)),
    # sepse
    comp_sepse = str_detect(complicacoes, regex("s[ée]p[st]", ignore_case = TRUE)),
    # pneumonia
    comp_pneumo = str_detect(complicacoes, regex("pneumonia", ignore_case = TRUE)),
    # FX
    comp_fx = str_detect(complicacoes, regex("fx", ignore_case = TRUE)),
    # parestesia
    comp_parest = str_detect(complicacoes, regex("parest", ignore_case = TRUE)),

    # qualquer complicacao acima
    # comp_qualquer = any(comp_anemia, comp_tep, comp_inst, comp_deisc, comp_infec, comp_hidro, comp_embol, comp_delir),
    comp_qualquer =
      comp_anemia +
      comp_tep +
      comp_tev +
      comp_tvp +
      comp_inst +
      comp_deisc +
      comp_hidro +
      comp_delir +
      comp_obito +
      comp_itu +
      comp_sepse +
      comp_pneumo +
      comp_fx +
      comp_parest +
      comp_infec > 0,

    # grupo
    group = idade >= 70,
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    # group = "Study group",
    # outcome = "Study outcome",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw #%>%
  # select analytic variables
  # select(
    # prontuario,
    # group,
    # outcome,
  # )

Nvar_final <- analytical %>% ncol
Nobs_final <- analytical %>% nrow

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( prontuario = c( "1", "2", "3", "...", as.character(Nobs_final) ) ) %>%
  left_join(analytical %>% head(0), by = "prontuario") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")

# final data --------------------------------------------------------------

# salvar dataset (CSV)
# write_csv(analytical, "dataset/atq septuagenarios clean.csv")
