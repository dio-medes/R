# Intro -------------------------------------------------------------------

# This file does PCA on the aggregate WARIA uncertainty indices.


# Packages ----------------------------------------------------------------
# -------------------------------------------------------------------------
library(here)
library(openxlsx)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


# Data --------------------------------------------------------------------
# -------------------------------------------------------------------------
df <- read.xlsx(here("i-basic-aggregate-indices.xlsx"),
                detectDates = TRUE) |>
  select(
    date,
    uncertain_grammatical_variants_count,
    sixty_two_word_dictionary_count,
    single_word_uncertain_count,
    single_word_risk_count,
    net_balance_count)

df_pca <- df |>
  select(!date)

df_moore <- read.xlsx(here("g-aggregate-moore-comparison.xlsx"),
                      detectDates = TRUE) |>
  select(date, moore_normalised)


# PCA ---------------------------------------------------------------------
# -------------------------------------------------------------------------

# PCA 
pca <- prcomp(df_pca, scale = TRUE, center = TRUE, retx = FALSE)

pca_timeseries <- data.frame(as.matrix(df_pca) %*% as.matrix(pca$rotation))

xx <- pca_timeseries |>
  mutate(index = 1:nrow(pca_timeseries)) |>
  pivot_longer(-index)

ggplot(data = xx) +
  geom_line(aes(x = index, y = value, colour = name))
  

# New series
fcs <-  as.matrix(df_pca) %*% as.matrix(pca$rotation[, 1])

df$fcs <- as.double(fcs)

# Graph

df_graph <- merge(df_moore, df |>   select(
  date,
  uncertain_grammatical_variants_count,
  sixty_two_word_dictionary_count,
  single_word_uncertain_count,
  single_word_risk_count,
  net_balance_count,
  fcs),
  all = TRUE, by = "date") |>
  pivot_longer(cols = -date, names_to = "series", values_to = "value")


ggplot(data = df_graph) +
  geom_line(aes(x = date, y = value, colour = series))

df_graph <- merge(df_moore, df |>   select(
  date,
  uncertain_grammatical_variants_count,
  sixty_two_word_dictionary_count,
  single_word_uncertain_count,
  single_word_risk_count,
  net_balance_count,
  fcs),
  all = TRUE, by = "date")

ggplot(data = df_graph) +
  geom_line(aes(x = date, y = moore_normalised, colour = 'red')) +
  geom_line(aes(x = date, y = fcs, colour = 'blue'))

# PCA without net balance -------------------------------------------------
# -------------------------------------------------------------------------
df <- read.xlsx(here("i-basic-aggregate-indices.xlsx"),
                detectDates = TRUE) |>
  select(
    date,
    uncertain_grammatical_variants_count,
    sixty_two_word_dictionary_count,
    single_word_uncertain_count,
    single_word_risk_count,
    net_balance_count)

df_pca <- df |>
  select(-c(date, net_balance_count))


# PCA 
pca <- prcomp(df_pca, scale = TRUE, center = TRUE, retx = TRUE)

# New series
fcs <-  as.matrix(df_pca) %*% as.matrix(pca$rotation[, 1])

df$fcs <- as.double(fcs)

# Graph

df_graph <- merge(df_moore, df |> select(
  date,
  uncertain_grammatical_variants_count,
  sixty_two_word_dictionary_count,
  single_word_uncertain_count,
  single_word_risk_count,
  net_balance_count,
  fcs),
  all = TRUE, by = "date") |>
  pivot_longer(cols = -date, names_to = "series", values_to = "value")


ggplot(data = df_graph) +
  geom_line(aes(x = date, y = value, colour = series))

df_graph <- merge(df_moore, df |>   select(
  date,
  uncertain_grammatical_variants_count,
  sixty_two_word_dictionary_count,
  single_word_uncertain_count,
  single_word_risk_count,
  net_balance_count,
  fcs),
  all = TRUE, by = "date")

ggplot(data = df_graph) +
  geom_line(aes(x = date, y = moore_normalised), colour = 'red') +
  geom_line(aes(x = date, y = fcs), colour = 'blue') +
  geom_line(aes(x = date, y = uncertain_grammatical_variants_count), colour = 'black')


df_graph |>
  filter(!is.na(moore_normalised)) |> 
  summarise(across(c(fcs, uncertain_grammatical_variants_count),
                   ~ cor(.x, moore_normalised)))



# Regression --------------------------------------------------------------
# -------------------------------------------------------------------------
df <- read.xlsx(here("i-basic-aggregate-indices.xlsx"),
                detectDates = TRUE) |>
  select(
    date,
    uncertain_grammatical_variants_count,
    sixty_two_word_dictionary_count,
    single_word_uncertain_count,
    single_word_risk_count,
    net_balance_count)

df_ols <- merge(df_moore, df |>   select(
  date,
  uncertain_grammatical_variants_count,
  fcs),
  all = TRUE, by = "date") |>
  select(ria = uncertain_grammatical_variants_count,
         er = moore_normalised,
         pca = fcs)


moore_ols <- lm(er ~ ria + lag(ria), data = df_ols)

pca_ols <- lm(er ~ pca, data = df_ols)








