## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(dplyr)
library(ggplot2)
library(recipes)
library(scimo)

theme_set(theme_light())

data("pedcan_expression")

## -----------------------------------------------------------------------------
pedcan_expression

## -----------------------------------------------------------------------------
count(pedcan_expression, disease, sort = TRUE)

## -----------------------------------------------------------------------------
rec_naive_pca <-
  recipe(pedcan_expression) %>% 
  update_role(-cell_line) %>% 
  step_zv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors()) %>% 
  prep()

rec_naive_pca %>% 
  bake(new_data = NULL) %>% 
  ggplot() +
  aes(x = PC1, y = PC2, color = disease) +
  geom_point()

## -----------------------------------------------------------------------------
rec_cv_pca <-
  recipe(pedcan_expression) %>% 
  update_role(-cell_line) %>% 
  step_select_cv(all_numeric_predictors(), prop_kept = 1/4) %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors()) %>%
  prep()

rec_cv_pca %>% 
  bake(new_data = NULL) %>% 
  ggplot() +
  aes(x = PC1, y = PC2, color = disease) +
  geom_point()

## -----------------------------------------------------------------------------
tidy(rec_cv_pca, 1)

