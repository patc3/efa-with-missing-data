"
2021.10.06
EFA with missing data

fn
"

#### imports ####
library(tidyverse)
library(psych)
library(MASS)


#### simulate data ####
simulate_data <- function(n)
{
  r <- psych::sim()$model
  df <- mvrnorm(n=n, mu=rep(0, ncol(r)), Sigma=r) |> as.data.frame()
  colnames(df) <- colnames(r)
  return(df)
}



#### efa ####
get_efa <- function(df)
{
  efa <- fa(
    r = df,
    nfactors = 4,
    rotate = "oblimin",
    scores = "tenBerge",
    fm = "minres",
    oblique.scores = TRUE,
    use = "pairwise",
    cor = "cor",
    missing = FALSE, impute = "median"
  )
  return(efa)
}


# add scores
get_factor_scores <- function(df, efa)
{
  #scores <- efa$scores # matrix
  scores <- predict(efa, df) |> as.data.frame() # predict.psych
  v_factors <- paste0("F", 1:ncol(scores))
  colnames(scores) <- v_factors
  
  # out
  return(scores)
}



