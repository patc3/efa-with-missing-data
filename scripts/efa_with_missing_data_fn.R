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
get_factor_scores <- function(efa, df, impute=FALSE)
{
  # impute?
  df_orig <- df
  if(impute) df <- df |> impute_items()
  
  #scores <- efa$scores # matrix
  scores <- predict(efa, df, old.data=df_orig) |> as.data.frame() # predict.psych
  v_factors <- paste0("F", 1:ncol(scores))
  colnames(scores) <- v_factors
  
  # out
  return(scores)
}


#### missing ####
impose_missing <- function(df, prop_missing=.25)
{
  # vars with missing: 2nd item of each factor
  v_missing <- seq(2,12,3) #sample(1:ncol(df), size = 4, replace = FALSE)
  v_missing <- v_missing |> sample(nrow(df), replace = TRUE)
  
  # row missing
  i_missing <- (1:nrow(df)) |> sample(prop_missing*nrow(df) |> round())
  
  # impose
  df_mis <- df
  for(i in i_missing) df_mis[i, v_missing[i]] <- NA
  
  # out
  return(df_mis)
}


impute_items <- function(df, fn=stats::median)
{
  # impute
  for(col in colnames(df))
  {
    df[df[,col] |> is.na(),col] <- fn(df[,col], na.rm=TRUE)
  }

  # out
  print("Imputed missing item values with median")
  return(df)
}


