"
2021.10.06
EFA with missing data

pip
"

source("scripts/efa_with_missing_data_fn.R")
df <- simulate_data(n=1000)
efa <- get_efa(df)
scores <- efa |> get_factor_scores(df)

df_mis <- df |> impose_missing()





"
goal is not estimate loadings, but get factor scores
  so need fa for each md pattern to be able to predict scores with appropriate loadings
  compare predict with all items efa and imputation (eg mice)
  --> how to estimate bias? what is the outcome?

one fa per md pattern
use all obs each time (borrow information)
cfa: to specify items load on which factor

do regression as in satis and bias in reg coefs and hyp tests


"