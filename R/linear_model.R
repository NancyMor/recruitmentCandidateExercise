#' @title Mixed effects with
#' @description Transforms factor variables into character or numeric variables.
#' @param  database The base to transform
#' @return A data.table object
#' @import data.table lme4 magrittr
#' @export
mixed_linear_model <- function(database) {
  #Getting the linear model taking the inference of Media Campaing effects
  ml_model <- lmer(`Search Volume` ~ 1 + (1 + `Media Spend (USD)`|`Media Campaign`) , database)
  coefficients <- coef(ml_model)$`Media Campaign` %>% as.data.table()

  #Getting coeficients to dsiplay in table
  coefs_table <- data.table("Media Campaign" = as.numeric(rownames(coefficients)),
                            "Beta" = coefficients[[1]],
                            "Intercept" = coefficients[[2]])

  #Creating total database with results
  data_results <- merge.data.frame(database, coefs_table)
  data_results$`Estimated Search Volume` <- fitted(ml_model)

  #Getting list with data and model
  model_list <- list("model" = ml_model,
                     "data_results" = data_results)

  return(model_list)

}
