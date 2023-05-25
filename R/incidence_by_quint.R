

#' Calculate program incidence by income quintile
#'
#' @param df Household-level dataset containing a variable for program receipt, income and survey weights, dataframe
#' @param program_var Name of binary variable which indicates whether household received program, character
#' @param inc_var Name of income variable, character
#' @param weight_var Name of survey weight variable, character
#'
#' @return
#' @export
#'
#' @examples
incidence_by_quint <- function(df, program_var, inc_var, weight_var) {
  # Generate quintiles
  quint_cuts <- DescTools::Quantile(df[[inc_var]], weights = df[[weight_var]], probs = seq(0,1,.2))
  print(length(quint_cuts))
  # Assign observations to a quintile
  df <- df |> mutate(quints = cut(.data[[inc_var]], quint_cuts, labels = 1:5, include.lowest = TRUE))
  # Perform incidence analysis
  df |>
    group_by(quints) |>
    summarize(share_quant_receive = weighted.mean(.data[[program_var]], .data[[weight_var]]),
              share_in_quant = sum(.data[[weight_var]]*.data[[program_var]])) %>%
    mutate(share_in_quant = share_in_quant/sum(share_in_quant)) |>
    mutate(income_var = inc_var, program = program_var)
}
