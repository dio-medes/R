xx <- maf(df_pca)



plot(x = seq_along(xx$mafs[, 1]), y = xx$mafs[, 1], type = "l")


df_graph <- merge(df_moore, df |> select(
  date,
  uncertain_grammatical_variants_count,
  sixty_two_word_dictionary_count,
  single_word_uncertain_count,
  single_word_risk_count,
  net_balance_count),
  all = TRUE, by = "date")




graph_data <- cbind(xx$mafs[, 1], df_moore$moore_normalised)
matplot(graph_data, type = "b")
