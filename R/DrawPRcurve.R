Draw_PR_curve <- function(pred_tensor, label_tensor) {
  pr <- PR_curve(pred_tensor, label_tensor)
  precision <- as_array(pr$precision)
  recall <- as_array(pr$recall)
  df <- data.frame(
    recall = recall,
    precision = precision
  )
  ggplot(df, aes(x = recall, y = precision)) +
    geom_line(color = "blue", size = 1) +
    theme_minimal() +
    labs(
      title = "Precision-Recall Curve",
      x = "Recall",
      y = "Precision"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
}