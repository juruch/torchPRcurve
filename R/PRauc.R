PR_AUC <- function(pred_tensor, label_tensor){
  pr = PR_curve(pred_tensor, label_tensor)
  recall_diff = pr$recall[3:N]-pr$recall[2:-2]
  precision_sum = pr$precision[3:N]+pr$precision[2:-2]
  torch::torch_sum(recall_diff*precision_sum/2.0)
}
PR_AUC(four_pred, four_labels)