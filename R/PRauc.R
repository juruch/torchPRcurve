PR_AUC <- function(pred_tensor, label_tensor){
  pr = PR_curve(pred_tensor, label_tensor)
  N = length(pr$recall)  
  recall_diff = pr$recall[2:N]-pr$recall[1:-2]
  precision_sum = pr$precision[2:N]+pr$precision[1:-2]
  torch::torch_sum(recall_diff*precision_sum/2.0)
}
