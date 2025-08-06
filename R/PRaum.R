Proposed_AUM <- function(pred_tensor, label_tensor){
  pr = PR_curve(pred_tensor, label_tensor)
  N = length(pr$recall)  
  min_FDR_FNR = pr[["min(FDR,FNR)"]][2:-2]
  constant_diff = pr$min_constant[2:N]$diff()
  torch::torch_sum(min_FDR_FNR * constant_diff)
}