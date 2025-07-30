Proposed_AUM <- function(pred_tensor, label_tensor){
  pr = PR_curve(pred_tensor, label_tensor)
  min_prec_rec = pr[["min(prec,rec)"]][2:-2]
  constant_diff = pr$min_constant[2:N]$diff()
  torch::torch_sum(min_prec_rec * constant_diff)
}