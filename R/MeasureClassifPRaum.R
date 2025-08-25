## from https://github.com/OGuenoun/R-AUM_Multiclass/blob/main/AUM_comparison.r
## and  https://github.com/tdhock/imbalanced-paper/blob/main/2025-07-30-conv-linear-prop0.01.R

MeasureClassifPRAUM = R6::R6Class(
  "MeasureClassifPRAUM",
  inherit = mlr3::MeasureClassif,
  public = list(
    AUM=Proposed_AUM,
    initialize = function() { 
      super$initialize(
        id = "MeasureClassifPRAUM",
        packages = "torch",
        properties = character(),
        task_properties = "twoclass",
        predict_type = "prob",
        range = c(0, Inf),
        minimize = TRUE
      )
    }
  ),
  private = list(
    # define score as private method
    .score = function(prediction, ...) {
      pred_tensor <- torch::torch_tensor(prediction$prob[,1])
      label_tensor <- torch::torch_tensor(prediction$truth)
      loss_tensor <- self$AUM(pred_tensor, label_tensor)
      torch::as_array(loss_tensor)
    }
  )
)
