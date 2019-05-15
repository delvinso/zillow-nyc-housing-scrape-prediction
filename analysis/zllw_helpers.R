# helpers ----
source("~/Documents/Projects/spotify/statSmoothFunc.R")
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 14))
# Enable caret to use MAE as eval metric
maeSummary <- function (train,
                        lev = NULL,
                        model = NULL) {
  require(Metrics)
  out <- mae(train$obs, train$pred)  
  names(out) <- "MAE"
  out
}

scttrCor <- function(...){
  ggplot(...) +
    geom_point( alpha = 0.5, size = 0.8) +
    geom_smooth(formula = y~x, method = "lm", se = FALSE, size = 1, alpha = 0.5, colour = "darkred") + 
    stat_smooth_func(geom = "text", method = "lm", xpos = -Inf, ypos = Inf, hjust = -0.2, vjust = 1, parse = TRUE, size = 3)+
    theme_minimal() + 
    theme(strip.text = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

corLine <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(size = 0.2, alpha = 0.5) + 
    stat_smooth_func(geom = "text", method = "lm", xpos = -Inf, ypos = Inf, hjust = -0.2, vjust = 1, parse = TRUE, size = 2) +
    geom_smooth(formula = y~x, method = "lm", se = FALSE, size = 0.3, alpha = 0.5, colour = "black")
}

ape <- function(obs, pred){
  # back transform from log
  
  obs <- exp(obs)
  pred <- exp(pred)
  
  abs_perc_error <- (abs((obs - pred)/obs)) * 100
  
  med_ape <- median(abs_perc_error)
  mean_ape <- mean(abs_perc_error)
  
  res <- tibble(median_ape = med_ape,
                mean_ape = mean_ape)
  
  return(res)
}
