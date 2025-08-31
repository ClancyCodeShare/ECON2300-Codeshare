library(ggplot2)
library(latex2exp)
R2_visualiser = function(X = seq(0,1, length.out = 8),
                         Y = seq(0,1, length.out = 8) + 1.5*runif(8),
                         ESS = T,RSS = T,TSS = T,ESS_TSS = F){
  reg = lm(Y~X)
  mu_Y = mean(Y)
  Y_hat = predict(reg, newdata = data.frame(X = X))
  
  est_col = "#FE6100"
  est_line_col = "#DC267F"
  data_col = "#785EF0"
  data_line_col = "#648FFF"
  rss_line_col = "#FFB000"
  other_col = "black"
  
  
  Base = ggplot(data = data.frame(X,Y), aes(X,Y)) +
    geom_smooth(method = lm, color = other_col, se = F, size = 0.5) + 
    geom_point(color = data_col, size = 2) +
    geom_segment(x = min(X), xend = max(X),
                 y = mu_Y, yend = mu_Y, linetype = 2) +
    geom_point(x = X, y = Y_hat, color = est_col, shape = 17, size = 2) 
  
  if(ESS == T){
  ESS_Lines = geom_segment(x = X, xend = X,
                 y =rep(mu_Y, length(X)),
                 yend = Y_hat,
                 col = est_line_col)  
  ESS_Plot = Base + ESS_Lines +
    labs(title = TeX("$\\bar{y}-\\hat{y_i}$ for ESS")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5))
  print(ESS_Plot)}
  
  if(TSS==T){
  TSS_Lines = geom_segment(x = X, xend = X,
                           y = rep(mu_Y, length(X)),
                           yend = Y,
                           col = data_line_col)
  TSS_Plot = Base + TSS_Lines +
    labs(title = TeX("$y_i - \\bar{y}$ for TSS"))+
    theme(plot.title = element_text(hjust = 0.5))
    
  print(TSS_Plot)}
  
  if(RSS==T){
    RSS_Lines = geom_segment(x= X, xend = X,
                             y = Y_hat, yend = Y,
                             col = rss_line_col)
  RSS_Plot = Base + RSS_Lines +
    labs(title = TeX("$y_i - \\hat{y_i}$ for RSS")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(RSS_Plot)}
  
  if(ESS_TSS==T){
  ESS_TSS_Plot = Base + ESS_Lines +
    geom_segment(x = X, xend = X,
                 y = rep(mu_Y, length(X)),
                 yend = Y,
                 col = data_line_col,
                 linetype = "dashed") +
    labs(title = TeX("$\\bar{y}-\\hat{y_i}$ and $y_i - \\bar{y}$")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(ESS_TSS_Plot)}
}


