visualiser = function(est, se, mu = 0, conf = 0.95, length.out = 500){
  #Values for plot
  alpha = (1-conf)/2 
  margin_size = qnorm(1-alpha, est, se) - qnorm((1-alpha)/2, est, se)
  ub = max(mu, qnorm(1-alpha, est, se)) + margin_size
  lb = min(mu, qnorm(alpha, est, se)) - margin_size
  X = seq(lb, ub, length.out = length.out)
  Y = dnorm(X, est, se)
  plot(X,Y, type = "l", xaxt = "n", main = "Estimated Distribution")
  grid()
  
  #Confidence interval lines
  conf_colour = "#648FFF"
  conf_pch = NA
  conf_lty = 1
  abline(v = qnorm(1-alpha, est, se),  col = conf_colour, lty = conf_lty)
  abline(v = qnorm(alpha, est, se),  col = conf_colour, lty = conf_lty)
  
  #Estimated mean line
  est_colour = "#785EF0"
  est_pch = 16
  est_lty = 3
  segments(est,0,est,dnorm(est,est,se), col = est_colour, lty = est_lty)
  points(est, dnorm(est, est, se), pch = est_pch, col = est_colour)
  
  #Annotating H0 value
  H0_colour = "#FE6100"
  H0_pch = 16
  H0_lty = 3
  segments(mu, 0, mu, dnorm(mu,est,se), col = H0_colour, lty = H0_lty)
  points(mu, dnorm(mu, est, se), pch = H0_pch, col = H0_colour)
  
  #Set xticks at sample mean, H0 mean and CI bounds
  axis(side = 1, at = c(est, mu, qnorm(1-alpha,est,se), qnorm(alpha, est, se)),
       labels = round(c(est, mu, qnorm(1-alpha,est,se), qnorm(alpha, est, se)),
                      digits = 2))
  
  #Position the legend
  position = if (mu> qnorm(1-alpha, est, se)) {"topright"} else {"topleft"}
  legend(x = position, legend = c("Estimate Mean",
                                  "H0 Hypothesised Mean",
                                  "Confidence Interval Bounds"),
         col = c(est_colour, H0_colour, conf_colour),
         pch = c(est_pch,H0_pch, conf_pch),
         lty = c(est_lty, H0_lty,conf_lty))
}
