plot.glm <- function(n,b0,b1,seed=NA){
  
  if(1==2){
    n <- 50
    b0 <- -1 # Intercept
    b1 <- 0.05 # Slope
  }
  
  if(!is.na(seed)) set.seed(seed)
  
  X_f <- runif(n,   # Number of samples
               0,   # Lower limit of the uniform distribution
               100) # Upper limit of the uniform distribution
  
  link_f <- plogis(b0 + b1 * X_f)
  
  y_f <- rbinom(n,    # Number of samples
                1,    # Trial size (one because we have Bernoulli
                # trials or coin flips)
                link_f) # Per trial probabilities
  
  glm_mod_f <- glm(y_f ~ X_f, family = "binomial")
  df_f <- data.frame(X_f = seq(0,100,1))
  pred_f <- predict(glm_mod_f, newdata=df_f, type="link",
                           se.fit = TRUE)
  df_f$y <- plogis(pred_f$fit)
  df_f$lci <- plogis(pred_f$fit - 1.96*pred_f$se.fit)
  df_f$uci <- plogis(pred_f$fit + 1.96*pred_f$se.fit)
  
  ggplot(df_f, aes(x = X_f, y = y)) +
    geom_hline(yintercept = c(0,1),colour="darkgrey")+
    geom_ribbon(aes(ymin = lci, ymax = uci),
                  fill = "black", alpha = 0.2) +
    geom_line(linewidth = 1, colour="red") + 
    labs(x = "Covariate (X)", y = "Probability and 95% CI")+
    ggtitle(paste0("Sample size = ",n,", b0 = ",b0,", b1 = ",b1))+
    ylim(c(0,1))+
    xlim(c(0,100))+
    theme(plot.title.position = 'plot', 
          plot.title = element_text(hjust = 0.5))
  
}