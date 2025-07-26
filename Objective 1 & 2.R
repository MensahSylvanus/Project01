rm(list=ls(all=T))
graphics.off()
## > 0. Packages and working directory
libs <- c('data.table','tibble', 'dplyr',"tidyr", 'readr','stringr','imputeTS','dplR','tidyverse',"lubridate","hrbrthemes",
          'car','nlme','lme4','ggplot2',"sjPlot", 'ggpubr', 'ggeffects',"gridExtra", "lmerTest","grid","cowplot")
invisible(lapply(libs, library, character.only = T))


setwd("C:/Users/Mensah/Documents/Mensah 2023-Research/fire plot 1/Analaysis") #set wd

data <- read.csv("fireplotdata.csv")
head(data)
str(data)
names(data)

### Obj. 1& 2. Effects of fire frequency on tree diversity and stand characteristics
### we performed mixed-effect models and bivariate analyses (with help of the 'plot_model' function of the sjPlot package). 
### Separate mixed-effect models were fitted with diversity metrics such as species richness, 
### FDis, sesMPD and sesMNTD, cvDBH and skwDBH. These diversity metrics were treated as individual response variable
### Fire frequency was the fixed predictor, whereas plot size and country were treated as random predictors

set_theme(theme_sjplot2(base_size = 12),axis.title.color = "black",
          axis.tickslen = 0, # hides tick marks
          axis.title.size = 0.9,
          axis.textsize = 0.9,
          legend.size = 1.1,
          legend.title.size = 1.1,plot.margin=unit(c(0,0,0.1,0), "cm"),
          geom.label.size = 3.5)


# List of variable names
responses <- c("Species_richness","FDis", "sesMPD", "sesMNTD", "cvDBH", "skwDBH", 
                "Tree_density", "LargeTrees_density", "RemTrees_density", "HLorey")### lmer applied here
predictor <- "Fire_frequency"

models <- list()
plots <- list()

for (resp in responses) {
  
  formula <- as.formula(paste0(resp, " ~ ", predictor, " +  (1|Plot_size)"))#  formula
  if (resp == "Species_richness") {
    model <- glmer.nb(formula, data = data)
  } else {
    model <- lmer(formula, data = data)
  }
  
  models[[resp]] <- model# Save model 
  
  plot <- plot_model(model, type = "pred", show.data = F, dot.size = 1.5,
                     mdrt.values = "meansd", terms = predictor,
                     axis.title = c("Fire frequency", resp),title="", 
                     legend.title = "",
                     line.size = 1, colors = "gs")+ggtitle(NULL)  # Remove title
  
#  if (TRUE) {  # or FALSE, depending on your choice
#    if (length(plot$layers) >= 3) {
#      plot$layers[[1]]$aes_params$colour <- "#00AFBB"  # points
#      plot$layers[[2]]$aes_params$fill <- "#FC4E07"    # ribbon
#      plot$layers[[3]]$aes_params$colour <- "#FC4E07"  # line
 #   }
#  }
  
  
  # Fix x-axis label
  x_label <- if (resp %in% tail(responses, 4)) "Fire frequency (2001-2021)" else ""
  plot <- plot + labs(x = x_label)
  
  # Fix y-axis range using coord_cartesian 
  y_range <- range(data[[resp]], na.rm = TRUE)
  plot <- plot + coord_cartesian(ylim = y_range)
  
  
  plots[[resp]] <- plot # Save plot
    
}

for (resp in names(models)) {
  cat("===== Summary for", resp, "=====\n")
  print(summary(models[[resp]]))
  cat("\n\n")
}

#### Plotting
plots[["FDis"]]# view


# Extract p-values for the Fire_frequency fixed effect
pvalues <- sapply(names(models), function(resp) {
  mod <- models[[resp]]
  
  if (inherits(mod, "lmerModLmerTest")) {
    # Use anova to get p-value for fixed effect in lmerTest
    anova(mod)["Fire_frequency", "Pr(>F)"]
    
  } else if (inherits(mod, "glmerMod")) {
    # For glmer.nb, use summary + coef table
    coef(summary(mod))["Fire_frequency", "Pr(>|z|)"]
    
  } else {
    NA
  }
})
pvalues_text <- paste0("p = ", formatC(pvalues, digits = 3, format = "f"))# Format nicely (rounded)
names(pvalues_text) <- names(pvalues)# Assign names to look them up correctly


blank_plot <- ggplot() + theme_void()
plot_list <- list(
  plots[["Species_richness"]],
  plots[["FDis"]],
  plots[["sesMPD"]],
  plots[["sesMNTD"]],
  plots[["cvDBH"]],
  plots[["skwDBH"]],
  blank_plot,
  blank_plot,
  plots[["Tree_density"]],
  plots[["LargeTrees_density"]],
  plots[["RemTrees_density"]],
  plots[["HLorey"]]
)

labels <- letters[c(1:6, NA, NA, 7:10)]

final_plot <- cowplot::plot_grid(plotlist = plot_list,
                                 ncol = 4,
                                 labels = labels,
                                 align = "hv",
                                 label_y = 0.95,
                                 label_x = 0.02,
                                 label_size = 10)
print(final_plot)

# Coordinates for p values
pval_positions <- list(
  Species_richness   = c(x=0.15, y=0.96),
  FDis               = c(x=0.40, y=0.96),
  sesMPD             = c(x=0.65, y=0.96),
  sesMNTD            = c(x=0.90, y=0.96),
  cvDBH              = c(x=0.15, y=0.60),
  skwDBH             = c(x=0.40, y=0.60),
  # empty plots at positions 7 & 8 skipped
  Tree_density       = c(x=0.15, y=0.26),
  LargeTrees_density = c(x=0.40, y=0.26),
  RemTrees_density   = c(x=0.65, y=0.26),
  HLorey             = c(x=0.90, y=0.26)
)
print(final_plot)

# Now add p-values
for(resp in names(pval_positions)) {
  pos <- pval_positions[[resp]]
  grid.text(pvalues_text[resp], x = pos["x"], y = pos["y"],
            gp = gpar(cex = 0.8, font = 4, col = "darkblue"))# Fig 1
}



#### Store models results(slope)

results <- data.frame()

for (resp in names(models)) {
  mod <- models[[resp]]
  
  if (inherits(mod, "lmerModLmerTest")) {
    # Linear mixed model
    coef_table <- anova(mod)
    p_value <- coef_table["Fire_frequency", "Pr(>F)"]
    est <- fixef(mod)["Fire_frequency"]
    se <- summary(mod)$coefficients["Fire_frequency", "Std. Error"]
  } else if (inherits(mod, "glmerMod")) {
    # Generalized linear mixed model (glmer.nb)
    coef_summary <- summary(mod)$coefficients
    est <- coef_summary["Fire_frequency", "Estimate"]
    se <- coef_summary["Fire_frequency", "Std. Error"]
    p_value <- coef_summary["Fire_frequency", "Pr(>|z|)"]
  } else {
    next
  }
  
  results <- rbind(results, data.frame(
    Response = resp,
    Estimate = est,
    Std_Error = se,
    P_value = p_value
  ))
}


results$P_value <- round(results$P_value, 4)
results$Significance <- cut(results$P_value,
                            breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                            labels = c("***", "**", "*", ".", ""))
write.csv(results, "fire_frequency_model_results2.csv", row.names = FALSE)





#### Store models results(slope and intercept)
results <- data.frame()

for (resp in names(models)) {
  mod <- models[[resp]]
  
  if (inherits(mod, "lmerModLmerTest")) {
    coef_summary <- summary(mod)$coefficients
    
    # Extract fixed effects
    intercept_est <- coef_summary["(Intercept)", "Estimate"]
    fire_est <- coef_summary["Fire_frequency", "Estimate"]
    
    intercept_se <- coef_summary["(Intercept)", "Std. Error"]
    fire_se <- coef_summary["Fire_frequency", "Std. Error"]
    
    intercept_t <- coef_summary["(Intercept)", "t value"]
    fire_t <- coef_summary["Fire_frequency", "t value"]
    
    intercept_p <- coef_summary["(Intercept)", "Pr(>|t|)"]
    fire_p <- coef_summary["Fire_frequency", "Pr(>|t|)"]
    
    stat_label <- "t"
    
  } else if (inherits(mod, "glmerMod")) {
    coef_summary <- summary(mod)$coefficients
    
    intercept_est <- coef_summary["(Intercept)", "Estimate"]
    fire_est <- coef_summary["Fire_frequency", "Estimate"]
    
    intercept_se <- coef_summary["(Intercept)", "Std. Error"]
    fire_se <- coef_summary["Fire_frequency", "Std. Error"]
    
    intercept_t <- coef_summary["(Intercept)", "z value"]
    fire_t <- coef_summary["Fire_frequency", "z value"]
    
    intercept_p <- coef_summary["(Intercept)", "Pr(>|z|)"]
    fire_p <- coef_summary["Fire_frequency", "Pr(>|z|)"]
    
    stat_label <- "z"
    
  } else {
    next
  }
  
  results <- rbind(results, data.frame(
    Response = rep(resp, 2),
    Term = c("(Intercept)", "Fire_frequency"),
    Estimate = c(intercept_est, fire_est),
    Std_Error = c(intercept_se, fire_se),
    Statistic = c(intercept_t, fire_t),
    Stat_Type = stat_label,
    P_value = c(intercept_p, fire_p)
  ))
}

# Add significance stars
results$P_value <- round(results$P_value, 4)
results$Significance <- cut(
  results$P_value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)

# Save to CSV
write.csv(results, "fire_frequency_model_results_with_stats.csv", row.names = FALSE)

