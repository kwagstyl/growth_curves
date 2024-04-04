 # multiple testing correction for linear regression estimates

# ------------------------------------------------------------------------------

# load dataset
growth_chart_path <- "/home/meldstudent/R/growth_curves-working_branch"
lin_reg <- read.csv(paste(growth_chart_path, "/datasets/ATP1A3_linear_rel_z.csv", sep = ""))

# order data by p value
lin_order <- order(lin_reg$P_Value)
lin_reg <- lin_reg[lin_order,]

#check data
head(lin_reg)

# carry out multiple testing correction by method
lin_reg$Bonferronni_P_Value <- p.adjust(lin_reg$P_Value, method = "bonferroni")
lin_reg$Holm_P_Value <- p.adjust(lin_reg$P_Value, method = "holm")
lin_reg$Hommel_P_Value <- p.adjust(lin_reg$P_Value, method = "hommel")
lin_reg$BH_P_Value <- p.adjust(lin_reg$P_Value, method = "BH")
lin_reg$BY_P_Value <- p.adjust(lin_reg$P_Value, method = "BY")
lin_reg$Hochberg_P_Value <- p.adjust(lin_reg$P_Value, method = "hochberg")
lin_reg$FDR_P_Value <- p.adjust(lin_reg$P_Value, method = "fdr")

# print all values
print(lin_reg)

#check significant structures
print(lin_reg$Segmentation[lin_reg$P_Value < 0.05])
print(lin_reg$Segmentation[lin_reg$Bonferronni_P_Value < 0.05])
print(lin_reg$Segmentation[lin_reg$Holm_P_Value < 0.05])

# plot change in p values - create matrices 
X = lin_reg$P_Value
Y = cbind(lin_reg$Bonferronni_P_Value,
          lin_reg$HB_P_Value,
          lin_reg$Holm_P_Value,
          lin_reg$Hochberg_P_Value,
          lin_reg$Hommel_P_Value,
          lin_reg$BY_P_Value,
          lin_reg$FDR_P_Value)

# Check for NULL or missing values in X and Y
if (any(is.null(X)) || any(is.null(Y)) || any(is.na(X)) || any(is.na(Y))) {
  stop("X and Y must contain valid data without NULL or missing values.")
}

# Check dimensions of X and Y
if (length(X) != nrow(Y)) {
  stop("The number of rows in Y must match the length of X.")
}

# make plot
matplot(X, Y,
        xlab="Raw p-value",
        ylab="Adjusted p-value",
        
        type="l",
        asp=1,
        col=1:6,
        lty=1,
        lwd=2)

# add title
title(main = "Multiple Testing Correction - ATP1A3 Linear Regression")

# add legend
legend('bottomright',
       legend = c("Bonferroni", "BH", "Holm", "Hochberg", "Hommel", "BY"),
       col = 1:6,
       cex = 1,   
       pch = 16)

# add diagonal
abline(0, 1,
       col=1,
       lty=2,
       lwd=1)

lin_reg_output <- lin_reg[lin_reg$Holm_P_Value < 0.05, ]
lin_reg_output <- subset(lin_reg_output, select = c("Segmentation", "Estimate", "P_Value", "Holm_P_Value"))

head(lin_reg_output)

write.csv(lin_reg_output, file = paste(growth_chart_path, "/datasets/holm_ATP1A3_linear_regression.csv", sep = ""))


