# Load required libraries for survival analysis and plotting
library(survival)  # For survival analysis
library(MASS)  # For functions like survreg() and more
library(foreign)  # For reading SPSS files
library(ggfortify)  # For automatic ggplot2 integration with survival objects
library(ggplot2)  # For plotting
library(parfm)  # For fitting parametric frailty models
library(gridExtra)  # For arranging multiple plots in a grid

# Install necessary packages (only need to run once)
install.packages("gridExtra")  # Package for arranging plots in grids

# Read the FMG data from SPSS file
FMG <- read.spss('C:\\Users\\user\\Desktop\\FMG data\\FMG0.sav')  # Load data
attach(FMG)  # Attach the data frame for easy reference of columns
View(FMG)  # View the data in a table format

# Fit a parametric frailty model with the log-logistic distribution
mymodel <- parfm(Surv(Age, Status)~Agem + Residence + Wealth + Religion + Education + Employment + Peducation + Media,
                 cluster="Region", data=FMG, frailty="gamma", dist="loglogistic")
summary(mymodel)  # Summary of the frailty model

# Fit survival regression models using different distributions for comparison
model1 <- survreg(Surv(Age, Status)~Agem + Residence + Wealth + Education + Employment + Peducation + Media, data=FMG, dist="weibull")
model2 <- survreg(Surv(Age, Status)~Agem + Residence + Wealth + Education + Employment + Peducation + Media, data=FMG, dist="lognormal")
model3 <- survreg(Surv(Age, Status)~Agem + Residence + Wealth + Education + Employment + Peducation + Media, data=FMG, dist="loglogistic")

# Display summary statistics for each model
summary(model1)
summary(model2)
summary(model3)

# Compare the AIC values for the different models to evaluate fit quality
AIC(model1, model2, model3)  # AIC values to compare models

# Confidence intervals for model2 (lognormal)
confint(model2)  # Confidence intervals for model2

# Diagnostic plot of Weibull distribution by residence
WeibullDiag(Surv(Age, Status) ~ Residence, data=FMG, labels=c("Rural", "Urban"))

# Fit survival function based on the variable 'Religion'
ag <- survfit(Surv(Age, Status) ~ Religion, data = FMG)
plot(ag, xlab="Age", ylab="Survival", col=1:6, lwd=3)  # Plot survival curve
legend("topright", levels(FMG$Religion), lty=1, col=1:6, bty="n")  # Add legend
plot(ag, fun="cumhaz", xlab="Age", ylab="Survival", col=1:6, lwd=3)  # Cumulative hazard plot

# Fit survival function based on the 'Agem' variable and plot it
ag <- survfit(Surv(Age, Status) ~ Agem, data = FMG)
q1 <- autoplot(KM, surv.size=1, censor=F) + ggtitle("Survival plot of FMG by mother age") +
  labs(x = "Time in year", y = "Survival Probability") + 
  labs(colour = "Age of Mothers") + 
  scale_color_manual(labels = c("15-24", "25-34", "35-49"), values = c(2, 1, 3)) +
  guides(fill=FALSE) + theme(panel.background = element_rect(fill='grey75'))  # GGPlot-based visualization

# Plot survival curves by 'Residence' (Urban vs Rural)
res <- survfit(Surv(Age, Status) ~ Residence, data=FMG)
q2 <- autoplot(res, surv.size=1, censor=F) + ggtitle("Survival plot of FMG by place of residence") +
  labs(x = "Time in years", y = "Survival Probability") + 
  labs(colour = "Residence") + scale_color_manual(labels = c("Rural", "Urban"), values = c(2, 1)) +
  guides(fill=FALSE) + theme(panel.background = element_rect(fill='grey75'))  # Custom plot of survival curves

# Similar steps repeated for other categorical variables such as 'Religion', 'Wealth', 'Education', 'Employment', etc.
# Here, we are visualizing survival curves for different categories of each variable.
rel <- survfit(Surv(Age, Status) ~ Religion, data = FMG)
q3 <- autoplot(rel, surv.size=1, censor=F) + ggtitle("Survival plot of FMG by religion") +
  labs(x = "Time in years", y = "Survival Probability") + 
  labs(colour = "Religion") + 
  scale_color_manual(labels = c("Christian", "Muslim", "Other"), values = c(1, 2, 3)) +
  guides(fill=FALSE) + theme(panel.background = element_rect(fill='grey75'))

# Repeat for 'Wealth', 'Education', 'Employment', etc., changing the survival curve's color and labels as necessary

grid.arrange(q1, q2, q3, q4)  # Arrange multiple survival plots in a grid layout
grid.arrange(q5, q6, q7, q8)  # Another arrangement for different survival plots

# Plot the Kaplan-Meier survival curve without any categorical grouping (overall survival)
surv0 <- survfit(Surv(Age, Status) ~ 1, data = FMG)
s1 <- autoplot(surv0, surv.size=1, censor=F, surv.col=2) + ggtitle("Survival plot of FMG") +
  labs(x = "Time in years", y = "Survival Probability") + labs(colour = "Survival curve") +
  guides(fill=FALSE) + theme(panel.background = element_rect(fill='grey75'))

# Plot cumulative hazard for the FMG data
surv0 <- survfit(Surv(Age, Status) ~ 1, data = FMG)
s2 <- autoplot(surv0, surv.size=1, censor=F, surv.col=2, fun="cumhaz") +
  ggtitle("Hazard plot of FMG") + labs(x = "Time in years", y = "Cumulative Hazard") +
  guides(fill=FALSE) + theme(panel.background = element_rect(fill='grey75'))

# Grid arrangement for the survival and hazard plots
grid.arrange(s1, s2)

# Plot Kaplan-Meier survival curve based on the 'Agem' (age of mothers)
autoplot(survfit(Surv(Age, rep(1, length(Status))) ~ Agem), col=c(3,6,7)) + ggtitle("Mother age based Survival") +
  labs(x = "Year", y = "Survival Probability") + guides(fill=FALSE) + 
  labs(colour = "Age of Mothers") + scale_color_manual(labels = c("15-24", "25-34", "35-49"), values = c(3, 2, 1))

# Print out the table of FMG religion distribution for analysis
table(FMG$Religion)
###############Modeling Determinants of Low Birth Weight for Under Five-Children in Ethiopia################
####### Loading Necessary Packages ######
# These packages are required for the analysis.
library(foreign)    # To read SPSS data
library(lattice)    # For graphical data representation
library(nlme)       # For linear mixed-effects models
library(faraway)    # For statistical modeling
library(gmodels)    # For regression models
library(lme4)       # For mixed-effects models
library(rcpp)       # For Rcpp integration

# Clear the environment to start fresh
rm(list = ls())

# ##############################
# ### Reading and Preparing Data ###
# ##############################

# Reading data from SPSS file and displaying the contents
lbirth <- read.spss("C:\\Users\\TOSHIBA\\Desktop\\Birth13trial.sav", to.data.frame = TRUE)
View(lbirth)  # View the dataset

# Convert the data to a dataframe format
lbirthh <- as.data.frame(lbirth)
head(lbirthh)  # Display the first few rows for inspection

# Attach the dataset for direct referencing of columns
attach(lbirthh)

# ##############################
# ### Logistic Regression Model ###
# ##############################

# Fit a logistic regression model to predict 'Childsize' based on various factors
fit <- glm(Childsize ~ Sex + Wealth + Residence + Age + Termpregnancy +
           Antenatalcare + Maritalst + Vaccination + Anemia + Educationle + 
           Birthorder + Prebirthinterval, family = binomial, data = lbirthh)
summary(fit)  # View a summary of the model results

# ##########################
# ### Mixed-Effects Models ###
# ##########################

# Fit a mixed-effects logistic regression model including random effects for 'Region' and 'CLID'
fit1 <- glmer(Childsize ~ Sex + Wealth + Residence + Age + Termpregnancy + 
              Antenatalcare + Maritalst + Vaccination + Anemia + Educationle + 
              Birthorder + Prebirthinterval + (1|Region) + (1|CLID),
              family = binomial, data = lbirthh)

# Print the model's result without correlation values and show summary
print(fit1, corr = FALSE)
summary(fit1)

# Compare the random effects of different models
anova(fit1, fit3, fit4, fit5, fit6)

# ##########################
# ### Fitting More Models ###
# ##########################

# Repeated analysis with different random effect structures:
fit2 <- glmer(Childsize ~ Sex + Wealth + Residence + Age + Termpregnancy +
              Antenatalcare + Maritalst + Vaccination + Anemia + Educationle + 
              Birthorder + Prebirthinterval + (1|CLID), family = binomial, data = lbirthh)
print(fit2, corr = FALSE)
summary(fit2)

# Compare model fit between 'fit1' and 'fit2'
anova(fit2, fit1)

# ##########################
# ### Comparison of Random Effects ###
# ##########################
# Fit additional models with different variations of random effects
fit3 <- glmer(Childsize ~ Sex + Wealth + Residence + Age + Termpregnancy + 
              Antenatalcare + Maritalst + Vaccination + Anemia + Educationle + 
              Birthorder + (1|Region) + (1|CLID), family = binomial, data = lbirthh)
print(fit3, corr = FALSE)
summary(fit3)

# Fitting models with various specifications for comparison
fit4 <- glmer(Childsize ~ Sex + Wealth + Residence + Age + Antenatalcare + 
              Maritalst + Vaccination + Anemia + Educationle + Birthorder +
              (1|Region) + (1|CLID), family = binomial, data = lbirthh)
summary(fit4)

# Adjust model further with reduced parameters
fit5 <- glmer(Childsize ~ Sex + Wealth + Residence + Age + Antenatalcare + 
              Maritalst + Vaccination + Anemia + Educationle + (1|Region) + 
              (1|CLID), family = binomial, data = lbirthh)
summary(fit5)

# Refined model with fewer predictors
fit6 <- glmer(Childsize ~ Sex + Wealth + Age + Antenatalcare + Maritalst + 
              Vaccination + Anemia + Educationle + (1|Region) + (1|CLID), 
              family = binomial, data = lbirthh)
summary(fit6)

# ##########################
# ### Calculate Confidence Intervals ###
# ##########################

# Compute fitted values and their 95% confidence intervals for model 'fit6'
fitted_values <- fitted(fit6)
lower <- coef(summary(fit6))[, 1] + qnorm(0.025) * coef(summary(fit6))[, 2]
upper <- coef(summary(fit6))[, 1] + qnorm(0.975) * coef(summary(fit6))[, 2]
cbind(coef(summary(fit6)), lower, upper)

# ##############################
# ### Comparison of Random Effects ###
# ##############################
# Compare all models for random effects and determine the best fit
anova(fit1, fit2, fit3, fit4, fit5, fit6)

# ##########################
# ### Model Diagnosis Plots ###
# ##########################

# Set up 2x2 plot grid
par(mfrow = c(2, 2))

# 1. Residuals Normality Check
qqnorm(resid(fit6), main = "Residual Normal Plot", col = 3, adj = 0.1)

# 2. Residuals vs. CLID
plot(lbirthh$CLID, resid(fit6), xlab = "CLID", ylab = "Residuals",
     main = "Residuals vs. Observation", col = 4, adj = 0.1)
abline(h = 0, col = 2)  # Add horizontal line at 0

# 3. Random Effects at Region Level
qqnorm(ranef(fit6)$"Region"[[1]], main = "Regional Level Random Effects", col = 2, adj = 0.1)

# 4. Random Effects at Cluster Level (CLID)
qqnorm(ranef(fit6)$"CLID"[[1]], main = "Cluster Level Random Effects", col = 6, adj = 0.1)

# End of the script

