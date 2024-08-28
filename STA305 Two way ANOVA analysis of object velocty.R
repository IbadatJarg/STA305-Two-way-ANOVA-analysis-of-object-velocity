#install and load packages
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("WebPower")) install.packages("WebPower")
if (!require("pwr")) install.packages("pwr")
library(readxl);
library(ggpubr);
library(WebPower);

#Using my Data
my_data <- read_excel("Data - Group WeLoveLuai Lec0102.xlsx");

#Checking/Verifying Data
View(my_data)

set.seed(1234)
dplyr::sample_n(my_data, 10)

str(my_data)

my_data$Degree <- factor(my_data$Degree, 
                         levels = c(15, 30, 45),
                         labels = c("15", "30", "45"))

# group the factors
my_data <- within(my_data,{group <- paste(Material, Degree)})

head(my_data)

#Showing Balanced Design with 5 samples each
table(my_data$Material, my_data$Degree)

#Plotting our Data in a box plot
my_plot <- ggboxplot(my_data, x = "Degree", y = "Time", color = "Material",
                     palette = c("#330421", "#E7B032", "#E22134"))

print(my_plot)

#Two way ANOVA tests

#Additive model
res.aov2 <- aov(Time ~ Material + Degree, data = my_data)
summary(res.aov2)

#Interaction
res.aov3 <- aov(Time ~ Material * Degree, data = my_data)
summary(res.aov3)


# Compare Adjusted R-squared values of Additive vs. Interaction Models

#Additive model
model1 <- lm(my_data$Time ~ my_data$Material + my_data$Degree, data=my_data)
summary(model1)

#Interaction model
model2 <- lm(my_data$Time ~ my_data$Material * my_data$Degree, data=my_data)
summary(model2)


#Summary Statistics of the data
require("dplyr")
group_by(my_data, Material, Degree) %>%
  summarise(
    count = n(),
    mean = mean(Time, na.rm = TRUE),
    sd = sd(Time, na.rm = TRUE)
  )

#alternative summary statistics
model.tables(res.aov3, type="means", se = TRUE)

#Testing for Assumptions (on Additive model, since interaction is not significant at the 0.05 level)

#Testing for Normality
plot(res.aov2, 2)

#Saving residuals
aov_residuals <- residuals(object = res.aov2)

# Running Shapiro-Wilkinson test for normality
shapiro.test(x = aov_residuals )

#Testing for homogeneity of variances
grouped_data <- my_data %>%
  group_by(Material, Degree) %>%
  summarise(variance = var(Time, na.rm = TRUE))

# Perform Bartlett's test of homogeneity of variances   
bartlett.test(Time ~ group, data = my_data) # (please check this line of code) 

#Graph for Homogeneity
plot(res.aov2, 1)

# compute the sample sizes
wp.kanova(n=NULL, ndf=4, f=0.5, ng=9, alpha=0.05, power=0.8);

# compute the confidence intervals for each of the group means
alpha = 0.05

u_cb15 = 0.944 + qt(1-(alpha/2),4) * 0.0966
l_cb15 = 0.944 - qt(1-(alpha/2),4) * 0.0966

u_cb30 = 0.550 + qt(1-(alpha/2),4) * 0.0515
l_cb30 = 0.550 - qt(1-(alpha/2),4) * 0.0515

u_cb45 = 0.652 + qt(1-(alpha/2),4) * 0.0823
l_cb45 = 0.652 - qt(1-(alpha/2),4) * 0.0823

u_cp15 = 1.110 + qt(1-(alpha/2),4) * 0.0890
l_cp15 = 1.110 - qt(1-(alpha/2),4) * 0.0890

u_cp30 = 0.620 + qt(1-(alpha/2),4) * 0.0892
l_cp30 = 0.620 - qt(1-(alpha/2),4) * 0.0892

u_cp45 = 0.656 + qt(1-(alpha/2),4) * 0.0643
l_cp45 = 0.656 - qt(1-(alpha/2),4) * 0.0643

u_mt15 = 1.070 + qt(1-(alpha/2),4) * 0.0843
l_mt15 = 1.070 - qt(1-(alpha/2),4) * 0.0843

u_mt30 = 0.668 + qt(1-(alpha/2),4) * 0.0864
l_mt30 = 0.668 - qt(1-(alpha/2),4) * 0.0864

u_mt45 = 0.622 + qt(1-(alpha/2),4) * 0.0589
l_mt45 = 0.622 - qt(1-(alpha/2),4) * 0.0589

# create the interaction plot
with(my_data, interaction.plot(my_data$Degree, my_data$Material, my_data$Time, col=c("red", "blue", "green"), main="Interaction Plot", xlab="Degree mean", ylab="Time, s"))