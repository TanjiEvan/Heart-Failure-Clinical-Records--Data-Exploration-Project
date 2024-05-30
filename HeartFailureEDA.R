
mydata <- read.csv("E:/HeartFailureClinicalRecords.csv" , header = TRUE, sep = ",")

mydata


str(mydata) 
summary(mydata)


mean(mydata$age, na.rm = TRUE)
median(mydata$age, na.rm = TRUE)

install.packages("modeest")
library(modeest)

mode_age <- mfv(mydata$age)
mode_age


plot_data <- mydata
hist(plot_data$ejection_fraction, main = "Histogram of Ejection Fraction", xlab = "Ejection Fraction", ylab = "Frequency", col = "skyblue", border = "black")
mean_ejection_fraction <- mean(plot_data$ejection_fraction, na.rm = TRUE)
median_ejection_fraction <- median(plot_data$ejection_fraction, na.rm = TRUE)
mode_ejection_fraction<- mfv(plot_data$ejection_fraction)

abline(v = mean_ejection_fraction, col = "red", lwd = 3)
abline(v = mode_ejection_fraction, col = "green", lwd = 3)
points(median_ejection_fraction, 0, col = "blue", pch = 19)
legend("topright", legend = c("Mean", "Mode", "Median"), col = c("red", "green", "blue"), lwd = 2, pch = c(NA, NA, 19))




mydata$creatinine_phosphokinase[mydata$creatinine_phosphokinase == ""] <- NA
mydata$sex[mydata$sex == ""] <- NA
colSums(is.na(mydata))


library(naniar)
gg_miss_var(mydata)




invalid_values<-mydata

invalid_values$age[invalid_values$age < 0 | invalid_values$age > 120] <- NA
invalid_values$age

invalid_values$anaemia[invalid_values$anaemia != 0 & invalid_values$anaemia != 1] <- NA
invalid_values$anaemia

invalid_values$creatinine_phosphokinase<-as.numeric(invalid_values$creatinine_phosphokinase)
invalid_values$creatinine_phosphokinase

invalid_sex <- invalid_values$sex[invalid_values$sex != "Male" & invalid_values$sex != "Female"]<- NA
invalid_values$sex

sexMode <- mfv(invalid_values$sex)
sexMode
invalid_values$sex[is.na(invalid_values$sex)] <- sexMode
invalid_values$sex
valid_sex_values<- invalid_values$sex 
invalid_values$sex

discard_invalid_value <- na.omit(invalid_values)
discard_invalid_value



data_outlier<- mydata
boxplot(data_outlier$platelets)
box_plot_platelets <- boxplot.stats(data_outlier$platelets)
box_plot_platelets $out
data_outlier$platelets[data_outlier$platelets %in% box_plot_platelets$out] <- NA
data_outlier$platelets
discard_outlier_value <- na.omit(data_outlier$platelets)
discard_outlier_value

boxplot(data_outlier$age)
box_plot_age <- boxplot.stats(data_outlier$age)
box_plot_age $out
discard_outlier_age<-data_outlier$age[!data_outlier$age %in% box_plot_age$out]
discard_outlier_age



missing_values<-mydata

discard_missing_value <- na.omit(missing_values)
discard_missing_value

AgeMean <- mean(discard_outlier_age, na.rm = TRUE)
AgeMean
discard_outlier_age[is.na(discard_outlier_age)] <- AgeMean
discard_outlier_age
Print_age <- as.integer(discard_outlier_age) 
Print_age

plateletsMedian <- median(missing_values$platelets, na.rm = TRUE)
plateletsMedian
missing_values$platelets[is.na(missing_values$platelets)] <- plateletsMedian
missing_values$platelets

smokingMode <- mfv(missing_values$smoking)
smokingMode
missing_values$smoking[is.na(missing_values$smoking)] <- smokingMode
missing_values$smoking


categorical_conversion<- mydata

invalid_values$sex <- ifelse(invalid_values$sex == "Male", 1, ifelse(invalid_values$sex == "Female", 2, NA))
invalid_values$sex

categorical_conversion$ejection_fraction <- ifelse(categorical_conversion$ejection_fraction >= 50 & categorical_conversion$ejection_fraction <= 70, "Normal",
                                            ifelse(categorical_conversion$ejection_fraction >= 41 & categorical_conversion$ejection_fraction <= 49, "Mildly abnormal",
                                            ifelse(categorical_conversion$ejection_fraction >= 30 & categorical_conversion$ejection_fraction <= 40, "Moderately abnormal",
                                            ifelse(categorical_conversion$ejection_fraction < 30, "Severely abnormal", NA))))
categorical_conversion$ejection_fraction



normalization<-discard_invalid_value
min_max_normalize <- function(x) {return((x - min(x)) / (max(x) - min(x)))}
normalization_platelets <- min_max_normalize(normalization$platelets)
normalization_platelets















