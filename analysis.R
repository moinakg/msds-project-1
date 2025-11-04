library(tidyverse)
library(gridExtra)
library(cowplot)

empData <- read.csv("C:/Users/moina/MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/CaseStudy1-data.csv", header=TRUE)

# Lets first summarize the data by attrition count for each
# joblevel within the different departments
empData$Attrition <- ifelse(empData$Attrition == "Yes", 1, 0)
selectDf <- empData[, c("Department", "Attrition", "JobLevel")]
totalDf <- selectDf %>%
  group_by(Department, JobLevel) %>%
  summarize(Attrition_count = sum(Attrition, na.rm=TRUE), Total = n())

# Now lets do a grouped bar plot to show this
totalDf$Percent_attrition <- (totalDf$Attrition_count / totalDf$Total) * 100
totalDf$JobLevel = as.factor(totalDf$JobLevel)
ggplot(data = totalDf, aes(fill=JobLevel, x=Department, y=Percent_attrition)) +
  geom_bar(position="dodge", stat = "identity") +
  labs(title = "Percentage Attrition by Department and Level",
       y = "Percentage Attrition",
       fill = "Job Level")

# As we will see Sales has the highest percentage Attrition.
# Also in general, the lowest job level has the highest attrition followed by the
# most senior roles.
# Mid-career employees seem less likely to switch jobs
#

# Now lets see the highest absolute number of attritions by department. A department
# may have the highest percentage of attritions but it may have lesser number of
# employees which can skew the percentage
#
totalDf <- selectDf %>%
  group_by(Department) %>%
  summarize(Attrition_yes = sum(Attrition, na.rm=TRUE), Attrition_no = n() - sum(Attrition, na.rm=TRUE))

totalDfLong <- totalDf %>% 
  pivot_longer(
    cols = starts_with("Attrition"),
    names_to = "Attrition",
    values_to = "Count"
  )

ggplot(data = totalDfLong, aes(fill=Attrition, x=Department, y=Count)) +
  geom_bar(position="stack", stat = "identity") +
  labs(title = "Total Employees vs Attrition count by Department",
       y = "Attrition Count",
       fill = "Attrition yes/no") +
  scale_fill_discrete(labels = c("Attrition: NO", "Attrition: YES"))

# Same as above but only for joblevel 1
totalDf <- selectDf[selectDf$JobLevel == 1,] %>%
  group_by(Department) %>%
  summarize(Attrition_yes = sum(Attrition, na.rm=TRUE), Attrition_no = n() - sum(Attrition, na.rm=TRUE))

totalDfLong <- totalDf %>% 
  pivot_longer(
    cols = starts_with("Attrition"),
    names_to = "Attrition",
    values_to = "Count"
  )

ggplot(data = totalDfLong, aes(fill=Attrition, x=Department, y=Count)) +
  geom_bar(position="stack", stat = "identity") +
  ylim(0, 550) +
  labs(title = "Total Employees vs Attrition count by Department (Job Level 1 only)",
       y = "Attrition Count",
       fill = "Attrition yes/no") +
  scale_fill_discrete(labels = c("Attrition: NO", "Attrition: YES"))


# From this graph we can see some data which tends to hold up the previous observation.
# R&D has slightly more attrition than Sales by absolute count. However, it also has the
# highest number of employees. The difference in attrition count is much less than the
# large difference in employee count between R&D and Sales.
# Thus, we can see that R&D employees are more likely to stay compared to Sales. This is
# the same picture that is evident from the percentage graph.
# Human Resources has much less employees are a low attrition count.

# So, the next analysis we can focus on attritions within R&D and Sales and look at the Job Level
# 1 in more detail where we see the highest number of attritions.
#

# First lets look at which roles are most affected within R&D and Sales
#
selectDf <- empData[, c("Department", "Attrition", "JobRole")]
totalDf <- selectDf[selectDf$Department == "Research & Development" | selectDf$Department == "Sales",] %>%
  group_by(Department, JobRole) %>%
  summarize(Attrition_count = sum(Attrition, na.rm=TRUE), Total = n())

totalDf$Percent_attrition <- (totalDf$Attrition_count / totalDf$Total) * 100
totalDf$JobRole = as.factor(totalDf$JobRole)
ggplot(data = totalDf, aes(fill=JobRole, x=Department, y=Percent_attrition)) +
  geom_bar(position="dodge", stat = "identity") +
  labs(title = "Percentage Attrition by Department and Role",
       y = "Percentage Attrition",
       fill = "Job Role")

selectDf = empData[, c("Department", "Attrition", "JobRole", "JobLevel")]
rolesRd = unique(empData[selectDf$Department == "Research & Development",]$JobRole)
rolesSales = unique(empData[selectDf$Department == "Sales",]$JobRole)
jobLevels = unique(empData$JobLevel)
plots = list()
plot_1 = list()
common_legend = NULL

# Generate plots for each job level
#
for (i in 1:length(jobLevels)) {
  level = jobLevels[i]
  totalDf = selectDf[(selectDf$Department == "Research & Development" | selectDf$Department == "Sales") & selectDf$JobLevel == level,] %>%
    group_by(Department, JobRole) %>%
    summarize(Attrition_count = sum(Attrition, na.rm=TRUE), Total = n())
    totalDf$Percent_attrition <- (totalDf$Attrition_count / totalDf$Total) * 100

  # Not all Job levels have employees in all roles. Add the missing roles for
  # each department with 0 values to ensure that the groups are consistent and
  # the bar plots are consistent as a result.
  #
  for (j in 1:length(rolesRd)) {
    if (!any(grepl(rolesRd[j], totalDf$JobRole))) {
      rDF = data.frame(
          Department = "Research & Development",
          JobRole = rolesRd[j],
          Attrition_count = 0,
          Total = 0,
          Percent_attrition = 0
        )
      totalDf = rbind(totalDf, rDF)
    }
  }
  for (j in 1:length(rolesSales)) {
      if (!any(grepl(rolesSales[j], totalDf$JobRole))) {
        rDF = data.frame(
          Department = "Sales",
          JobRole = rolesSales[j],
          Attrition_count = 0,
          Total = 0,
          Percent_attrition = 0
        )
        totalDf = rbind(totalDf, rDF)
      }
  }
  totalDf$JobRole = as.factor(totalDf$JobRole)
  if (level == 1) {
    pl = ggplot(data = totalDf, aes(fill=JobRole, x=Department, y=Percent_attrition)) +
      geom_bar(position = position_dodge2(preserve = "single"), stat = "identity") +
      ylim(0, 50) +
      labs(title = paste0("% Attrition for Job Level ", level),
           y = "Percentage Attrition",
           fill = "Job Role") +
      theme(plot.title = element_text(size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 7))
    common_legend = get_legend(pl)
    plot_1[[level]] = pl
  } else {
    pl = ggplot(data = totalDf, aes(fill=JobRole, x=Department, y=Percent_attrition)) +
      geom_bar(position = position_dodge2(preserve = "single"), stat = "identity") +
      ylim(0, 50) +
      labs(title = paste0("% Attrition for Job Level ", level),
           y = "Percentage Attrition",
           fill = "Job Role") +
      theme(plot.title = element_text(size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            legend.position = "none")
    plots[[level]] = pl
  }
}

plot_1[[1]]
plots <- plots[-1]
plots[[length(plots) + 1]] =  common_legend
grid.arrange(grobs = plots, ncol = 3)

# Job level 2 has  the lowest attrition rates in general
# Job level 3 has the highest attrition within the various R&D roles
# Job levels 1, 2, 4 and 5 have highest attrition in the Sales roles
# Overall the role "Sales representative" for Job role 1 has the highest
# percentage attrition  among all other roles in any group.
#
# So, we can first focus on job level 3 for R&D and Job level 1 in Sales and
# later try to find other patterns in other groups.
# Looking at "Sales Representatives" we can see that there are no attritions
# in this roles for the other job levels. Lets see how many such higher level
# sales representatives are there.
result = empData[empData$Department == "Sales" &
                   empData$JobLevel != 1 &
                   empData$JobRole == "Sales Representative" &
                   empData$Attrition == 0,]
count(result)
# n
# 1 3

# There are just 3 such cases. This means that a lot of junior sales
# representatives tend to eventually leave or get promoted. The role
# "Sales Executive" seems to be the next level role. That seems to be
# indicated by the available data. There are no sales executives at job
# level 1

length(empData[empData$JobRole == "Sales Executive" &
                 empData$Attrition == 0 &
                 empData$JobLevel == 1,]$YearsAtCompany)
# [1] 0

length(empData[empData$JobRole == "Sales Executive" &
                 empData$Attrition == 0 &
                 empData$JobLevel > 1,]$YearsAtCompany)
# [1] 167

# However there is attrition among sales executives as well, though not as high
# as for sales representatives. From the graphs we can see that the attrition rate
# for sales executives increases with level. Could it be that they do not see a
# growth path beyond that level? Lets look at some counts for managers in the sales
# department.

length(empData[empData$Department == "Sales" &
                 empData$JobRole == "Manager" &
                 empData$Attrition == 0,]$YearsAtCompany)
# [1] 18

length(empData[empData$Department == "Sales" &
                 empData$JobRole == "Manager" &
                 empData$Attrition == 1,]$YearsAtCompany)
# [1] 2

# So only 18 managers and very little attrition among sales managers. It seems to
# indicate that the growth path could be an issue. Now lets do some correlations to
# assess other factors.


install.packages("polycor")
library(polycor)
library(corrplot)

empData <- read.csv("C:/Users/moina/MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/CaseStudy1-data.csv", header=TRUE)
empData$Attrition <- ifelse(empData$Attrition == "Yes", 1, 0)
selectDf = empData[, -c(1, 4, 5, 6, 9, 10, 11, 12, 13, 14, 17, 19, 21, 23, 24, 28)]
cmat = cor(selectDf)
corrplot(cmat, method = "circle", order = "hclust")

#
# From this correlation plot we can see some weak correlations with respect to
# Attrition and a bunch of other fields, notably:
# JobInvolvement(-0.1877934090), StockOptionLevel(-0.148680303),
# YearsWithCurrentManager(-0.146782245), YearsAtCompany(-0.128754060),
# YearsInCurrentRole(-0.156215707), Age(-0.149383577), TotalWorkingYears(-0.167206122),
# JobLevel(-0.162136444), MonthlyIncome(-0.1549149555) and JobSatisfaction(-0.107520935)
# However, the top 3 correlations are with JobInvolvement, TotalWorkingYears and JobLevel
#
# Note that these are all negative correlations. That is larger numbers are associated with
# fewer attritions. Some of them are quite obvious like StockOptionLevel
#

selectDf1 <- empData[,c("Attrition", "BusinessTravel", "EducationField", "JobRole", "MaritalStatus", "Gender", "OverTime", "Department")]
selectDf1$BusinessTravel = as.factor(selectDf1$BusinessTravel)
selectDf1$EducationField = as.factor(selectDf1$EducationField)
selectDf1$JobRole = as.factor(selectDf1$JobRole)
selectDf1$MaritalStatus = as.factor(selectDf1$MaritalStatus)
selectDf1$Gender = as.factor(selectDf1$Gender)
selectDf1$OverTime = as.factor(selectDf1$OverTime)
selectDf1$Department = as.factor(selectDf1$Department)
cmat1 = hetcor(selectDf1)
corrplot(cmat1$correlations)

#
# This plot shows polychoric correlations between Attrition and non-numeric variables:
# BusinessTravel, Department, EducationFields, JobRole and MaritalStatus
#
# Interestingly, the significant correlations visible here are MaritalStatus and OverTime. This
# is a positive correlation. The factor levels are "Divorced Married Single". This means that
# Single employees are more likely to leave.
# For OverTime the factor levels are No, Yes. The correlation here is stronger. It is
# likely that employees subject to OverTime are more likely to leave.
# 

attrition_by_field <- function(df, column, y1, y2, titleStr) {
  selectDf <- df[, c("Department", "Attrition", "JobLevel", column)]
  cValues = unique(df[[column]])
  jobLevels = unique(df$JobLevel)
  plots = list()

  for (i in 1:length(cValues)) {
    mValue = cValues[i]
    totalDf <- selectDf %>% filter(.data[[column]] == mValue) %>%
      group_by(Department, JobLevel) %>%
      summarize(Attrition_count = sum(Attrition, na.rm=TRUE), Total = n())
    totalDf$Percent_attrition <- (totalDf$Attrition_count / totalDf$Total) * 100
    totalDf$JobLevel = as.factor(totalDf$JobLevel)
    p = ggplot(data = totalDf, aes(fill=JobLevel, x=Department, y=Percent_attrition)) +
      geom_bar(position="dodge", stat = "identity") +
      ylim(y1, y2) +
      labs(title = paste0(titleStr, mValue) ,
           y = "Percentage Attrition",
           fill = "Job Level") +
      theme(plot.title = element_text(size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 7))
    plots[[i]] = p
  }
  plots
}

# Lets look at Marital Status in more detail
grid.arrange(grobs =
              attrition_by_field(
                empData,
                "MaritalStatus", 0, 75,
                "Percentage Attrition by Department and Level for "),
             ncol = 2)

# Same thing but for OverTime
grid.arrange(grobs =
               attrition_by_field(
                 empData,
                 "OverTime", 0, 100,
                 "Percentage Attrition by Department and Level for Overtime: "),
             ncol = 1, nrow = 2)

#
# We can see here that in general Single employees are more prone to leave the job
# especially at level 1 and especially within the Sales department. The Human Resources
# Department bucks the trend!
#

empData <- read.csv("C:/Users/moina/MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/CaseStudy1-data.csv", header=TRUE)
empData$Attrition <- ifelse(empData$Attrition == "Yes", 1, 0)
#selectDf = empData[empData$Department == "Research & Development", -c(1, 4, 5, 6, 9, 10, 11, 12, 13, 14, 17, 19, 21, 23, 24, 28)]
#selectDf = empData[empData$Department == "Research & Development", -c(1, 4, 6, 9, 10, 11, 13, 17, 19, 23, 24, 28)]
selectDf = empData[, -c(1, 4, 6, 9, 10, 11, 13, 17, 19, 23, 24, 28)]
cmat = cor(selectDf)
corrplot(cmat, method = "circle", order = "hclust")

selectDf1 <- empData[,c("Attrition", "BusinessTravel", "EducationField", "JobRole", "MaritalStatus", "Gender", "OverTime", "Department")]
selectDf1$BusinessTravel = as.factor(selectDf1$BusinessTravel)
selectDf1$EducationField = as.factor(selectDf1$EducationField)
selectDf1$JobRole = as.factor(selectDf1$JobRole)
selectDf1$MaritalStatus = as.factor(selectDf1$MaritalStatus)
selectDf1$Gender = as.factor(selectDf1$Gender)
selectDf1$OverTime = as.factor(selectDf1$OverTime)
selectDf1$Department = as.factor(selectDf1$Department)
cmat1 = hetcor(selectDf1)
corrplot(cmat1$correlations)

selectDf = empData[,c("Attrition", "JobLevel", "Department", "MaritalStatus", "JobInvolvement", "YearsInCurrentRole", "OverTime")]
selectDf$Department = as.numeric(as.factor(selectDf$Department))
selectDf$MaritalStatus = as.numeric(as.factor(selectDf$MaritalStatus))
selectDf$OverTime = as.numeric(as.factor(selectDf$OverTime))
set.seed(97)
trainIndices = sample(1:dim(selectDf)[1], round(0.7 * dim(selectDf)[1]))
train = selectDf[trainIndices,]
test = selectDf[-trainIndices,]
classifications = knn(train[,c(2,3,4,5,6,7)], test[,c(2,3,4,5,6,7)], train$Attrition, prob = TRUE, k = 5)
confusionMatrix(table(test$Attrition,classifications))

accuracies = c(); sensitivities = c(); specificities = c()
for (i in 1:100) {
  set.seed(i)
  trainIndices = sample(1:dim(selectDf)[1], round(0.7 * dim(selectDf)[1]))
  train = selectDf[trainIndices,]
  test = selectDf[-trainIndices,]
  classifications = knn(train[,c(2,3,4,5,6,7)], test[,c(2,3,4,5,6,7)], train$Attrition, prob = TRUE, k = 5)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accuracies[i] = CM$overall["Accuracy"]; sensitivities[i] = CM$byClass["Sensitivity"]; specificities[i] = CM$byClass["Specificity"]
}

plotDF = data.frame(
  accuracy = accuracies,
  sensitivity = sensitivities,
  specificity = specificities,
  seed = seq(from = 1, to = 100, by = 1)
)

ggplot(plotDF, aes(seed)) +
  geom_line(aes(y = accuracy, color = "accuracy")) +
  geom_line(aes(y = sensitivity, color = "sensitivity")) +
  geom_line(aes(y = specificity, color = "specificity")) +
  geom_vline(xintercept = 97, color = "red", linetype = "dashed", size = 0.5) +
  annotate("text", x = 97, y = 0.2, label = "seed: 97", color = "darkgreen") +
  scale_x_continuous(minor_breaks = seq(1,100, by = 1),
                     breaks = seq(0, 100, by = 10)) +
  labs(title = "KNN Accuracy, Sensitivity, Specificity for various split seeds")


library(e1071)
selectDf = empData[,c("Attrition", "JobLevel", "JobRole", "Department", "MaritalStatus", "JobInvolvement", "YearsInCurrentRole", "TotalWorkingYears", "MonthlyIncome", "StockOptionLevel", "Age", "OverTime")]
selectDf$Department = as.numeric(as.factor(selectDf$Department))
selectDf$JobRole = as.numeric(as.factor(selectDf$JobRole))
selectDf$MaritalStatus = as.numeric(as.factor(selectDf$MaritalStatus))
selectDf$OverTime = as.numeric(as.factor(selectDf$OverTime))
set.seed(12)
trainIndices = sample(1:dim(selectDf)[1], round(0.7 * dim(selectDf)[1]))
train = selectDf[trainIndices,]
test = selectDf[-trainIndices,]
model = naiveBayes(train[,c(2,3,4,5,6,7,8,9,10,11,12)], train$Attrition)
confusionMatrix(table(predict(model, test[,c(2,3,4,5,6,7,8,9,10,11,12)]), test$Attrition))

accuracies = c(); sensitivities = c(); specificities = c()
for (i in 1:100) {
  set.seed(i)
  trainIndices = sample(1:dim(selectDf)[1], round(0.7 * dim(selectDf)[1]))
  train = selectDf[trainIndices,]
  test = selectDf[-trainIndices,]
  model = naiveBayes(train[,c(2,3,4,5,6,7,8,9,10,11,12)], train$Attrition)
  CM = confusionMatrix(table(predict(model, test[,c(2,3,4,5,6,7,8,9,10,11,12)]), test$Attrition))
  accuracies[i] = CM$overall["Accuracy"]; sensitivities[i] = CM$byClass["Sensitivity"]; specificities[i] = CM$byClass["Specificity"]
}

plotDF = data.frame(
  accuracy = accuracies,
  sensitivity = sensitivities,
  specificity = specificities,
  seed = seq(from = 1, to = 100, by = 1)
)

ggplot(plotDF, aes(seed)) +
  geom_line(aes(y = accuracy, color = "accuracy")) +
  geom_line(aes(y = sensitivity, color = "sensitivity")) +
  geom_line(aes(y = specificity, color = "specificity")) +
  geom_vline(xintercept = 12, color = "red", linetype = "dashed", size = 0.5) +
  annotate("text", x = 12, y = 0.2, label = "seed: 12", color = "darkgreen") +
  scale_x_continuous(minor_breaks = seq(1,100, by = 1),
                     breaks = seq(0, 100, by = 10)) +
  labs(title = "Naive Bayes Accuracy, Sensitivity, Specificity for various split seeds")


install.packages("klaR")
selectDf = empData[,c("Attrition", "JobLevel", "JobRole", "Department", "MaritalStatus", "JobInvolvement", "YearsInCurrentRole", "TotalWorkingYears", "MonthlyIncome", "StockOptionLevel", "Age", "OverTime")]
selectDf$Department = as.numeric(as.factor(selectDf$Department))
selectDf$JobRole = as.numeric(as.factor(selectDf$JobRole))
selectDf$MaritalStatus = as.numeric(as.factor(selectDf$MaritalStatus))
selectDf$OverTime = as.numeric(as.factor(selectDf$OverTime))
selectDf$Attrition = as.factor(selectDf$Attrition)
set.seed(97)
trainIndices = sample(1:dim(selectDf)[1], round(0.7 * dim(selectDf)[1]))
train = selectDf[trainIndices,]
test = selectDf[-trainIndices,]
train_control <- trainControl(method = "cv", number = 5)
#model_nb_cv <- train(Attrition ~ JobLevel + Department + OverTime,
model_nb_cv <- train(Attrition ~ JobLevel + Department + MaritalStatus + JobInvolvement + YearsInCurrentRole + OverTime,
                     data = selectDf, 
                     method = "naive_bayes", 
                     trControl = train_control,
                     tuneLength = 0)
print(model_nb_cv)
