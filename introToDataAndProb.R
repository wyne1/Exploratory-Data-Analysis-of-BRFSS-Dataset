library(ggplot2)
library(dplyr)

load("brfss2013.RData")
dim(brfss2013)

names(brfss2013)

# Select appropriate variables from dataset and omit NAs
q1 <- select(brfss2013,genhlth,sex,X_bmi5cat,smokday2) %>% na.omit()
dim(q1)

unique(q1$genhlth)
unique(q1$sex)
length(brfss2013$mishopls %>% na.omit())
plot(q1$genhlth, q1$X_bmi5cat)
g <- ggplot(q1) + aes(x=X_bmi5cat,fill=genhlth) + geom_bar(position = "fill") + help(goem_bar)
g <- g + xlab("BMI category") + ylab("Proportion") + scale_fill_discrete(name="Reported Health")
g

# select required variables
smoking <- select(brfss2013,smoke100,genhlth,sex) %>% na.omit()
smokPlot <- ggplot(smoking) + aes(x=,fill=genhlth) + geom_bar(position = "fill") 
smokPlot <- smokPlot + xlab("BMI category") + ylab("Proportion") + scale_fill_discrete(name="Reported Health")
smokPlot

g <- ggplot(smoking) + aes(x=sex,fill=genhlth) + geom_bar(position = "fill") + facet_grid(.~smoke100)

g
g <- ggplot(smoking) + aes(x=smokday2,fill=genhlth) + geom_bar(position = "fill") + facet_grid(.~sex)
g
g <- ggplot(smoking) + aes(x=smoke100,fill=genhlth) + geom_bar(position = "dodge") + facet_grid(.~sex)
g
g <- ggplot(smoking) + aes(x=X_bmi5cat,fill=genhlth) + geom_bar(position = "fill") + facet_grid(.~smoke100)
g

x <- ggplot(smoking) + aes(x = genhlth, y = smoke100) + geom_boxplot()
x
g <- ggplot(smoking) + aes(x=smoke100,fill=genhlth) + geom_bar(position = "dodge") + facet_grid(.~sex)
g

g <- ggplot(smoking) + aes(x=sex,fill=genhlth) + geom_bar(position = "fill") + facet_grid(.~X_bmi5cat) 
g


emoHealth <- select(brfss2013, employ1, misdeprd,smoke100, genhlth) %>% na.omit()
emoPlot <- ggplot(emoHealth) + aes(x=misdeprd,fill=genhlth) + geom_bar(position = "fill") 
emoPlot
brfss2013$sleptim1 %>% na.omit()
help(geom_bar())

"WORKING ON EMOTIONAL HEALTH AND SALARY HERE"

mentalIncome <- select(brfss2013, employ1, income2, misdeprd, misrstls, misnervs, sex) %>% na.omit()
head(mentalIncome) 
dim(mentalIncome)

mentalPlot <- ggplot(mentalIncome) + aes(x = misdeprd, fill=employ1) + geom_bar(position = "fill") 
+ scale_x_discrete(name = "Income",
                   labels = c("Less than $10,000" = "< $10,000", 
                              "Less than $15,000" = "< $15,000",
                              "Less than $20,000" = "< $20,000",
                              "Less than $25,000" = "< $25,000", 
                              "Less than $35,000" = "< $35,000",
                              "Less than $50,000" = "< $50,000", 
                              "Less than $75,000" = "< $75,000",
                              "$75,000 or more" = ">= $75,000"))
x <- ggplot(data = mentalIncome) + pl
unique(mentalIncome$misrstls)
geom_bar()

t1 <- select(brfss2013, sex, income2, seatbelt, educa, scntwrk1, employ1, lsatisfy, X_bmi5) %>%
      filter(!is.na(sex), !is.na(income2), !is.na(seatbelt), !is.na(educa), !is.na(scntwrk1), !is.na(lsatisfy),!is.na(X_bmi5),!is.na(employ1))
ggplot(data = t1, aes(x = income2, y = X_bmi5, fill = sex)) +
      geom_boxplot() + geom_abline(slope=0)+geom_point(size=4, position=position_dodge(width=0.08))
      scale_x_discrete(name = "Income",
                       labels = c("Less than $10,000" = "< $10,000", 
                                "Less than $15,000" = "< $15,000",
                                "Less than $20,000" = "< $20,000",
                                "Less than $25,000" = "< $25,000", 
                                "Less than $35,000" = "< $35,000",
                                "Less than $50,000" = "< $50,000", 
                                "Less than $75,000" = "< $75,000",
                                "$75,000 or more" = ">= $75,000")) +
      scale_y_continuous(name = "Body Mass Index") +
      ggtitle("Body Mass across income and gender") +
      theme(legend.position = "bottom",
      legend.background = element_rect(fill = "gray90", size = .5, linetype = "dotted"))

dim(t1)
eduEmploye <- select(brfss2013, educa, sex, employ1, income2, X_bmi5) %>%
      filter(!is.na(sex), !is.na(income2), !is.na(educa),!is.na(X_bmi5),!is.na(employ1))

eduPlot <- ggplot(eduEmploye) + aes(x = income2, fill = educa) + geom_bar(position = "fill")+
      scale_x_discrete(name = "Income",
                       labels = c("Less than $10,000" = "< $10,000", 
                                "Less than $15,000" = "< $15,000",
                                "Less than $20,000" = "< $20,000",
                                "Less than $25,000" = "< $25,000", 
                                "Less than $35,000" = "< $35,000",
                                "Less than $50,000" = "< $50,000", 
                                "Less than $75,000" = "< $75,000",
                                "$75,000 or more" = ">= $75,000")) + 
  theme( legend.position = "bottom",legend.background = element_rect(fill = "darkgray"),
  legend.key = element_rect(fill = "lightblue", color = NA),
  # Change legend key size and key width
  legend.key.size = unit(0.2, "cm"),
  legend.key.width = unit(0.2,"cm") )
eduPlot
dim(eduEmploye)

summary(eduEmploye$sex)

