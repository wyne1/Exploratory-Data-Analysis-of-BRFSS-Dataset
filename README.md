---
title: "Exploring the BRFSS data"
output:
  html_document:
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages

```{r load-packages, message = FALSE}
library(ggplot2)
library(dplyr)
```

### Load data

Make sure your data and R Markdown files are in the same directory. When loaded
your data file will be called `brfss2013`. Delete this note when before you submit
your work.

```{r load-data}
load("brfss2013.RData")
dim(brfss2013)
```
The dimentions of the data indicate that there are almost 500,000 samples (rows), and around 330 possible features (columns). Since everyone didn't answer all of the questions, some samples have missing features as well. This requires us to preprocess our data before using it to make any decisions


* * *

## Part 1: Data

### BRFSS Background

The Behavioral Risk Factor Surveillance System (BRFSS) is the  system of health-related telephone surveys that collect state data about U.S. residents regarding their health-related risk behaviors, chronic health conditions, and use of preventive services. The CDC official website statest that, "The Behavioral Risk Factor Surveillance System (BRFSS) is the nation’s premier system of health-related telephone surveys that collect state data about U.S. residents regarding their health-related risk behaviors, chronic health conditions, and use of preventive services. Established in 1984 with 15 states, BRFSS now collects data in all 50 states as well as the District of Columbia and three U.S. territories. BRFSS completes more than 400,000 adult interviews each year, making it the largest continuously conducted health survey system in the world."

### How they collect data
“BRFSS is a cross-sectional telephone survey that state health departments conduct monthly over landline telephones and cellular telephones with a standardized questionnaire and technical and methodological assistance from CDC. In conducting the BRFSS landline telephone survey, interviewers collect data from a randomly selected adult in a household. In conducting the cellular telephone version of the BRFSS questionnaire, interviewers collect data from an adult who participates by using a cellular telephone and resides in a private residence or college housing.”

### Generalizability & Causality
Before jumping into the exploratory analysis of the BRFSS data, we need to establish our understanding on the generalizability and causality within the data.

  - The data can be generalized to the broader US population since the surveys are conducted across all 50 US states, which means it captures enough random sample.</p>
  - No causality because the BRFSS is an observational exercise - with no explicit random assignments to treatments - all relationships indicated may indication association, but not causation.



* * *

## Part 2: Research questions

**Research quesion 1:**

> **Is the respondend's opinion about their health status related to the fact that they smoke? Are the observation similar between genders?**

The question is pretty interesting before there is a general perception that people who smoke tend to have a bad health compared to those who don't. The difference between genders is also interesting because if there is an association between smoking and general health, then it should be similar amongst genders.

The variable required for this analysis are:

  - genhlth: Corresponds to General Health
  - smoke100: Smoked At Least 100 Cigarettes
  - smokday2: Frequency Of Days Now Smoking
  - sex: Respondents Sex

**Research quesion 2:**

> **Does being emotionally Depressed, Nervous, and Hopeless have any impact on the general health opinion of the individuals? Is the observation similar between genders**

The analysis of this question is very important in the current status quo. Since people around the globe are now well aware of mental illness, there is a need to understand the impact of it on general health of individuals.

The variable required for this analysis are:

  - genhlth: Corresponds to General Health
  - misdeprd: How Often Feel Depressed Past 30 Days
  - misrstls: How Often Feel Restless Past 30 Days
  - misnervs: How Often Feel Nervous Past 30 Days
  - sex: Respondents Sex

**Research quesion 3:**


* * *

## Part 3: Exploratory data analysis

NOTE: Insert code chunks as needed by clicking on the "Insert a new code chunk"
button (green button with orange arrow) above. Make sure that your code is visible
in the project you submit. Delete this note when before you submit your work.

**Research quesion 1:**

> **Is the respondend's opinion about their health status related to the fact that they smoke? Are the observation similar between genders?**



```{r}
# select required variables
smoking <- select(brfss2013,smoke100,genhlth,sex) %>% na.omit()
smoking2 <- select(brfss2013,smokday2,genhlth,sex) %>% na.omit()
dim(smoking)
dim(smoking2)
```

```{r}
prop.table(table(smoking$smoke100, smoking$genhlth),2)
```
After the initial load of data (over 470,000 observations), we can take an initial look at the frequency of responses and then consider their proportion.

The way to interpret the table above is that for each column (“Excellent”,“Very good”,"Good","Fair","Poor"), what is the proportion of respondents that indicated their smoking (Yes, No). In other words, the column sums up to 1.

Lets jump to graphical representations
``` {r}
smokPlot <- ggplot(smoking2) + aes(x=smokday2,fill=genhlth) + geom_bar(position = "dodge")
smokPlot <- smokPlot + xlab("Categories of Smoking Frequency") + ylab("Proportion") + scale_fill_discrete(name="Reported Health")
smokPlot
```

``` {r}
smokPlot <- ggplot(smoking2) + aes(x=smokday2,fill=genhlth) + geom_bar(position = "fill")
smokPlot <- smokPlot + xlab("Categories of Smoking Frequency") + ylab("Proportion") + scale_fill_discrete(name="Reported Health")
smokPlot
```

The plots depicts the comparison between people who smoke everday, some days, and not at all. The categorical bar graph clearly shows the difference in the proportion of poeple with health and smoke everyday/somedays/not at all. There are indeed a very less proportion of people with excellent health and who smoke everyday. The bar chart of the people who don't smoke at all is a little right skewed, meaning that there are more people towards the excellent/very good health.

This tells us that maybe smoking does have an impact on ones health. Lets explore a little more on this.


``` {r}
smokPlot <- ggplot(smoking) + aes(x=smoke100,fill=genhlth) + geom_bar(position = "fill") + facet_grid(.~sex)
smokPlot <- smokPlot + xlab("Smoked At Least 100 Cigarettes") + ylab("Proportion") + scale_fill_discrete(name="Reported Health")
smokPlot
```

The interesting thing to obeserve here is that both male and female have a similar trend. We see that both have a larger proportion of people with Excellent opinion about their health in the No category of smoking. Similarly, the people with Poor health opinion are in abundance in the peoplpe who smoked more than 100 cigarettes.

Another way to look at this is as a categorical bar graph

```{r}
smokPlot <- ggplot(smoking) + aes(x=smoke100,fill=genhlth) + geom_bar(position = "dodge") + facet_grid(.~sex)
smokPlot <- smokPlot + xlab("Smoked At Least 100 Cigarettes") + ylab("Proportion") + scale_fill_discrete(name="Reported Health")
smokPlot
```

The thing to observe from the above plots is that from a normal distribution perspective, both the bar graphs of people who did not smoke at least 100 cigarettes are left skewed. This means that people who do not smoke tend to have a positive opinion about their health as compared to the people who fall in the other category.

**Research quesion 2:**

> **Does being emotionally Depressed, Nervous, and Hopeless have any impact on the general health opinion of the individuals? Is the observation similar between genders**



```{r}
emoHealth <- select(brfss2013, misnervs,misrstls, misdeprd, genhlth, sex) %>% na.omit()
dim(emoHealth)
dim(x)
```

We have about 36000 samples in total for people who have felt depressed, restless, or nervous in the past 30 days. Although this isn't as good a number as we had with the previous question, we can still derive a generalized impact of all these emotional issues on their general health opinion, or even vise versa.

``` {r}
emoPlot <- ggplot(emoHealth) + aes(x=misdeprd,fill=genhlth) + geom_bar(position = "fill")
emoPlot <- smokPlot + xlab("Felt depressed in the last 30 Days") + ylab("Proportion") + scale_fill_discrete(name="Reported Health")
emoPlot
```

``` {r}
emoPlot <- ggplot(emoHealth) + aes(x=misnervs,fill=genhlth) + geom_bar(position = "dodge")
emoPlot <- smokPlot + xlab("Felt depressed in the last 30 Days") + ylab("Proportion") + scale_fill_discrete(name="Reported Health")
emoPlot
```

``` {r}
emoPlot <- ggplot(emoHealth) + aes(x=misrstls,fill=misdeprd) + geom_bar(position = "dodge")

emoPlot
```


**Research quesion 3:**

```{r}

```
