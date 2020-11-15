---
title: How TV Affected Turnout in the 2016 Election
author: ''
date: '2020-11-14'
slug: []
categories: []
tags: []
---


For my election data science final project my project partner and I chose to look at the effect that watching TV programs had on turnout in the 2016 election. To do this we used the ANES 2016 survey specifically its series of questions on what programs did the participant watch. We matched this up with the survey's question on whether they voted in the 2016 election. I predicted that shows that were focused on politics would have a greater affect on turnout than shows that were apolitical. We broke the shows up into 5 categories: traditional news programs, opinion news or talk shows, entertainment shows that are expressly political, shows that are not expressly political but focus on a salient political issue (in every instance that issue is criminal justice), and shows that are apolitical. We ran a linear regression for each group but did not group the shows allowing us to see how each show affected turnout, all the variables together, each category grouped, and all the categories grouped together. Below is the full code. 


### Libraries ###
```{r}

library(ggplot2)

library(tidyverse)

library(anesr)

library(coefplot)

```

### Data ###

```{r}

data("timeseries_2016")

anes16 <- timeseries_2016 

TV <- anes16 %>% select(V161364, V161365, V161366, V161367, V161368, V161369, V161370, V161371, V161372, V161373, V161374, V161375, V161376, V161377, V161378, V161379, V161380, V161381, V161382, V161383, V161384, V161385, V161386, V161387, V161388, V161389, V161390, V161391, V161392, V161393, V161394, V161395, V161396, V161397, V161398, V161399, V161400, V161401, V161402, V161403, V161404, V161405, V161406, V161407, V161408, V161409, V161410, V161411, V162034) 

Did_You_register <- anes16 %>% select(V161011)

Did_You_Vote <- anes16 %>% select(V162034)


```

### Data Grouped

```{r}

Traditional_Political_News_Programs <- TV %>% select(V161364, V161367, V161380, V161384, V161388, V161390, V161396, V161399, V161405)

Entertainment_or_Opinion_Political_News_Programs <- TV %>% select(V161365, V161370, V161371, V161372, V161375, V161379, V161381, V161382, V161386,V161391, V161393, V161400, V161403, V161404, V161409)

Entertainment_Programs_that_are_Expressly_Political <- TV %>% select(V161385, V161389, V161402, V161406,V161411)

Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue <- TV %>% select(V161366, V161368, V161374, V161377, V161387, V161392, V161397, V161401)

Entertainment_Programs_with_little_to_No_Political_Content <- TV %>% select(V161369, V161373, V161376, V161378, V161383, V161394, V161395, V161398, V161407, V161410)

Twenty_Twenty <- TV %>% select (V161364) %>% unlist()
All_In_with_Chris_Hayes	<- TV %>% select (V161365) %>% unlist()
The_Blacklist <- TV %>% select (V161366) %>% unlist()
CBS_Evening_News_with_Scott_Pelley <- TV %>% select (V161367) %>% unlist()
Criminal_Minds <- TV %>% select (V161368) %>% unlist()
Empire <- TV %>% select (V161369) %>% unlist()
Hannity	<- TV %>% select (V161370) %>% unlist()
Jimmy_Kimmel_Live <- TV %>% select (V161371) %>% unlist()
The_Kelly_File <- TV %>% select (V161372) %>% unlist()
Modern_Family <- TV %>% select (V161373) %>% unlist()
NCIS <-	 TV %>% select (V161374) %>% unlist()
The_Nightly_Show_with_Larry_Wilmore	<- TV %>% select (V161375) %>% unlist()
Sunday_Night_Football <-  TV %>% select (V161376) %>% unlist()
Scorpion <-  TV %>% select (V161377) %>% unlist()
The_Simpsons <-  TV %>% select (V161378) %>% unlist()
Today <- TV %>% select (V161379) %>% unlist()
Sixty_Minutes <- TV %>% select (V161380) %>% unlist()
Anderson_Cooper_Three_Hundred_and_Sixty <- TV %>% select (V161381) %>% unlist()
CBS_This_Morning <- TV %>% select (V161382) %>% unlist()
Dancing_with_the_Stars <-  TV %>% select (V161383) %>% unlist()
Face_the_Nation <- TV %>% select (V161384) %>% unlist()
House_of_Cards <- TV %>% select (V161385) %>% unlist()
Hardball_with_Chris_Matthews <- TV %>% select (V161386) %>% unlist()
Judge_Judy <- TV %>% select (V161387) %>% unlist()
Meet_the_Press	<- TV %>% select (V161388) %>% unlist()
Game_of_Thrones	<- TV %>% select (V161389) %>% unlist()
NBC_Nightly_News_with_Lester_Holt <- TV %>% select (V161390) %>% unlist()
On_the_Record_with_Greta_Van_Susteren <- TV %>% select (V161391) %>% unlist()
Daredevil <- TV %>% select (V161392) %>% unlist()
The_Rachel_Maddow_Show <- TV %>% select (V161393) %>% unlist()
Shark_Tank <- TV %>% select (V161394) %>% unlist()
The_Voice <- TV %>% select(V161395) %>% unlist()
ABC_World_News_with_David_Muir <- TV %>% select (V161396) %>% unlist()
Blue_bloods <-  TV %>% select (V161397) %>% unlist()
Conan <-  TV %>% select (V161398) %>% unlist()
Dateline_NBC <-	TV %>% select (V161399) %>% unlist()
Good_Morning_America <- TV %>% select (V161400) %>% unlist()
Hawaii_Five_O <- TV %>% select (V161401) %>% unlist()
Madam_Secretary	<- TV %>% select (V161402) %>% unlist()
Nancy_Grace	<- TV %>% select (V161403) %>% unlist()
Erin_Burnett_OutFront <- TV %>% select (V161404) %>% unlist()
PBS_News_Hour <-  TV %>% select (V161405) %>% unlist()
Scandal <- TV %>% select (V161406) %>% unlist()
The_Big_Bang_Theory <- TV %>% select (V161407) %>% unlist()
The_Late_Show_with_Stephen_Colbert <- TV %>% select(V161408) %>% unlist()
The_O_Reilly_Factor	<-	 TV %>% select (V161409) %>% unlist()
The_Tonight_Show_Starring_Jimmy_Fallon <- TV %>% select(V161410) %>% unlist()
Alpha_House	<- TV %>% select(V161411) %>% unlist()



```

### Regression for Traditional Political News Programs and Turnout ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Traditional_Political_News_Programs_Moded_vote <- as.data.frame(Twenty_Twenty + CBS_Evening_News_with_Scott_Pelley + Sixty_Minutes + Face_the_Nation + Meet_the_Press + NBC_Nightly_News_with_Lester_Holt + ABC_World_News_with_David_Muir + Dateline_NBC + PBS_News_Hour) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Traditional_Political_News_Programs_Moded_vote_lm <- lm(vote ~  Twenty_Twenty + CBS_Evening_News_with_Scott_Pelley + Sixty_Minutes + Face_the_Nation + Meet_the_Press + NBC_Nightly_News_with_Lester_Holt + ABC_World_News_with_David_Muir + Dateline_NBC + PBS_News_Hour , data = anes_clean) 

summary(Traditional_Political_News_Programs_Moded_vote_lm)

coefplot::coefplot(Traditional_Political_News_Programs_Moded_vote_lm)


```

### Regression for Entertainment or Opinion Political News Programs and Turnout ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Entertainment_or_Opinion_Political_News_Programs_Moded_vote <- as.data.frame(All_In_with_Chris_Hayes + Hannity + Jimmy_Kimmel_Live + The_Kelly_File + The_Nightly_Show_with_Larry_Wilmore + Today + Anderson_Cooper_Three_Hundred_and_Sixty + CBS_This_Morning + Hardball_with_Chris_Matthews + On_the_Record_with_Greta_Van_Susteren + The_Rachel_Maddow_Show + Good_Morning_America + Nancy_Grace + Erin_Burnett_OutFront + The_O_Reilly_Factor) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Entertainment_or_Opinion_Political_News_Programs_Moded_vote_lm <- lm(vote ~ All_In_with_Chris_Hayes + Hannity + Jimmy_Kimmel_Live + The_Kelly_File + The_Nightly_Show_with_Larry_Wilmore + Today + Anderson_Cooper_Three_Hundred_and_Sixty + CBS_This_Morning + Hardball_with_Chris_Matthews + On_the_Record_with_Greta_Van_Susteren + The_Rachel_Maddow_Show + Good_Morning_America + Nancy_Grace + Erin_Burnett_OutFront + The_O_Reilly_Factor , data = anes_clean)

summary(Entertainment_or_Opinion_Political_News_Programs_Moded_vote_lm)

coefplot::coefplot(Entertainment_or_Opinion_Political_News_Programs_Moded_vote_lm)

```

 ### Regression for Entertainment Programs that are Expressly Political and Turnout ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Entertainment_Programs_that_are_Expressly_Political_Moded_vote <- as.data.frame(House_of_Cards + Game_of_Thrones + Madam_Secretary + Scandal + Alpha_House) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Entertainment_Programs_that_are_Expressly_Political_Moded_vote_lm <- lm(vote ~  House_of_Cards + Game_of_Thrones + Madam_Secretary + Scandal + Alpha_House , data = anes_clean)

summary(Entertainment_Programs_that_are_Expressly_Political_Moded_vote_lm)

coefplot::coefplot(Entertainment_Programs_that_are_Expressly_Political_Moded_vote_lm)

```

### Regresion for Entertainment Programs that are not Expressly Political but Focus on a Salient Political issue and Turnout###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote <- as.data.frame( The_Blacklist + Criminal_Minds + NCIS + Scorpion + Judge_Judy + Daredevil + Blue_bloods + Hawaii_Five_O) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_lm <- lm(vote ~ The_Blacklist + Criminal_Minds + NCIS + Scorpion + Judge_Judy + Daredevil + Blue_bloods + Hawaii_Five_O, data = anes_clean)

summary(Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_lm)

coefplot::coefplot(Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_lm)

```

### Regresion for Entertainment Programs with little to No Political Content and Turnout###

```{r}
clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Entertainment_Programs_with_little_to_No_Political_Content_moded_vote <- as.data.frame(Empire + Modern_Family + Sunday_Night_Football + The_Simpsons + Dancing_with_the_Stars + Shark_Tank + The_Voice + Conan + The_Big_Bang_Theory + The_Tonight_Show_Starring_Jimmy_Fallon) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_lm <- lm(vote ~ Empire + Modern_Family + Sunday_Night_Football + The_Simpsons + Dancing_with_the_Stars + Shark_Tank + The_Voice + Conan + The_Big_Bang_Theory + The_Tonight_Show_Starring_Jimmy_Fallon, data = anes_clean)

summary(Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_lm)

coefplot::coefplot(Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_lm)

```

### Regression for All Variables and Turnout ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))


All_Variables_Moded_vote <- as.data.frame(Twenty_Twenty + CBS_Evening_News_with_Scott_Pelley + Sixty_Minutes + Face_the_Nation + Meet_the_Press + NBC_Nightly_News_with_Lester_Holt + ABC_World_News_with_David_Muir + Dateline_NBC + PBS_News_Hour + All_In_with_Chris_Hayes + Hannity + Jimmy_Kimmel_Live + The_Kelly_File + The_Nightly_Show_with_Larry_Wilmore + Today + Anderson_Cooper_Three_Hundred_and_Sixty + CBS_This_Morning + Hardball_with_Chris_Matthews + On_the_Record_with_Greta_Van_Susteren + The_Rachel_Maddow_Show + Good_Morning_America + Nancy_Grace + Erin_Burnett_OutFront + The_O_Reilly_Factor + House_of_Cards + Game_of_Thrones + Madam_Secretary + Scandal + Alpha_House + The_Blacklist + Criminal_Minds + NCIS + Scorpion + Judge_Judy + Daredevil + Blue_bloods + Hawaii_Five_O + Empire + Modern_Family + Sunday_Night_Football +  The_Simpsons + Dancing_with_the_Stars + Shark_Tank + The_Voice + Conan + The_Big_Bang_Theory + The_Tonight_Show_Starring_Jimmy_Fallon) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

All_Variables_Moded_vote_lm <- lm(vote ~ Twenty_Twenty + CBS_Evening_News_with_Scott_Pelley + Sixty_Minutes + Face_the_Nation + Meet_the_Press + NBC_Nightly_News_with_Lester_Holt + ABC_World_News_with_David_Muir + Dateline_NBC + PBS_News_Hour + All_In_with_Chris_Hayes + Hannity + Jimmy_Kimmel_Live + The_Kelly_File + The_Nightly_Show_with_Larry_Wilmore + Today + Anderson_Cooper_Three_Hundred_and_Sixty + CBS_This_Morning + Hardball_with_Chris_Matthews + On_the_Record_with_Greta_Van_Susteren + The_Rachel_Maddow_Show + Good_Morning_America + Nancy_Grace + Erin_Burnett_OutFront + The_O_Reilly_Factor + House_of_Cards + Game_of_Thrones + Madam_Secretary + Scandal + Alpha_House + The_Blacklist + Criminal_Minds + NCIS + Scorpion + Judge_Judy + Daredevil + Blue_bloods + Hawaii_Five_O + Empire + Modern_Family + Sunday_Night_Football +  The_Simpsons + Dancing_with_the_Stars + Shark_Tank + The_Voice + Conan + The_Big_Bang_Theory + The_Tonight_Show_Starring_Jimmy_Fallon, data = anes_clean)

summary(All_Variables_Moded_vote_lm)

coefplot::coefplot (All_Variables_Moded_vote_lm)

```

### Regression for Traditional Political News Programs and Turnout Grouped ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))


Traditional_Political_News_Programs_Moded_vote_grouped <- as.data.frame(Twenty_Twenty + CBS_Evening_News_with_Scott_Pelley + Sixty_Minutes + Face_the_Nation + Meet_the_Press + NBC_Nightly_News_with_Lester_Holt + ABC_World_News_with_David_Muir + Dateline_NBC + PBS_News_Hour) %>%
    rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()



Traditional_Political_News_Programs_Moded_vote_grouped_lm <- lm(vote ~  Traditional_Political_News_Programs_Moded_vote_grouped , data = anes_clean)

summary(Traditional_Political_News_Programs_Moded_vote_grouped_lm)

coefplot::coefplot(Traditional_Political_News_Programs_Moded_vote_grouped_lm)


```

### Regression for Entertainment or Opinion Political News Programs and Turnout Grouped  ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Entertainment_or_Opinion_Political_News_Programs_Moded_vote_grouped <- as.data.frame(All_In_with_Chris_Hayes + Hannity + Jimmy_Kimmel_Live + The_Kelly_File + The_Nightly_Show_with_Larry_Wilmore + Today + Anderson_Cooper_Three_Hundred_and_Sixty + CBS_This_Morning + Hardball_with_Chris_Matthews + On_the_Record_with_Greta_Van_Susteren + The_Rachel_Maddow_Show + Good_Morning_America + Nancy_Grace + Erin_Burnett_OutFront + The_O_Reilly_Factor) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Entertainment_or_Opinion_Political_News_Programs_Moded_vote_grouped_lm <- lm(vote ~Entertainment_or_Opinion_Political_News_Programs_Moded_vote_grouped , data = anes_clean)

summary(Entertainment_or_Opinion_Political_News_Programs_Moded_vote_grouped_lm)

coefplot::coefplot(Entertainment_or_Opinion_Political_News_Programs_Moded_vote_grouped_lm)

```

 ### Regression for Entertainment Programs that are Expressly Political and Turnout Grouped ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Entertainment_Programs_that_are_Expressly_Political_Moded_vote_grouped <- as.data.frame (House_of_Cards + Game_of_Thrones + Madam_Secretary + Scandal + Alpha_House) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Entertainment_Programs_that_are_Expressly_Political_Moded_vote_grouped_lm <- lm (vote ~ Entertainment_Programs_that_are_Expressly_Political_Moded_vote_grouped , data = anes_clean)

summary(Entertainment_Programs_that_are_Expressly_Political_Moded_vote_grouped_lm)

coefplot::coefplot(Entertainment_Programs_that_are_Expressly_Political_Moded_vote_grouped_lm)

```

### Regresion for Entertainment Programs that are not Expressly Political but Focus on a Salient Political issue and Turnout Grouped ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_grouped <- as.data.frame(The_Blacklist + Criminal_Minds + NCIS + Scorpion + Judge_Judy + Daredevil + Blue_bloods + Hawaii_Five_O) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_grouped_lm <- lm(vote ~ Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_grouped, data = anes_clean)

summary(Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_grouped_lm)

coefplot::coefplot(Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_grouped_lm)

```

### Regresion for Entertainment Programs with little to No Political Content and Turnout Grouped ###

```{r}
clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_grouped <- as.data.frame(Empire + Modern_Family + Sunday_Night_Football + The_Simpsons + Dancing_with_the_Stars + Shark_Tank + The_Voice + Conan + The_Big_Bang_Theory + The_Tonight_Show_Starring_Jimmy_Fallon) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_grouped_lm <- lm(vote ~ Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_grouped , data = anes_clean)

summary(Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_grouped_lm)

coefplot::coefplot(Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_grouped_lm)

```

### Regression for All Variables and Turnout Grouped ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))


All_Variables_Moded_vote_grouped <- as.data.frame(Twenty_Twenty + CBS_Evening_News_with_Scott_Pelley + Sixty_Minutes + Face_the_Nation + Meet_the_Press + NBC_Nightly_News_with_Lester_Holt + ABC_World_News_with_David_Muir + Dateline_NBC + PBS_News_Hour + All_In_with_Chris_Hayes + Hannity + Jimmy_Kimmel_Live + The_Kelly_File + The_Nightly_Show_with_Larry_Wilmore + Today + Anderson_Cooper_Three_Hundred_and_Sixty + CBS_This_Morning + Hardball_with_Chris_Matthews + On_the_Record_with_Greta_Van_Susteren + The_Rachel_Maddow_Show + Good_Morning_America + Nancy_Grace + Erin_Burnett_OutFront + The_O_Reilly_Factor + House_of_Cards + Game_of_Thrones + Madam_Secretary + Scandal + Alpha_House + The_Blacklist + Criminal_Minds + NCIS + Scorpion + Judge_Judy + Daredevil + Blue_bloods + Hawaii_Five_O + Empire + Modern_Family + Sunday_Night_Football +  The_Simpsons + Dancing_with_the_Stars + Shark_Tank + The_Voice + Conan + The_Big_Bang_Theory + The_Tonight_Show_Starring_Jimmy_Fallon) %>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()

All_Variables_Moded_vote_grouped_lm <- lm(vote ~ All_Variables_Moded_vote_grouped ,  data = anes_clean)

summary(All_Variables_Moded_vote_grouped_lm)

coefplot::coefplot (All_Variables_Moded_vote_grouped_lm)
```

### Regression for All Grouped Variables Grouped and Turnout ###

```{r}

clean <- function(x){ifelse (x < 0, NA, x)}
anes_clean <- anes16 %>%
    mutate(across (everything(), clean)) %>%

mutate(vote = case_when(
V162034 == 2 ~ 0,
V162034 == 1 ~ 1))

All_Variables_Moded_vote_grouped_grouped <- as.data.frame(
Traditional_Political_News_Programs_Moded_vote_grouped + 
Entertainment_or_Opinion_Political_News_Programs_Moded_vote_grouped +
Entertainment_Programs_that_are_Expressly_Political_Moded_vote_grouped + 
Entertainment_Programs_that_are_not_Expressly_Political_but_Focus_on_a_salient_Political_issue_Moded_vote_grouped +
Entertainment_Programs_with_little_to_No_Political_Content_moded_vote_grouped 
)%>%
rename(count = 1) %>%
    mutate(count = ifelse(count <0, NA, count)) %>% unlist()


All_Variables_Moded_vote_grouped_grouped_lm <- lm(vote ~ All_Variables_Moded_vote_grouped_grouped , data = anes_clean)

summary (All_Variables_Moded_vote_grouped_grouped_lm)

coefplot::coefplot (All_Variables_Moded_vote_grouped_grouped_lm)

```

As you can see by running the code only 4 shows had a significant effect on turnout at the 5% level, Twenty-Twenty, Scandal, Conan, and the Big Bang Theory. Furthermore of the 4 shows 2 were apolitical which went against my prediction. As it stands I can only speculate on why these shows had an effect and the others did not. My initial thoughts have to do with the amount of advertising dollars that campaigns put towards these shows, I will have to follow up at some point and look at the effect on turnout per dollar per person each show has, but that will have to be for another project down the line. 

This project is a work in progress and is far from complete. My Partner and I still need to go in and control for other variables such as education, political interest etc. We also have to clean up the code as it is a bit messy and the formatting on many of the plots are not nice to look at.

In my follow up post I will be going over the effect that these shows had on registration in 2016.
