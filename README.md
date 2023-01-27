
# Teaching to expand student inclusion of societal concepts in science 

Paula E. Adams*, Emily Driessen, Enya Granados, Penny Ragland, Jeremiah A. Henning, Abby E. Beatty & Cissy J. Ballen

 *Corresponding Author: pea0013@auburn.edu 

## Abstract:
""

## Project Summary:
Supplementary Materials:





# Statistical and Data Visualization Code

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#clear environment
rm(list=ls())

#Node Data
datum <- read.csv("/Users/Owner/Library/CloudStorage/Box-Box/IA Integration_SemesterLong/R analysis/Final Checked Data/Data for R Analysis.csv")

#Society Topic Data
SocietyTopics <- read.csv('/Users/Owner/Library/CloudStorage/Box-Box/IA Integration_SemesterLong/R analysis/Societal Nodes Coding - Paula/IA_Topics_R_DRAFT.csv', sep=",", header=TRUE)

#Other Category Data
OtherCategory <- read.csv('/Users/Owner/Library/CloudStorage/Box-Box/IA Integration_SemesterLong/R analysis/Societal Nodes Coding - Paula/9_other_R_draft.csv', sep=",", header=TRUE)
```
### Load Libraries 
```{r}
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(knitr)
```

### Clean data
```{r}
#Node Data
datum<-datum %>%
  select(student, Nodes, Bionodes, Societynodes, Section)
datum$Section[datum$Section == "Free"] <- "Traditional"
datum$Section[datum$Section == "IA"] <- "Ideol. Aware"
datum$Section <- as.factor(datum$Section)
datum$Section <- factor(datum$Section, levels=c('Traditional', 'Ideol. Aware'))

#Society Topic Data
colnames(SocietyTopics)[3] <- "Section"
SocietyTopics$Section[SocietyTopics$Section == "Free"] <- "Traditional"
SocietyTopics$Section[SocietyTopics$Section == "IA"] <- "Ideol. Aware"
SocietyTopics$Section <- as.factor(SocietyTopics$Section)
SocietyTopics$Section <- factor(SocietyTopics$Section, levels=c('Traditional', 'Ideol. Aware'))
SocietyTopics$IA.Concept <- as.factor(as.character(SocietyTopics$IA.Concept))
SocietyTopics$Student <- as.factor(SocietyTopics$Student)

#Other Category Data
colnames(OtherCategory)[2] <- "Section"
OtherCategory.1 <- pivot_longer(OtherCategory,3:7,names_to="Category",values_to="count",values_drop_na=TRUE)
OtherCategory <- select(OtherCategory.1,c(Student,Section,Category))
OtherCategory$Section[OtherCategory$Section == "Free"] <- "Traditional"
OtherCategory$Section[OtherCategory$Section == "IA"] <- "Ideol. Aware"
OtherCategory$Category[OtherCategory$Category == "Societal.factors.effecting.science"] <- "Societal factors affecting science"
OtherCategory$Category[OtherCategory$Category == "Problems.in.Science"] <- "Problems in science"
OtherCategory$Category[OtherCategory$Category == "Faulty.information.about.science"] <- "Faulty information about science"
OtherCategory$Category[OtherCategory$Category == "Public.Science.Experience"] <- "Public science experience"
OtherCategory$Category[OtherCategory$Category == "Distrust.in.science"] <- "Distrust in science"
OtherCategory$Section <- as.factor(OtherCategory$Section)
OtherCategory$Section <- factor(OtherCategory$Section, levels=c('Traditional', 'Ideol. Aware'))
OtherCategory$Category <- as.factor(OtherCategory$Category)
OtherCategory$Student <- as.factor(OtherCategory$Student)
```

# Total Nodes per Student Analyses
### Graph of number of overall nodes - density and boxplot
```{r}
#Density Plot
NodeCount<-ggplot(datum, aes(x = Nodes, fill = fct_rev(Section))) +
  scale_fill_manual(values=c("#6A6599","#79AF97"))+
  geom_density(alpha=0.7) +
  ggtitle("Total") +
  theme_classic() +
  scale_y_continuous(name = "Proportion of C. Maps",limits=c(0,0.020)) +
  scale_x_continuous(name = "") +
  theme(legend.position="none",axis.title = element_text(size = 9),plot.title = element_text(hjust = 0.5,face="bold"))

#Boxplot
Nodebox<-ggplot(datum, aes(x=Section, y=Nodes, fill=fct_rev(Section))) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("#6A6599","#79AF97"))+
  scale_y_continuous(name="Nodes per Concept Map",breaks=c(0, 10, 20, 30, 40, 50 , 60, 70, 80, 90, 100), limits=c(0, 100))+
  theme_classic() +
  theme(panel.grid = element_blank()) +
  scale_x_discrete(name = "") + theme(legend.position="none",axis.title = element_text(size = 9))

```
### Graph of number of biology nodes - density and boxplot
```{r}
#Density Plot
BionodeCount<-ggplot(datum, aes(x = Bionodes, fill = fct_rev(Section))) +
  scale_fill_manual(values=c("#6A6599","#79AF97"))+
  ggtitle("Biology") +
  geom_density(alpha=0.7) +
  theme_classic() +
  scale_y_continuous(name = "",limits=c(0,0.02)) +
  scale_x_continuous(name = "# of Nodes per Concept Map") +
  theme(legend.position="none",axis.title = element_text(size = 9),plot.title = element_text(hjust = 0.5,face="bold"))

# Boxplot
Biologybox<-ggplot(datum, aes(x=Section, y=Bionodes, fill=fct_rev(Section))) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("#6A6599","#79AF97"))+
  scale_y_continuous(name="",breaks=c(0, 10, 20, 30, 40, 50 , 60, 70, 80, 90, 100), limits=c(0, 100))+
  theme_classic() + 
  theme(panel.grid = element_blank())+ 
  scale_x_discrete(name="") +  
  theme(legend.position="none")
```
### Graph of number of society nodes - density and boxplot
```{r}
#Density Plot
SocietyCount<-ggplot(datum, aes(x = Societynodes, fill = fct_rev(Section))) +
  scale_fill_manual(values=c("#6A6599","#79AF97"))+
  geom_density(alpha=0.7) +
  theme_classic() +
  theme(legend.position =c(0.75, 0.6),legend.box.background = element_rect(color="black", size=1),plot.title = element_text(hjust = 0.5,face="bold"),legend.title=element_text(size=9),legend.text = element_text(size=6),legend.key.size = unit(.4, 'cm'))+
  ggtitle("Society") +
  scale_y_continuous(name = "",limits=c(0, 0.25)) +
  scale_x_continuous(name = "") +
       labs(fill='Section') 

#Boxplot
Societybox<-ggplot(datum, aes(x=Section, y=Societynodes, fill=fct_rev(Section))) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("#6A6599","#79AF97")) +
  scale_y_continuous(name="",breaks=c(0, 5, 10, 15, 20, 25, 30), limits=c(0, 30))+
  theme_classic() + 
 theme(panel.grid = element_blank(),legend.position="none") + 
  scale_x_discrete(name="") +
  labs(fill='Section')
```
### Combine Node Count Plots
plot_grid from the cowplot package
```{r}
Combinedgraph <- plot_grid(NodeCount, BionodeCount, SocietyCount, Nodebox, Biologybox, Societybox , ncol=3, labels="AUTO",rel_heights =c(2,3)) #rel_widths = c(2,2, 3)

Combinedgraph
```
### Average number of nodes calculations (total, biology, society)
```{r}
Average_numNodes_perStudent <- datum %>% group_by(Section) %>% summarize(average=mean(Nodes)) %>% data.frame # average
knitr::kable(Average_numNodes_perStudent,format = "html", caption = "Average Number of Nodes per Concept Map", table.attr = "style='width:20%;'")

Average_numBioNodes_perStudent <- datum %>% group_by(Section) %>% summarize(average=mean(Bionodes)) %>% data.frame # average
knitr::kable(Average_numBioNodes_perStudent,format = "html", caption = "Average Number of Biology Nodes per Concept Map", table.attr = "style='width:20%;'")

Average_numSocietyNodes_perStudent <- datum %>% group_by(Section) %>% summarize(average=mean(Societynodes)) %>% data.frame # average
knitr::kable(Average_numSocietyNodes_perStudent,format = "html", caption = "Average Number of Society Nodes per Concept Map", table.attr = "style='width:20%;'")
```
### Linear model to estimate differences in the number of overall nodes between sections
```{r}
overall<-lm(Nodes~Section, data=datum, na.action=na.exclude) 
summary(overall)
```
### Linear model to estimate differences in the number of biology nodes between sections
```{r}
bionodes<-lm(Bionodes~Section, data=datum, na.action=na.exclude)
summary(bionodes)
```
### Linear model to estimate differences in the number of societal nodes between sections
```{r}
Societalnodes<-lm(Societynodes~Section, data=datum, na.action=na.exclude)
summary(Societalnodes)
```

# Societal Nodes by Category Code
### Count students per Class
```{r}
FreeStudents <- subset(SocietyTopics,Section=="Traditional")
FreeStudentsCount <- as.numeric(length(unique(FreeStudents$Initials)))
IAStudents <- subset(SocietyTopics,Section=="Ideol. Aware")
IAStudentsCount <- as.numeric(length(unique(IAStudents$Initials)))
```
### Create Condensed Data
Combine duplicate categories for each student. 
Example student with 5 nodes coded 1, 2, 3, 1, 3 would be condensened to a student mentioning categories 1, 2, & 3. Useful because some students may have lots of nodes of the same category artifically inflating their societal nodes. However, this does lower the total count of nodes. 
```{r}
SocietyTopics_Condensed <- distinct(SocietyTopics)
```
## Diferences in number of Topics per student by class Section
### Create Counts by student
How many topics did each student say? Average number of topics per student
```{r}
Topic_perStudent <- (SocietyTopics_Condensed %>% group_by(Student,Section) %>% summarise(TotalperStudent = sum(!is.na(IA.Concept))) %>% data.frame )

Average_numTopics_perStudent <- Topic_perStudent %>% group_by(Section) %>% summarize(average=mean(TotalperStudent)) %>% data.frame # average

knitr::kable(Average_numTopics_perStudent,format = "html", caption = " Average Number of topics per Concept Map by Section", table.attr = "style='width:20%;'")
```
### Boxplot of societal topics per student  
```{r}
Topics_perstudentBox <- ggplot(Topic_perStudent, aes(x=Section, y=TotalperStudent,fill=Section)) + 
  geom_boxplot(alpha=0.65) +  theme_classic() + 
  scale_fill_manual(values=c("#79AF97","#6A6599")) +
  ggtitle("Society Topics per Concept Map") +
  theme(legend.position="none",
        axis.title = element_text(size = 13),
        axis.text.x= element_text(size=13),
        plot.title = element_text(size=13,hjust=.5,face="bold"))+
  labs(x="", y="Society Topics per Student") +
  annotate(geom="text", x=2.3, y=6.5, label="*",
              color="black",size=6)

Topics_perstudentBox
```
### Linear model to estimate differences in the number of societal Topics per student between sections
```{r}
topics_per_student_lm <-lm(TotalperStudent~Section, data=Topic_perStudent, na.action=na.exclude) 
summary(topics_per_student_lm)
```

## Differences in frequency of each topic by Class Section
### Create Concept Count Tables
Count how many students mention each category in both sections (IA vs Free) 
```{r}
Condensed_Counts <- count(SocietyTopics_Condensed,Section,IA.Concept,name="Total",.drop=FALSE)
```
### Percentage of Students mentioning each topic
```{r}
Condensed_Counts_Percents <- mutate(Condensed_Counts, Percent = case_when(Section=="Traditional" ~ 100*(Total/FreeStudentsCount), Section=="Ideol. Aware" ~ 100*(Total/IAStudentsCount))) ## Add percent of students using each IA category to the condensed version of Counts

Percent_Counts <- subset(Condensed_Counts_Percents,select=c(Section,IA.Concept,Percent))
Percent_Counts2 <-pivot_wider(Percent_Counts, names_from = "Section",values_from = "Percent")
knitr::kable(Percent_Counts2, caption = "Percent students mentioning each topic") 
```
### Figure for percent of students mentioning each topic at least once (condensed)
Percents plot grid for paper
(does not equal 100% because each student can mention multiple topics)
```{r}
Condensed_Counts_Percents$IA.Concept <- as.character(Condensed_Counts_Percents$IA.Concept)
Condensed_Counts_Percents[is.na((Condensed_Counts_Percents))] <- 0
Condensed_Counts_Percents$IA.Concept <- as.factor(Condensed_Counts_Percents$IA.Concept)

Condensed_Counts_Percents$IA.Concept <- factor(Condensed_Counts_Percents$IA.Concept, levels=c('0','7','5','3','1','2','9','4','6'),exclude = NULL)

IA_Condensed_PercentPlot_free <- ggplot(subset(Condensed_Counts_Percents, Section %in% "Traditional"), aes(IA.Concept,Percent,fill=IA.Concept)) + 
  geom_bar(color="black",position="dodge",stat="identity",alpha=0.7) +  theme_cowplot() + 
  scale_fill_manual(values=c("white","#79AF97","#79AF97","#79AF97","#79AF97","#79AF97","#79AF97","#79AF97","#79AF97")) +
  scale_x_discrete(name = "") + 
  theme(legend.position="none",
        plot.margin = margin(r = 0,t=0,b=5),        
        axis.text.x= element_text(size=11),
        axis.text.y= element_text(size=11),
        axis.title = element_text(size = 9))+
  scale_y_reverse(name="",breaks=c(100,50,0),limits=c(100,0))+
  coord_flip()

IA_Condensed_PercentPlot_IA <- ggplot(subset(Condensed_Counts_Percents, Section %in% "Ideol. Aware"), aes(IA.Concept,Percent,fill=IA.Concept)) + 
  geom_bar(color="black",position="dodge",stat="identity",alpha=0.7) +  theme_cowplot() + 
  scale_fill_manual(values=c("white","#6A6599","#6A6599","#6A6599","#6A6599","#6A6599","#6A6599","#6A6599","#6A6599")) +
  theme(legend.position="none",
        axis.line.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x= element_text(size=11),
        plot.margin = margin(l = 0, t=0,b=5), # Left margin 
        axis.title = element_text(size = 9))+
  scale_x_discrete(name=NULL)+
  guides(y="none")+
  scale_y_continuous(name="",breaks=c(0,50,100),limits=c(0, 100))+
  coord_flip()

percents_plot_row <- plot_grid(IA_Condensed_PercentPlot_free,IA_Condensed_PercentPlot_IA,rel_widths = c(1,.85))
title <- ggdraw() + 
  draw_label("Percent of Concept Maps Including a Topic",
    fontface = 'bold', x=0,hjust=0,size = 11.5) +
  theme(plot.margin = margin(l=20,t=0,b=0)) # add margin on the left of the drawing canvas,so title is aligned with left edge of first plot

Percents_PlotGrid <- plot_grid(title, percents_plot_row,ncol = 1, rel_heights = c(0.06, 1))   # rel_heights values control vertical title margins

Percents_PlotGrid
```

# Societal Nodes not aligning with Ideological Topics, aka "Other Category"
### Create Condensed Data
Combine duplicate categories for each student. 
Example student with 5 nodes coded 1, 2, 3, 1, 3 would be condensened to a student mentioning categories 1, 2, & 3. Useful because some students may have lots of nodes of the same category artifically inflating their societal nodes. However, this does lower the total count of nodes. 
```{r}
OtherCategory_Condensed <- distinct(OtherCategory)
```
### Count Mentions per student and convert to percentages
```{r}
condensed_counts <- count(OtherCategory_Condensed,Section,Category,name="Total",.drop=FALSE)

#Convert to percentages
Condensed_Counts_Percents <- mutate(condensed_counts, Percent = case_when(Section=="Traditional" ~ 100*(Total/FreeStudentsCount), Section=="Ideol. Aware" ~ 100*(Total/IAStudentsCount))) ## Add percent of students using each IA category to the condensed version of Counts

Percent_Counts <- subset(Condensed_Counts_Percents,select=c(Section,Category,Percent))
Percent_Counts2 <-pivot_wider(Percent_Counts, names_from = "Section",values_from = "Percent")
knitr::kable(Percent_Counts2, caption = "Percent students mentioning each topic") 
```
### Graph for Percent of Students mentioning each "Other" societal topic
```{r}
Percent_Counts$Category <- factor(Percent_Counts$Category, levels=c('Distrust in science','Public science experience','Societal factors affecting science','Problems in science','Faulty information about science'),exclude = NULL)

Other_free_percent <- ggplot(subset(Percent_Counts, Section %in% "Traditional"), aes(Category,Percent,fill=Category)) + 
  geom_bar(color="black",position="dodge",stat="identity",alpha=0.7) +  theme_cowplot() + 
  scale_fill_manual(values=c("#79AF97","#79AF97","#79AF97","#79AF97","#79AF97","#79AF97","#79AF97","#79AF97"),na.value="white") +
  theme(legend.position="none",
        axis.text.x= element_text(size=11),
        axis.text.y= element_text(size=11),
        plot.margin = margin(r = 0, t=0,b=0), # Left margin 
        axis.title = element_text(size = 9))+
  scale_x_discrete(name=NULL)+
  guides(y="none")+
  scale_y_reverse(name="",breaks=c(50,25,0),limits=c(50,0))+
  coord_flip()

Other_IA_percent <- ggplot(subset(Percent_Counts, Section %in% "Ideol. Aware"), aes(Category,Percent,fill=Category)) + 
  geom_bar(color="black",position="dodge",stat="identity",alpha=0.7) +  theme_cowplot() + 
  scale_fill_manual(values=c("#6A6599","#6A6599","#6A6599","#6A6599","#6A6599","#6A6599","#6A6599","#6A6599"),na.value="white") +
  theme(legend.position="none",
        axis.line.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x= element_text(size=11),
        plot.margin = margin(l = 0, t=0,b=0), # Left margin 
        axis.title = element_text(size = 9))+
  scale_x_discrete(name=NULL)+
  guides(y="none")+
  scale_y_continuous(name="",breaks=c(0,25,50),limits=c(0,50))+
  coord_flip()

total_plot_row <- plot_grid(Other_free_percent,Other_IA_percent)
title <- ggdraw() + 
  draw_label("Percent Mentions of Topic per Class",
    fontface = 'bold', x=0,hjust=0,size = 12) #+

other_plot <- plot_grid(total_plot_row,title,ncol = 1, rel_heights = c(1,0.06))   # rel_heights values control vertical title margins

other_plot  
```



