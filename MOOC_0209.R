setwd("/Users/vivi/Documents/04-UMD/09-GA/04-research/02-data/csv")
st <- read.csv("Student_Grand_Data.csv", header=TRUE, stringsAsFactors = FALSE)
micro <- read.csv("Micromasters Full Data 12-5-2019.csv", header=TRUE, stringsAsFactors = FALSE)

View(micro)
unique(micro$Mode)
table(micro$Status)

names(st)
summary(st)
table(st)

View(st)

table(st$Micromaster.Course.Count)


#clean EMPTY COLUMNS
st.re <-  subset(st, select= -c(Institution.Filter, Micromaster.Completion.Flag, Number.of.Records, institution,
                                Micromaster.Course.Count, Micromasters.Program, City))
View(st.re)


#ORDER BY USER ID AND COURSE NAME
st.re.or <- st.re[order(st.re$User.Id, st.re$Display.Name), ]
View(st.re.or)
table(st.re.or$Completion.Date)


####register more than one time(didn't complete the course, and never complete the course)####
length(st.re.or$User.Id[table(st.re.or$User.Id) == 7])
length(st.re.or$User.Id[table(st.re.or$User.Id) >= 7])
#117 have exactly 7 courses registered
#108 have more than 7 courses registered
length(st.re.or$User.Id[table(st.re.or$User.Id) < 7])
length(st.re.or$User.Id[table(st.re.or$User.Id) < 6])
length(st.re.or$User.Id[table(st.re.or$User.Id)])
table(st.re.or$User.Id)


View(subset(st.re.or, st.re.or$User.Id == 912198))
View(subset(st.re.or, st.re.or$User.Id == 17919735))


# used user id to count courses taken
temp <- table(st.re.or$User.Id)
temp<-data.frame(temp)
colnames(temp) <- c("User.Id","visit_courses")
merge.st <- merge(temp, st.re.or, by="User.Id")

table(st.re.or$User.Id)

View(merge.st)

length( merge.st$visit_courses> 7 )
sum( merge.st$visit_courses > 7 )

merge.st$Completion.Date[merge.st$Completion.Date == ""] <- NA

#### add new visit####
merge.st$newvisit <- 0
merge.st$newvisit[merge.st$visit_courses == 1] <- 1

merge.st$newvisit <- ifelse(merge.st$visit_courses==1, 1, 0)

length(merge.st$newvisit)
mutate(merge.st, newvisit=ifelse(merge.st$visit_courses==1, 1, 0))


####subset verified obs 1656/61581 course level, verified####
merge.st.v <- merge.st[merge.st$Enrollment.Track == "verified",]
table(merge.st.v$Enrollment.Track)
merge.st.v$Enrollment.Track <-factor(merge.st.v$Enrollment.Track)
str(merge.st.v$Enrollment.Track)

nrow(merge.st.v)
names(merge.st.v)




####add no of verified courses (track)####
merge.st.v$temp <-1
v.agg <- aggregate(merge.st.v$temp, by=list(merge.st.v$User.Id), FUN=sum, na.rm=T)
v.agg
v.agg <- data.frame(v.agg)
names(v.agg) <- c("User.Id","verified_courses")
merge.st.c <- merge(merge.st, v.agg, by="User.Id", all=T)
View(merge.st.c)

#verified table
table(merge.st.c$verified_courses)



####total 49796 users, 842 verified####
length(unique(merge.st.c$User.Id))
v.user <- merge.st.c[merge.st.c$verified_courses == NA, ]
vuser <- merge.st.c[complete.cases(merge.st.c$verified_courses)==T,]
nrow(vuser)
View(vuser)
length(unique(vuser$User.Id))
unique(vuser$User.Id)





#DATA EXPLORER
install.packages(("DataExplorer"))
library(DataExplorer)
DataExplorer::create_report(merge.st)


devtools::install_github("alastairrushworth/inspectdf")

install.packages("inspectdf")
library(inspectdf)

install.packages("devtools")
library(devtools)

inspect_types(cigarate, show_plot = TRUE)


####LAST COURSE_incomplete####
merge.st <- merge.st[order(merge.st$User.Id, merge.st$Display.Name), ]

merge.st$temp <-1
str(merge.st)
agg1 <- aggregate(merge.st$temp, by=list(merge.st$visit_courses,merge.st$Display.Name),FUN=sum, na.rm=T )
agg2 <- aggregate(merge.st$temp, by=list(merge.st$visit_courses,merge.st$Course.Success.Flag),FUN=sum, na.rm=T )
agg2
agg1


course12<-merge.st[merge.st$visit_courses== 12,]
View(course12)

ggplot2::aes()
a<-merge.st[merge.st$visit_courses ==2,]
a



#figure out why flag=false, it means not passing
sum(merge.st$Course.Success.Flag =="FALSE", na.rm=T)
View(merge.st[4736,])
which(merge.st$Course.Success.Flag =="FALSE")


#how many pass/ out of attempt
#last-course column(for taking over 2 verified courses), percentage of different courses
#when is the time they registered for verified course(who verified 7 courses(decision at first))



merge.st$Course.Success.Flag =="FALSE"


 <- ifelse(merge.st$Course.Success.Flag=="TRUE", 1,0)
unique(merge.st$Completion.Date)


agg2
agg1
merge.st$Display.Name<-factor(merge.st$Display.Name)

#micromaster.complet.course????
#10127437 3 completed class, but one didn't attempt to take final and no success flag???




####plot####
plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
     main = "rpois(100, lambda = 5)")

plot(merge.st.c$visit_courses, merge.st.c$User.Id, type = "h", col = "blue", lwd = 10,
     main = "courses taken for individual user")
library(ggplot2)

qplot(visit_courses, data=merge.st.c, color="blue")




#train and test

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]






#DATA EXPLORER
install.packages("data.table")
library(data.table)
transpose(temp)
typeof(temp)
table(st.re.or$User.Id)
table(st.re.or$User.Id) ==1


nrow(subset(st.re, User.Id==)

st.re$User.Id != 

table(st.re$Display.Name)



View(st$City)

table(st$Micromaster.Completed.Courses)
table(st$Course.Success.Flag)
table(st$Micromaster.Completion.Flag)#
table(st$institution)#
sum(is.na(st$Micromaster.Completion.Flag))
table(st$Number.of.Records)#NA
table(st$Micromaster.Course.Count)#NA
table(st$Micromasters.Program)#NA
table(st$City)#NA
table(st$Completion.Date)
table(st$Country)
table(st$Course.End)
table(st$Course.Id)
table(st$Course.Start)
table(st$Display.Name)
table(st$Staff.Flag)
table(st$Enrollment.Status)
table(st$Certificate.Type) 

sum((st$Percent.Grade==st$Grade)=="TRUE")
a<- st$Percent.Grade==st$Grade
sum(a == "TRUE")
as.factor(a)
a




#when percent.grade==NA, Grade might==0 ?!
st$Grade[717]
dim(st$Percent.Grade)


