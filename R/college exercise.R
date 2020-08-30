library(dplyr)
library(DBI)
con <- dbConnect(odbc::odbc(), "college", timeout = 10)
Classrooms<-dbGetQuery(con,'SELECT * FROM "college"."dbo"."Classrooms$"')
Courses<-dbGetQuery(con,'SELECT * FROM "college"."dbo"."Courses$"')
Departments<-dbGetQuery(con,'SELECT * FROM "college"."dbo"."Departments$"')
Students<-dbGetQuery(con,'SELECT * FROM "college"."dbo"."Students$"')
Teachers<-dbGetQuery(con,'SELECT * FROM "college"."dbo"."Teachers$"')

data_college <- full_join(Classrooms,Courses,by="CourseId")
data_college <- full_join(Departments,data_college,by
                          = c("DepartmentId"="DepartmentID"))
data_college <- full_join(data_college,Teachers, by= "TeacherId")
data_college<- full_join(data_college,Students,by ="StudentId")
data_college <- as_tibble(data_college)

#data_college<- unique.data.frame(data_college)
#distinct(data_college)

data_college_sml<-data_college%>% filter(!is.na(data_college))
#data_college_sml%>% tally()
View(data_college_sml)

#a.
a<-data_college_sml %>% 
  group_by(DepartmentName)%>%
  count(StudentId) %>% summarise(students=n())

#b. 
b<-data_college_sml %>% filter(DepartmentId==1) %>% 
  group_by(CourseName) %>% count(StudentId) %>%
  summarise(students=n())


total_sum <-data_college_sml %>% filter(DepartmentId==1) %>% 
        group_by(DepartmentName) %>% count(StudentId) %>% 
        summarise(Total=n())

print(b)
print(total_sum)
#c.

c<-data_college_sml%>% filter(DepartmentName=="Science")%>%
  group_by(CourseName)%>% count()

c$Classize = "Big"
c$Classize[c["n"]<22] <- "Small"

c%>%group_by(Classize)%>% tally()%>% rename(num_Classrooms=n)

#d.

Students%>%group_by(Gender)%>% count()%>% rename(num_Students=n)

#e.
Total<-data_college_sml%>%group_by(CourseName)%>%count()

crsbygndr<-data_college_sml%>%group_by(CourseName,CourseId,Gender.y)%>%
  count() 

e<-left_join(crsbygndr,Total,by="CourseName")

e$Gndr_stu_percent=(e$n.x/e$n.y)*100

e<-e%>%filter(Gndr_stu_percent>70)%>%na.omit(e)%>%
  select(CourseId,CourseName,Gender.y,Gndr_stu_percent)
e
#f.
Total_students<-data_college_sml%>%group_by(DepartmentName)%>%
  distinct(StudentId)%>%tally() 
Students_over80<-data_college_sml%>% filter(degree>80.0)%>%
  group_by(DepartmentName)%>%distinct(StudentId)%>%tally()
f<-inner_join(Total_students,Students_over80,by="DepartmentName")

f$Student_over_80_percent=(f$n.y / f$n.x)*100.0
f%>%select(DepartmentName,n.y,n.x,Student_over_80_percent)%>% 
  rename(Totalstudents=n.x,students_80=n.y)
#g.

Students_less60<-data_college_sml%>% filter(degree<60.0)%>%
  group_by(DepartmentName)%>%distinct(StudentId)%>%tally()

g<-inner_join(Total_students,Students_less60,by="DepartmentName")
g$Student_less_60_percent=(g$n.y / g$n.x)*100.0
g%>%select(DepartmentName,n.y,n.x,Student_less_60_percent)%>% 
  rename(Totalstudents=n.x,students_60=n.y)
#h.

data_college_sml$Teacher<- paste(data_college_sml$FirstName.x, data_college_sml$LastName.x, sep='')

h<-data_college_sml %>% select(Teacher,degree)%>%group_by(Teacher)%>% 
  summarize(degree_mean=mean(degree,na.rm = T))%>%arrange(desc(degree_mean))%>%
  na.omit()

i<-data_college_sml%>%group_by(CourseId,CourseName,DepartmentName,Teacher)%>% 
  distinct(StudentId)%>% count()

View(i)

#j.
j<-full_join(Students,Classrooms,by="StudentId")
j<-full_join(j, Courses, by= "CourseId")
df_cnt_crs<-j%>%group_by(StudentId,FirstName,LastName)%>% distinct(CourseId)%>% count()

df_English <-j %>%filter(j$DepartmentID==1)
df_Mean_English<-df_English %>% group_by(StudentId)%>%summarize(English=mean(degree,na.rm = T))
df_j<-full_join(df_j,df_Mean_English, by="StudentId")

df_Arts<- j %>% filter(j$DepartmentID==3)
df_Mean_Arts<- df_Arts%>%group_by(StudentId) %>%summarize(Arts=mean(degree,na.rm = T))
df_j<- full_join(df_j,df_Mean_Arts, by ="StudentId")

df_Science<- j %>% filter(j$DepartmentID==2)
df_Mean_Science<- df_Science%>%group_by(StudentId) %>%summarize(Science=mean(degree,na.rm = T))
df_j<- full_join(df_j,df_Mean_Science, by ="StudentId")

df_Sport<- j %>% filter(j$DepartmentID==4)
df_Mean_Sport<- df_Sport%>%group_by(StudentId) %>%summarize(Sport=mean(degree,na.rm = T))
df_j<- full_join(df_j,df_Mean_Sport, by ="StudentId")


df_Mean_General <- j%>% group_by(StudentId)%>% summarize(General=mean(degree,na.rm = T))
df_j=full_join(df_j,df_Mean_General,by="StudentId")
View(df_j)

#dbDisconnect(con)
