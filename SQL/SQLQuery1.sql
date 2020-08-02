--a. 
use college
select count(d.StudentId) as num_students, b.DepartmentID,a.DepartmentName
from Departments$ as A
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D
on d.StudentId=c.StudentId
group by b.DepartmentID,a.DepartmentName

--b.

select count(distinct d.StudentId) as num_students,a.DepartmentName, b.CourseName, b.CourseId, e.TeacherId
from Departments$ as A
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D
on d.StudentId=c.StudentId
inner join Teachers$ as E 
on e.TeacherId=b.TeacherId
where b.CourseName like '%english%'
group by b.DepartmentID,a.DepartmentName, b.CourseName, b.CourseId, e.TeacherId

--c.
select count(distinct d.StudentId) as num_students, b.CourseName, 
case when (count(distinct d.StudentId)<22) then ('small') else('big') end as 'size_class'
from Departments$ as A
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D
on d.StudentId=c.StudentId
inner join Teachers$ as E 
on e.TeacherId=b.TeacherId
where b.DepartmentID=2
group by b.CourseName

--d.

select count(gender)as num_students,Gender from Students$
group by Gender

--e.

select b.CourseId, b.CourseName,d.Gender,count(d.StudentId)as num_students,count(d.studentid)*100/cast(sum(count(d.studentid)) over(partition by b.courseid)as decimal(2,0))as percentage
into #coursegender_percentage
from Departments$ as A
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D
on d.StudentId=c.StudentId
inner join Teachers$ as E 
on e.TeacherId=b.TeacherId
group by  b.courseid,b.courseName, d.Gender
order by b.CourseId

select * from #coursegender_percentage
where percentage>70

--f
select a.DepartmentName, count(distinct d.StudentId) as num_students, sum(case when c.degree>80 then 1 else null end) as stu_deg_over80,cast(sum(case when c.degree>80 then 1 else null end) *100.0 /count(distinct d.StudentId)as decimal (18,2))as percentage
from Departments$ as A                   
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D
on d.StudentId=c.StudentId
inner join Teachers$ as E 
on e.TeacherId=b.TeacherId

group by a.DepartmentName



--g
select a.DepartmentName, count(distinct d.studentid) as num_students, sum(case when c.degree<60 then 1 else null end)as stu_deg_less60,cast(sum(case when c.degree<60 then 1 else null end) *100.0 /count(distinct d.StudentId)as decimal (18,2))as percentage
from Departments$ as A
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D
on d.StudentId=c.StudentId
inner join Teachers$ as E 
on e.TeacherId=b.TeacherId

group by a.DepartmentName

--h
select e.FirstName,e.LastName,e.TeacherId,avg(C.degree)as degree_avg
from Departments$ as A
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D
on d.StudentId=c.StudentId
inner join Teachers$ as E 
on e.TeacherId=b.TeacherId
group by e.FirstName,e.LastName,e.TeacherId
order by degree_avg desc


--view

--a
create view [datacourse] as
select b.CourseName,a.DepartmentName,e.FirstName,e.LastName,count(d.studentid)as student_total
from Departments$ as A
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D
on d.StudentId=c.StudentId
inner join Teachers$ as E 
on e.TeacherId=b.TeacherId
group by b.CourseName,a.DepartmentName,e.FirstName,e.LastName


--b
create view [data students] as
select d.FirstName,d.LastName,a.DepartmentName,count(b.CourseId) as num_courses, cast(avg(c.degree)as decimal)as avg_degree,cast(AVG(c.degree) OVER(PARTITION BY d.studentid)as decimal) AS Avgtotaldegree
from Departments$ as A
inner join Courses$ as B
on A.DepartmentId= B.DepartmentID
inner join Classrooms$ as C
on C.CourseId= B.CourseId
inner join Students$ as D 
on d.StudentId=c.StudentId
inner join Teachers$ as E 
on e.TeacherId=b.TeacherId
group by a.DepartmentName,d.FirstName,d.LastName, d.StudentId,c.degree,b.CourseName




