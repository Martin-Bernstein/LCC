setwd(file.path('~','Dropbox-HarvardUniversity','Martin Bernstein','Fall 2025','Other','LCC'))

source(file.path('code','NIPA-utility-functions.R'))

dh <- fread(file.path('data','NIPA','income-employment-industry','avg_wages_1998-2023.csv'))
f <- clean_NIPA(dh,years=1998:2023,colname = 'avg_salary',use_deflator = T)

ggplot(f[level==2],aes(year,avg_salary))+
  geom_line()+
  facet_wrap(~industry)