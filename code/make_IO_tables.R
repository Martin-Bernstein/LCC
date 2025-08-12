setwd(file.path('~','Dropbox-HarvardUniversity','Martin Bernstein','Fall 2025','Other','LCC'))

source(file.path('code','NIPA-utility-functions.R'))

years <- 1963:2023
for (year in years) {
  #Read data
  if(year <= 1996){
    m <- read_excel(file.path('data','NIPA','IO tables','Historical IO','IOUse_Before_Redefinitions_PRO_1963-1996_Summary.xlsx'),
                    sheet=as.character(year))
    colnames(m) <- m[5,]
    colcodes <- m[4,3:ncol(m)]
  }else{
    m <- read_excel(file.path('data','NIPA','IO tables','AllTablesIO','IOUse_Before_Redefinitions_PRO_1997-2023_Summary.xlsx'),
                    sheet=as.character(year))
    colnames(m) <- m[6,]
    colcodes <- m[5,3:ncol(m)]
  }
  
  #Extract codes for industries (columns)
  colcodes <- pivot_longer(colcodes, cols = 1:ncol(colcodes), names_to='industry',values_to='ind_code')
  setDT(colcodes)
  colcodes <- colcodes[!is.na(ind_code)]
  
  #Clean
  m <- m[7:nrow(m),]
  setDT(m)
  #Use this to build matrix
  build_matrix(m,colcodes)
}


#HERE: make clean data per year, join years.
# #Want:
# #One dt: industry, commodity, value, total_industry_output.
# #One dt: industry, total labor, total 
# 
# key <- m[!is.na(IOCode)&!is.na(Name),.(IOCode, Name)]
# 
# setnames(m, old = 'Name', new = 'purchases')
# setnames(m, old = 'IOCode', new = 'code_purchases')
# 
# 
# #Pivot longer and format
# dt <- setDT(pivot_longer(m,cols = 3:(ncol(m)-2),names_to = 'industry',values_to='value'))
# totals <- dt[purchases == 'Total Industry Output',.(industry,value)]
# names(totals)[2] <- c('total_industry_output')
# dt[,total_industry_output := totals[.SD,on=.(industry),x.total_industry_output]]
# 
# total_intermediates <- dt[purchases == 'Total Intermediate',.(industry,value)]
# names(total_intermediates)[2] <- c('total_intermediates')
# dt[,total_intermediate_spending := total_intermediates[.SD,on=.(industry),x.total_intermediates]]
# 
# dt <- dt[!is.na(code_purchases) & !is.na(purchases)]
# 
# dt[,code_industry := colcodes[.SD,on=.(industry),x.ind_code]]
# 
# #Remove final uses
# dt <- dt[!substr(code_industry,1,1)=='F']
# 
# 
# convert <- c('Total Final Uses (GDP)','Total Commodity Output','value','total_industry_output')
# dt[,(convert) := lapply(.SD, as.numeric),.SDcols = convert]
# 
# dt[,c('Total Final Uses (GDP)','Total Commodity Output') := NULL]
# 
# # ggplot(dt[purchases=='Compensation of employees'],aes(value/total_industry_output,industry))+
# #   geom_bar(stat='identity')
# 
# dt <- dt[,.( code_industry,industry,code_purchases,purchases,value,total_industry_output,total_intermediate_spending)]
# setorder(dt,code_industry,code_purchases)
# dt[is.na(value),value:=0]
# dt[,share := value / total_industry_output]
# 
# labor_shares <- dt[code_purchases %in% c('V001','V002','V003')]
# resids <- dt[code_purchases %in% c('Used','Other')]
# dt <- dt[!code_purchases %in% c('Used','Other','V001','V002','V003')]
# dt[,total_domestic_intermediates := sum(value),by=.(industry)]
# 
# 
# 
# 
# 
