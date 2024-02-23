datapath = 'C:/Users/baiji/Documents/MSc Econometrics/Master Thesis Tail Risk RF Code/DGPs/Application/'

monthly_path = paste0(datapath, "MONTHLY_RAW/")

########## Macro econonomic indicators

calcMonthlyGrowth <- function(index){
  index = as.numeric(index)
  res = rep(NA, length(index))
  for (i in 2:length(index)){
    res[i] = ((index[i]-index[i-1])/index[i-1])*100
  }
  return(res)
}
calcYoYGrowth <- function(index){
  index = as.numeric(index)
  res = rep(NA, length(index))
  for (i in 13:length(index)){
    res[i] = ((index[i]-index[i-12])/index[i-12])*100
  }
  return(res)
}

# Function for reading data, calculating monthly/yoy growth rates and converts monthly frequency to daily
read_and_process_data <- function(file_path, column_name) {
  data = read.csv(file = paste0(monthly_path, file_path))
  data$Date = as.Date(data$Date) + months(1)
  data$monthly_growth = calcMonthlyGrowth(data[[column_name]])
  data$yoy_growth = calcYoYGrowth(data[[column_name]])
  data = na.omit(data)
  dates = seq(from = min(as.Date(data$Date)),
               to = ceiling_date(max(as.Date(data$Date)), "month") - days(1),
               by = "1 days")
  data = data.frame(dates,
                     as.numeric(setNames(data$monthly_growth, data$Date)[format(dates, format = "%Y-%m-01")]),
                     as.numeric(setNames(data$yoy_growth, data$Date)[format(dates, format = "%Y-%m-01")]))
  colnames(data) = c('Date',  paste0(column_name, '_monthly'), paste0(column_name, '_yoy') )
  return(data)
}

# capacity utilization data
capacity = read_and_process_data('CAPACITYUTIL.csv', 'capacity')

# energy index data
energy = read_and_process_data('ENERGYINDEX.csv', 'energy')

# housing index data
housing = read_and_process_data('HOUSINGNEW.csv', 'housing')

# unemployment rate
unemployment = read_and_process_data('UNEMPRATE.csv', 'unemp')

# manufacturers orders
manufac = read_and_process_data('MANUFACTURERORDERS.csv', 'manufac')

# M2 Us money supply
m2 = read_and_process_data('M2.csv', 'm2')

# industrial production index
indpro = read_and_process_data('INDPRO_totalindex.csv', 'indpro')

#cpi monthly growth
cpi_monthlygrowth = read.csv(file=paste0(monthly_path, 'CPI_monthlygrowth.csv'))
cpi_monthlygrowth$Date = as.Date(cpi_monthlygrowth$Date) + months(1)
cpi_monthlygrowth_Dates = seq(from = min(as.Date(cpi_monthlygrowth$Date)),
                        to = ceiling_date(max(as.Date(cpi_monthlygrowth$Date)), "month") - days(1),
                        by = "1 days")
cpi_monthlygrowth = data.frame('Date' = cpi_monthlygrowth_Dates,
                               'cpi_monthly'=as.numeric(setNames(cpi_monthlygrowth$cpi_monthlygrowth, cpi_monthlygrowth$Date)[format(cpi_monthlygrowth_Dates, format = "%Y-%m-01")]))
# cpi yoy growth
cpi_yoygrowth = read.csv(file=paste0(monthly_path, 'CPI_yoygrowth.csv'))
cpi_yoygrowth$Date = as.Date(cpi_yoygrowth$Date) + months(1)
cpi_yoygrowth_Dates = seq(from = min(as.Date(cpi_yoygrowth$Date)),
                              to = ceiling_date(max(as.Date(cpi_yoygrowth$Date)), "month") - days(1),
                              by = "1 days")
cpi_yoygrowth = data.frame('Date' = cpi_yoygrowth_Dates,
                               'cpi_yoy'=as.numeric(setNames(cpi_yoygrowth$cpi_yoygrowth, cpi_yoygrowth$Date)[format(cpi_yoygrowth_Dates, format = "%Y-%m-01")]))
