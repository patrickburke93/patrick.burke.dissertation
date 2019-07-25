# patrick.burke.thesis
Listing of R code for thesis
# Clear Environment
rm(list=ls())
# load required libraries
library(lubridate)
library(PerformanceAnalytics)
# set working directory
# setwd("C:/Users/.....")
# file name 
#file_name = 'S&P Growth Data (R) .csv'
#file_name = 'S&P Value Data.(R)update.csv'
#file_name = 'Low Volatility S&P(R)Update.csv'
#file_name = 'S&P Momentum Data (R)amended.csv'
#
file_name = 'S&P Size Data- amended.csv'
# load data from csv
df_GD = read.csv(paste0('data/',file_name),skip = 1,header = T)
# load fund names
list_FundNames = names(read.csv(paste0('data/',file_name),nrows = 1,header = T))
# clean text
list_FundNames = gsub("\\.", "", list_FundNames)
fundNames = list_FundNames[!grepl('^X', list_FundNames)]
#--------------------------------------------------------------------------------------
# US Federal Funds Effective rate
# select RF data and drop empty records/Nulls
df_RF = na.omit(df_GD[,c(3,4)])
# convert date to appropriate format
df_RF[,1] = mdy(df_RF[,1])
# convert data to xts format
df_RF = as.xts(x = df_RF[,-1],order.by = df_RF[,1])
# set column name
colnames(df_RF)='RF'
#--------------------------------------------------------------------------------------
return_xts <- function(df=df_GD[,c(1,2)],fund_name='SP500',returns=F){
  df = na.omit(df)
  df[,1] = mdy(df[,1])
  df = as.xts(x = df[,-1],order.by = df[,1])
  colnames(df)=fund_name
  if(returns==T){
    df = Return.calculate(df)
    df = df[-1,]
  }
  return(df)
}
return_materics <- function(df_fund,RF,BM=NULL,fund_name){
  # df_fund - dataframe with (date,daily price,date,Fund Monthly AUM) columns
  # RF - risk free returns as xts
  # BM - benchmark as xts
  df_RF = RF 
  df_temp_returns = return_xts(df_fund[,c(1,2)],fund_name,returns = T)
  df_temp_returns = merge(df_temp_returns,df_RF,join = 'inner')
  sr_val = SharpeRatio( R = df_temp_returns[,1]
                        ,Rf = df_temp_returns[,2]
                        ,FUN = c("StdDev", "VaR"))
  so_val = SortinoRatio( R = df_temp_returns[,1]
                         ,MAR = 0)[1]
  # Various Value At Risk (VaR) Measures
  VaR_val = VaR(R = df_temp_returns[,1])[1]
  n_months_val = dim(df_temp_returns)[1]
  # Excess return
  excessR_val = mean(Return.excess(R = df_temp_returns[,1],Rf = df_temp_returns[,2]))
  # benchmark
  if(ncol(df_fund)<3){
    return(list(c(excessR_val,sr_val[1],
             sr_val[2],so_val,
             VaR_val,n_months_val,
             NA,NA)))
 # for benchmark - SP500
  df_BM = BM
  df_BM = merge(df_BM,df_RF,join = 'inner')
  df_BM = merge(df_BM,df_temp_returns,join = 'inner')
  df_BM = na.omit(df_BM)
  sr_val_bm = SharpeRatio( R = df_BM[,1]
                        ,Rf = df_BM[,2]
                        ,FUN = c("StdDev", "VaR"))
  so_val_bm = SortinoRatio( R = df_BM[,1]
                         ,MAR = 0)[1]
  # Various Value At Risk (VaR) Measures
  VaR_val_bm = VaR(R = df_BM[,1])[1]
  
  n_months_val_bm = dim(df_BM)[1]
  # Excess return
  excessR_val_bm = mean(Return.excess(R = df_BM[,1],Rf = df_BM[,2]))
  return( list(c(excessR_val,sr_val[1],
            sr_val[2],so_val,
            VaR_val,n_months_val,
            fund_flow,n_months_val_fund),
            c(excessR_val_bm,sr_val_bm[1],
              sr_val_bm[2],so_val_bm,
              VaR_val_bm,n_months_val_bm,
              NA,NA))
# S &P 500 (Market Cap Benchmark)
df_SP500 = return_xts(df_GD[,c(1,2)],'SP500',returns = T)

res = return_materics(df_fund = df_GD[,c(1,2)],RF = df_RF,fund_name = 'SP500')
# final table
final_df = data.frame(ExcessReturn=res[[1]][1],
                      SharpeRatio_StdDev= res[[1]][2],
                      SharpeRatio_VaR= res[[1]][3],
                      SortinoRatio= res[[1]][4],
                      VaR= res[[1]][5],
                      N= res[[1]][6],
                      FundFlowCorrelation = res[[1]][7],
                      N_FundFlow = res[[1]][8]
                      )
# loop over other fund-names
for (n in 3:length(fundNames)){
  # get index of fund name
  x = which(fundNames[n] == list_FundNames,arr.ind = T)

  df_fund = df_GD[,c(x+0:3)]
  res = return_materics(df_fund = df_fund,
                        RF = df_RF,
                        BM = df_SP500,
                        fund_name = fundNames[n]
                        )
  final_df[nrow(final_df)+1,] = c(res[[1]])
  final_df[nrow(final_df)+1,] = c(res[[2]])
}

# Adding fund names
final_df$FundName = c(fundNames[1],c(rbind(fundNames[c(-1,-2)],'SP500')))

# Rearranging columns
final_df = final_df[,c(ncol(final_df),1:(ncol(final_df)-1))]
rm(n,fundNames,list_FundNames,x,df_GD,df_RF)
# Saving output to a csv
write.csv(final_df,paste0('output/Metrics ',file_name),row.names = F)
