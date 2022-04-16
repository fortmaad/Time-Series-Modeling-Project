automl = function(data, data_list){
  
  feat_df = tk_get_timeseries_signature(unique(data$date)) %>% 
    na.omit() %>% select(-year.iso, -month.xts, -month.lbl,
                         -hour, -minute, -second, -hour12,
                         -am.pm, -wday, -wday.lbl, -index.num,
                         -year, -diff)
  
  results = data.frame(Symbol = c(), MSE = c(),
                       MAE = c(), RMSE = c())
  
  for (i in data_list){
    df = filter(data, symbol == i)
    df = merge(df, feat_df, by.x = "date", by.y = "index")
    df %<>% select(-date)
    df %<>% mutate(lag1 = lag(close), lag2 = lag(close, k = 2),
                   lag3 = lag(close, k = 3))
    traindf = df[1:floor(.8*nrow(df)),] %>% as.h2o()
    validdf = df[ceiling(.8*nrow(df)):floor(nrow(df)*.9),] %>% as.h2o()
    testdf = df[ceiling(nrow(df)*.9):nrow(df),] %>% as.h2o()
    
    x = 4:(ncol(df))
    y = 3
    
    model = h2o.automl(x = x, y = y,
                       training_frame = traindf,
                       validation_frame = validdf,
                       leaderboard_frame = testdf,
                       nfolds = 5,
                       stopping_metric = "AUTO",
                       max_runtime_secs = 30)
    
    best = model@leader
    
    autoML_pred = h2o.predict(best, newdata = testdf)
    error = h2o.performance(best, newdata = testdf)
    MSE = error@metrics$MSE
    RMSE = error@metrics$RMSE
    MAE = error@metrics$mae
    
    res = cbind(i, MSE, RMSE, MAE)
    results = rbind(results, res)
  } 
  
  results %<>% rename(Symbol = i)
  return(results)
}