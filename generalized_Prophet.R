prophetfun = function(y, dates, targetDate){
  df = as.data.frame(cbind(dates, y)) %>% 
    rename(ds=1,y=2)
  df = unnest(df, cols = c(ds,y))
  model = prophet(df, yearly.seasonality = "auto",
                  weekly.seasonality = "auto")
  targetDate %<>% as.data.frame() %>% 
    rename(ds=1)
  pred = predict(model, targetDate)
  res = pred$yhat
  return(res)
}



# not run -----------------------------------------------------------------


roll1 %<>% mutate(
  Naive = map(.x = trainAdjusted, .f = naive, h = 1) %>% 
    map_dbl(.f = extract2, "mean"),
  Prophet = map(.x = trainAdjusted, .f = prophetfun2, 
                dates = trainDates, targetDate = targetDate)
)