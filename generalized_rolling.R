rolling = function(dat, dat_list, initial){
  
  results = data.frame(splits = c(), id = c())
  
  for (i in dat_list){ 
    temp = filter(dat, symbol == i)
    temp = na_ma(temp)
    temp_res = rolling_origin(temp, initial = which(temp$date == as.Date(initial)),
                              assess = 1,
                              cumulative = TRUE) 
    results = rbind(results, temp_res)
  }
  return(results)
}

roll = rolling(dat, dat_list)

roll %<>% mutate(
  symbol = rep(dat_list, each = (nrow(roll)/length(dat_list))),
  data = map(.x = splits, .f = analysis),
  trainDates = map(.x = data, .f = extract2, 'date'),
  trainPrice =  map(.x = data, .f = extract2, 'adjusted'),
  target_data = map(.x = splits, .f = assessment),
  targetDate = map(.x = target_data, .f = extract2, 'date'),
  targetPrice =  map_dbl(.x = target_data, .f = extract2, 'adjusted')
  
)

roll_res = roll %>% select(symbol, trainPrice, targetDate, targetPrice)

plan(multiprocess)

roll_res %<>% mutate(
  Naive = map(.x = trainPrice, .f = naive, h = 1) %>% 
    map_dbl(.f = extract2, "mean"),
  SES = map(.x = trainPrice, .f = ses, h = 1) %>% 
    map_dbl(.f = extract2, "mean"),
  Arima = map(.x = trainPrice, .f = auto.arima) %>% 
    map(.f = forecast, h = 1) %>% 
    map_dbl(.f = extract2, "mean"),
  Prophet = future_map(.x = trainPrice, .f = prophetfun, 
                       dates = trainDates, targetDate = targetDate)
)