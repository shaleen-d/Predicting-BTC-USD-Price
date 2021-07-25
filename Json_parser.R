parse_JSON <- function(json_data) {
  df <- data.frame(
    Date = as.POSIXct(strptime(json_data$data$date,
                               "%Y-%m-%d %H:%M:%S")), 
    Open = json_data$data$open,
    High = json_data$data$high,
    Low = json_data$data$low,
    Close = json_data$data$close,
    Volume = json_data$data$volume_ac
  )
  df
}