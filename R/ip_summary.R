ip_summary = function(){
  info = read.csv(paste0("http://localhost:5984/serrf/info/info.csv"), stringsAsFactors = F,na.strings = "")
  # get ip summary
  ip_summ = t(info$num)
  colnames(ip_summ) = info$code

  ip_summ = data.frame(ip_summ)

  ip_summ = jsonlite::toJSON(ip_summ)
  return(ip_summ)
}
