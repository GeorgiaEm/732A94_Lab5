#' Election distribution plot.
#' @return A plot
#' @export
val2014<- function(){
  require(XML)
  require(stringr)
  require(stringi)
  require(ggplot2)
  url <- "http://www.val.se/val/val2014/statistik/index.html"
  get_url <- getHTMLLinks(url)
  res <- get_url[str_sub(get_url, start= -10)=="kommun.skv"]
  hejsan <- paste("http://www.val.se/val/val2014/statistik/",res,sep="")
  zz<-list()
  for (i in 1:length(hejsan)){
    data_set<-read.csv2(as.character(hejsan)[i])
    data_set[,3:4] <- data.frame(lapply(data_set, as.character), stringsAsFactors=FALSE)[,3:4]
    
    for (j in 1:nrow(data_set)){
      for (k in 3:4){
        data_set[j,k]<-stri_trans_general(data_set[j,k], "latin-ascii")
        
      }
    }
    zz[[i]]<-data_set
    colnames(zz[[i]])<-stri_trans_general(colnames(zz[[i]]), "latin-ascii")
    
  }
  zz<-list("Riksdagsval"=zz[[1]][1:20],"Landstingsval"=zz[[2]][1:20],"Kommunval"=zz[[3]][1:20])
  Riksdagsval<<-zz[[1]]
  Landstingsval<<-zz[[2]]
  Kommunval<<-zz[[3]]
  return(zz)
}
hej<-val2014()
#runGitHub("732A94_Lab5_shiny","GeorgiaEm")

