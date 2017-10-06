#' Election distribution plot.
#' @return A plot
#' @export
#' 
val2014<- function(){
  require(XML)
  require(stringr)
  require(stringi)
  require(ggplot2)
  require(readxl)
  require(httr)
  url <- "http://www.val.se/val/val2014/statistik/index.html"
  get_url <- getHTMLLinks(url)
  res1 <- get_url[str_sub(get_url, start= -10)=="kommun.xls"]
  res2 <- get_url[str_sub(get_url, start= -11)=="kommun.xlsx"]
  res <- c(res1,res2)
  hejsan <- paste("http://www.val.se/val/val2014/statistik/",res,sep="")
  zz<-list()
  for (i in 1:length(hejsan)){
    if(str_sub(hejsan[i], start= -4)==".xls"){
      GET(hejsan[i], write_disk(dat <- tempfile(fileext = ".xls")))
      zz[[i]] <- read_excel(dat, 1L,skip = 2)
    }

    if(str_sub(hejsan[i], start= -5)==".xlsx"){
      GET(hejsan[i], write_disk(dat <- tempfile(fileext = ".xlsx")))
      zz[[i]] <- read_excel(dat, 1L,skip = 2)
    }
    
    # zz[[1]]<-read_excel(hejsan[1])
    # colnames(zz[[i]])<-stri_trans_general(colnames(zz[[i]]), "latin-ascii")
    
  }
  zz<-list("Riksdagsval"=zz[[1]][1:20],"Landstingsval"=zz[[2]][1:20],"Kommunval"=zz[[3]][1:20])
  Riksdagsval<<-zz[[1]]
  Landstingsval<<-zz[[2]]
  Kommunval<<-zz[[3]]
  return(zz)
}

#hej<-val2014()
#runGitHub("732A94_Lab5_shiny","GeorgiaEm")

