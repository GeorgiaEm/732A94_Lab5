#' 
#'  Election data function
#' 
#'  @return Put values to val_data, lan_val and kommun_val
#' @export

val2014_data<- function(){
  require("XML")
  require(stringr)
  require(ggplot2)
  
  url <- "http://www.val.se/val/val2014/statistik/index.html"
  get_url <- getHTMLLinks(url)
  
  res <- get_url[str_sub(get_url, start= -10)=="kommun.skv"]
  val<-print(data.frame("Val"=res))
  val_data<<-readline(prompt="Valj data: ")
  while(val_data=="" || !(val_data>0 && val_data<=length(res))){
    print(val)
    warn<-paste("***Pick a number between 1 and ",length(res),sep="")
    cat(warn)
    
    val_data<<-readline(prompt="Valj data:")
  }
  val_data<<-as.numeric(val_data)
  hejsan <- paste("http://www.val.se/val/val2014/statistik/",res[val_data],sep="")
  zz<-read.csv2(as.character(hejsan))
  print(data.frame(unique(zz[,3])))
  lan_val <<- readline(prompt="Valj lan: ")
  while(!suppressWarnings(!is.na(as.numeric(lan_val)))  || !(lan_val>0 && as.numeric(lan_val)<=length(unique(zz[,3])))){
    print(data.frame(unique(zz[,3])))
    warn<-paste("***Pick a number between 1 and ",length(unique(zz[,3])),sep="")
    cat(warn)
    
    lan_val<<-readline(prompt="Valj data:")
  }
  lan_val<<-as.numeric(lan_val)
  yy <- zz[zz[,3]==unique(zz[,3])[lan_val],]
  print(data.frame(unique(yy[,4])))
  kommun_val <<- readline(prompt="Valj kommun: ")
  while(!suppressWarnings(!is.na(as.numeric(kommun_val))) || !(kommun_val>0 && as.numeric(kommun_val)<=nrow(unique(yy)))){
    print(data.frame(unique(yy[,4])))
    
    warn<-print("***Pick a number between 1 and ",nrow(unique(yy)),sep="")
    warn
    kommun_val<<-readline(prompt="Valj data:")
  }
  kommun_val<<-as.numeric(kommun_val)
  val_typ<-character()
  if (val_data==1) val_typ<-"Riskdagsval år 2014"
  if (val_data==2) val_typ<-"Landstingsval år 2014"
  if (val_data==3) val_typ<-"Kommunval år 2014"
  lan<-as.character(unique(zz[,3])[lan_val][1])
  kommun<-as.character(unique(yy[,4])[kommun_val][1])
  ret<-paste("You have chosen",val_typ,"in",kommun,"in",lan,sep=" ")
  return(ret)
}
