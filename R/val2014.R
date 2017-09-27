#' 
#' #' Election results function.
#' #' @return Plot of the results.
#' 

val2014<- function(){
  require("XML")
  require(stringr)
  require(ggplot2)
  
  url <- "http://www.val.se/val/val2014/statistik/index.html"
  get_url <- getHTMLLinks(url)
  
  res <- get_url[str_sub(get_url, start= -10)=="kommun.skv"]
  val<-data.frame("Val"=res)
  print(val)
  val_data<-as.numeric(readline(prompt="Valj data: "))
  while(!(val_data>0 && val_data<=length(res))){
    print(val)
    val
    warn<-paste("***Pick a number between 1 and ",length(res),sep="")
val_data<-as.numeric(readline(prompt=paste(warn,"\nValj data:",sep="")))
  }
  hejsan <- paste("http://www.val.se/val/val2014/statistik/",res[val_data],sep="")
  zz<-read.csv2(as.character(hejsan))
  print(data.frame(unique(zz[,3])))
  
  lan_val <- as.numeric(readline(prompt="Valj lan: "))
  while(!(lan_val>0 && lan_val<=length(unique(zz[,3])))){
    print(data.frame(unique(zz[,3])))
    
    warn<-paste("***Pick a number between 1 and ",length(unique(zz[,3])),sep="")
    lan_val<-as.numeric(readline(prompt=paste(warn,"\nValj data:",sep="")))
  }
  
  yy <- zz[zz[,3]==unique(zz[,3])[lan_val],]
  print(data.frame(unique(yy[,4])))
  
  kommun_val <- as.numeric(readline(prompt="Valj kommun: "))
  
  
  while(!(kommun_val>0 && kommun_val<=length(unique(yy[,4])))){
    print(data.frame(unique(yy[,4])))
    
    warn<-paste("***Pick a number between 1 and ",length(unique(yy[,4])),sep="")
    kommun_val<-as.numeric(readline(prompt=paste(warn,"\nValj data:",sep="")))
  }
  
  
  xx <- yy[yy[,4]==unique(yy[,4])[kommun_val],]
  jj <- as.vector(xx[,seq(6,20,2)])
  names(jj) <- NULL
  jj <- t(jj)
  parti <- names(xx[,seq(6,20,2)])
  
  parti_proc<-data.frame(cbind(parti,as.numeric(jj)))

p<-ggplot(parti_proc)+ 
  aes(x = reorder(parti, as.numeric(V2)), y = as.numeric(V2)) + 
  geom_bar(stat = "identity", fill = "dark orange", colour = "black")+
  theme_bw()+
  theme(axis.title.y = element_text(angle = 0, hjust = 1))+
  xlab("Parti") + ylab("Procent") + ggtitle("Fordelningen av civilstand pa Pahittade gatan")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
  theme(panel.grid.major.y = element_line(color = "dark grey"))
return(p)
}

