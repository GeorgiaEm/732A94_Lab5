
#' Election distribution plot.
#' 
#' @param val_data A number to choose election type
#' @param lan_val A number to choose county
#' @param kommun_val A number to choose city
#' @return A plot
#' @export

val2014<- function(val_data,lan_val,kommun_val){
  require("XML")
  require(stringr)
  require(ggplot2)
  
  url <- "http://www.val.se/val/val2014/statistik/index.html"
  get_url <- getHTMLLinks(url)
  
  res <<- get_url[str_sub(get_url, start= -10)=="kommun.skv"]
    hejsan <- paste("http://www.val.se/val/val2014/statistik/",res[val_data],sep="")
  zz<<-read.csv2(as.character(hejsan))
  yy <<- zz[zz[,3]==unique(zz[,3])[lan_val],]
  
  
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
