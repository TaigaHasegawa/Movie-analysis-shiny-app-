
library("rvest")
history=function(companyurl){
  download.file(companyurl, destfile = "company.html", quiet=TRUE)
  company=read_html("company.html")
  history=company %>%
    html_nodes("table") %>%
    html_table()
  history=history[[1]]
  Genre=rep(NA,dim(history)[1])
  Running_Time=rep(NA,dim(history)[1])
  Keywords=rep(NA,dim(history)[1])
  Franchise=rep(NA,dim(history)[1])
  history=cbind(history,Genre)
  history=cbind(history,Running_Time)
  history=cbind(history,Keywords)
  history=cbind(history,Franchise)  
  return(history)
}

movie1=history("https://m.the-numbers.com/movies/production-company/Warner-Bros")
movie1
movie2=history("https://m.the-numbers.com/movies/production-company/Columbia-Pictures")
movie3=history("https://m.the-numbers.com/movies/production-company/Universal-Pictures")
movie4=history("https://m.the-numbers.com/movies/production-company/Walt-Disney-Pictures")
movie5=history("https://m.the-numbers.com/movies/production-company/Marvel-Studios")
movie6=history("https://m.the-numbers.com/movies/production-company/Paramount-Pictures")
movie7=history("https://m.the-numbers.com/movies/production-company/20th-Century-Fox")
movie8=history("https://m.the-numbers.com/movies/production-company/Relativity-Media")
movie9=history("https://m.the-numbers.com/movies/production-company/DreamWorks-Pictures")
movie10=history("https://m.the-numbers.com/movies/production-company/Dune-Entertainment")
movie11=history("https://m.the-numbers.com/movies/production-company/Amblin-Entertainment")
movie12=history("https://m.the-numbers.com/movies/production-company/Pixar")

make_url=function(companyurl){
  company=read_html(companyurl)
  url=company %>%
    html_nodes("b a") %>%
    html_attr("href")
  url1=paste0("https://m.the-numbers.com",url)
}
url1=make_url("https://m.the-numbers.com/movies/production-company/Warner-Bros")
url2=make_url("https://m.the-numbers.com/movies/production-company/Columbia-Pictures")
url3=make_url("https://m.the-numbers.com/movies/production-company/Universal-Pictures")
url4=make_url("https://m.the-numbers.com/movies/production-company/Walt-Disney-Pictures")
url5=make_url("https://m.the-numbers.com/movies/production-company/Marvel-Studios")
url6=make_url("https://m.the-numbers.com/movies/production-company/Paramount-Pictures")
url7=make_url("https://m.the-numbers.com/movies/production-company/20th-Century-Fox")
url8=make_url("https://m.the-numbers.com/movies/production-company/Relativity-Media")
url9=make_url("https://m.the-numbers.com/movies/production-company/DreamWorks-Pictures")
url10=make_url("https://m.the-numbers.com/movies/production-company/Dune-Entertainment")
url11=make_url("https://m.the-numbers.com/movies/production-company/Amblin-Entertainment")
url12=make_url("https://m.the-numbers.com/movies/production-company/Pixar")

i=1
for(url in url12){
  movie=read_html(url)
  a=movie%>%
    html_nodes("h2+ .table-responsive td")%>%
    html_text()
  if(length(a)!=0){
    s=which(a=="Genre:")
    t=which(a=="Running Time:")
    u=which(a=="Keywords:")
    v=which(a=="Franchise:")
    if(length(s)==0){
      movie12[i,"Genre"]=NA  
    }else{
      movie12[i,"Genre"]=a[s+1]  
    }
    if(length(t)==0){
      movie12[i,"Running_Time"]=NA
    }else{
      movie12[i,"Running_Time"]=a[t+1]
    }
    if(length(u)==0){
      movie12[i,"Keywords"]=NA   
    }else{
      movie12[i,"Keywords"]=a[u+1]
    }
    if(length(v)==0){
      movie12[i,"Franchise"]=NA   
    }else
      movie12[i,"Franchise"]=a[v+1]
  }
  i=i+1   
}
}

movie=rbind(movie1,movie2)
movie=rbind(movie,movie3)
movie=rbind(movie,movie4)
movie=rbind(movie,movie5)
movie=rbind(movie,movie6)
movie=rbind(movie,movie7)
movie=rbind(movie,movie8)
movie=rbind(movie,movie9)
movie=rbind(movie,movie10)
movie=rbind(movie,movie11)
movie=rbind(movie,movie12)
movie
write.csv(movie,"movie.csv")