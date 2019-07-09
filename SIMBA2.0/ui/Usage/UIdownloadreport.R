GetUIdownloadreport<- function(){
  div(id="Download_report",
      class="section scrollpsy",
      tags$h3("Download report"),
      p(stri_rand_lipsum(1, start_lipsum = TRUE)))
}