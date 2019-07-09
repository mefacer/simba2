
GetUIanalysis<- function(){
  div(id="Analysis",
      class="section scrollpsy",
      br(tags$h3("Analysis")),
      style="white-space: initial;",
      p(stri_rand_lipsum(1, start_lipsum = TRUE)))
}