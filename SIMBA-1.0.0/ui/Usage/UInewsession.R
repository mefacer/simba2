GetUInewsession<- function(){
  div(id="New_session",
      class="section scrollpsy",
      br(tags$h3("New session")),
      style="white-space: initial;",
      p(stri_rand_lipsum(1, start_lipsum = TRUE)))
}