#Introduction

GetUIintroduction<- function() {
    div(id="Introduction",
    class="section scrollpsy",
    br(tags$h3("Introduction")),
    style="white-space: initial;",
    p(stri_rand_lipsum(1, start_lipsum = F)))
}