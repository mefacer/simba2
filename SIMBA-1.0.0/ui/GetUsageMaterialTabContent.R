### Pagina Usage UI
source("ui/Usage/UIintroduction.R")
source("ui/Usage/UIloadfile.R")
source("ui/Usage/UIanalysis.R")
source("ui/Usage/UIdownloadreport.R")
source("ui/Usage/UInewsession.R")

GetMaterialUsageTabContent<-function(){ 
  material_tab_content(
  tab_id = "Usage",
  div(class="toc-wrapper pinned hide-on-small-only", id="Usage",
      tags$ul(class="section table-of-contents",
              tags$li(tags$a(href="#Introduction","Introduction")),
              tags$li(tags$a(href="#Load_file","Load file")),
              tags$li(tags$a(href="#Analysis","Analysis")),
              tags$li(tags$a(href="#Download_report","Download report")),
              tags$li(tags$a(href="#New_session","New session"))
      )),
  div(class="row",div(class="col s12 m9 offset-m2 truncate",
                      GetUIintroduction(),
                      GetUIload_file(),
                      GetUIanalysis(),
                      GetUIdownloadreport(),
                      GetUInewsession()
                      
  )))
}


