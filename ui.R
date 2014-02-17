# user interface for committee app

library(shiny)

shinyUI(fluidPage(
  
  h2("check my exam committee!"),
  
  fluidRow(
    column(3, 
           selectInput("type", label="committee type:", choices=c("preliminary oral", "thesis")),
           textInput("dept", label="your department:", "[department]")
    ),
    column(3, 
           actionButton("button", "check!"),
           h4("and the result is..."),
           textOutput("result")
    )
  ),
  
  hr(),
  
  fluidRow(
    h4("potential committee members:"),
    column(2,
           #h5("name"),
           #htmlOutput("nameBoxes")
           h5("advisor info"),
           htmlOutput("advisorColumn")
    ),
    column(2,
           h5("member 2 info"),
           htmlOutput("memberColumn2")
    ),
    column(2,
           h5("member 3 info"),
           htmlOutput("memberColumn3")
    ),
    column(2,
           h5("member 4 info"),
           htmlOutput("memberColumn4")
    ),
    conditionalPanel(condition="input.type == 'preliminary oral'",
    column(2,
           h5("member 5 info"),
           htmlOutput("memberColumn5")))
  
  ),

  fluidRow(
    h4("alternates:"),
    column(2,
           h5("in-department alternate"),
           h6("or, can be out-of department if 3 main committee members are from your department"),
           htmlOutput("alt1Column")
    ),
    column(2,
           h5("out-of-department alternate"),
           h6("(this one actually has to be outside your department)"),
           htmlOutput("alt2Column")
    )
  ),
  
  hr(),
  

  p(a("full rules: thesis committee", href="http://gist.io/9012377")),
  p(a("full rules: preliminary oral exam committee", href="http://gist.io/9012395")),
  p(a("blog post about this project", href="http://alyssafrazee.com/committee-checker-2.html")),
  p("disclaimer: this isn't endorsed by JHSPH (it was just a fun side project), so please double check your committee with your academic coordinator before you take your exam.")  
  
))



