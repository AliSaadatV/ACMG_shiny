library(shiny)
library(dplyr)


ui <- fluidPage( theme = bslib::bs_theme(bootswatch = "minty"),
  tabsetPanel(
    
  tabPanel("Very strong",
           fluidRow(
             column(6, h3("Pathogenic")),
             column(6, h3("Benign"))
           ),
           wellPanel(
             fluidRow(
               column(6, checkboxInput("pvs1", "High confidence LoF")),
               column(6, checkboxInput("ba1", "Population MAF > 5%"))
             )
           )),
  
  tabPanel("Strong",
           fluidRow(
             column(6, h3("Pathogenic")),
             column(6, h3("Benign"))
           ),
           wellPanel(
             fluidRow(
               column(6, checkboxInput("ps1", "Novel missense, functional evidence for pathogenicity exists (e.g. A>T is proved as pathogenic, we observe C>T)")),
               column(6, checkboxInput("bs1", "Population MAF > 2%"))
             ),
             fluidRow(
               column(6, checkboxInput("ps3", "Functional evidence for pathogenicity")),
               column(6, checkboxInput("bs3", "Functional evidence for benign"))
             ),
             fluidRow(
               column(6, checkboxInput("pvs1_strong", "Low confidence LoF")),
               column(6, checkboxInput("bs2", "Observed in matched controls"))
             )
           )),
  
  tabPanel("Moderate",
           fluidRow(
             column(6, h3("Pathogenic")),
             column(6, h3("Benign"))
           ),
           wellPanel(
             fluidRow(
               column(6, checkboxInput("pm1", "Missense in mutational hot-spot or in a functional domain")),
               column(6, checkboxInput("bs2_moderate", "Observed in matched controls, but it is a heterozygous variant in recessive gene"))
             ),
             fluidRow(
               column(6, checkboxInput("pm4", "Protein length-changing variant (start-lost, stop-lost, or inframe indel)"))
             ),
             fluidRow(
               column(6, checkboxInput("pm5", "Novel missense, functional evidence for another missense exists (e.g. A>T is proved as pathogenic, we observe A>G)"))
             )
           )),
  
  tabPanel("Supporting",
           fluidRow(
             column(6, h3("Pathogenic")),
             column(6, h3("Benign"))
           ),
           wellPanel(
             fluidRow(
               column(6, checkboxInput("pp2", "Missense variant in a missense-intolerant gene")),
               column(6, checkboxInput("bp1", "Missense variant in a missense-tolerant gene"))
             ),
             fluidRow(
               column(6, checkboxInput("pp3", "Computational evidence (e.g. REVEL > 0.5)")),
               column(6, checkboxInput("bp4", "Computational evidence (e.g. REVEL < 0.4)"))
             ),
             fluidRow(
               column(6, checkboxInput("pm2_supporting", "Population MAF < 0.5%")),
               column(6, checkboxInput("bp3", "Inframe indel in a repeat region"))
             )
           )),
  
  tabPanel("Results",
           h2(htmlOutput("probability")),
           actionButton("reset", "Restart"))
  
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  PV <- reactive(as.numeric(input$pvs1))
  
  PS <- reactive(as.numeric(input$ps1) + as.numeric(input$ps3) + as.numeric(input$pvs1_strong))
  
  PM <- reactive(as.numeric(input$pm1) + as.numeric(input$pm4) + as.numeric(input$pm5))
  
  PP <- reactive(as.numeric(input$pp2) + as.numeric(input$pp3) + as.numeric(input$pm2_supporting))
  
  BA <- reactive(as.numeric(input$ba1))
  
  BS <- reactive(as.numeric(input$bs1) + as.numeric(input$bs2) + as.numeric(input$bs3))
  
  BM <- reactive(as.numeric(input$bs2_moderate))
  
  BP <- reactive(as.numeric(input$bp1) + as.numeric(input$bp3) + as.numeric(input$bp4))
  
  OP <- reactive(350^(PV() + (PS()-BS())/2 + (PM()-BM())/4 + (PP()-BP())/8))
  
  output$probability <- renderText({
    if(BA()==1){
      paste("The probability of pathogenicity is","<font color=\'#30E3DF\'><b>", 0, "</b></font>")
    }
    else{
      prob <- (OP()*0.1) / ( (OP()-1)*0.1 + 1)
      if(prob >= 0.89) {
        paste("The probability of pathogenicity is","<font color=\'#D61355\'><b>", prob, "</b></font>")
      }
      else if (prob <= 0.1){
        paste("The probability of pathogenicity is","<font color=\'#30E3DF\'><b>", prob, "</b></font>")
      }
      else {
        paste("The probability of pathogenicity is","<font color=\'#FCE22A\'><b>", prob, "</b></font>")
      }
      
    }
  })
  
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
    data.frame(
      names = names(x),
      values = unlist(x, use.names = FALSE)
    )
  })
  
  observeEvent(input$reset, {
    to_clear <- AllInputs() %>%
      filter(names != "reset" & values == 1) %>%
      pull(names)
    for(i in 1:length(to_clear)){
      updateCheckboxInput(session, to_clear[i], value = FALSE)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
