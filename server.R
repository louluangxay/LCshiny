server <- function(input, output) { 
  
  dyPlot1 <- reactive({
    dytest1 <- filter(lc, grade == input$grade2) %>% group_by(issue_d) %>% summarise(avg_int = mean(int_rate))
    time_series <- xts(dytest1, order.by = dytest1$issue_d)
  })
  
  #subset data for variable correlation graoh
  x_int_plot <- reactive ({
    filter(lc,annual_inc < 5000000) %>% group_by_(input$xinput) %>% summarise(avg_int = mean(int_rate))
    #select(lc, input$xinput, int_rate)
  })

  
  
  #subset data for time evolution plot (interest rate smoothing)
  selected_grades <- reactive ({
    filter(lc, grade == input$grade) %>% select(issued_yr, grade, int_rate)
  })
  
  #subset data for scandal plot (monthly)
  scandal_plot1 <- reactive ({
    filter(scandal_table1, issued_yr > 2011) %>% select(., issued_yr, issued_month, loanMetric = input$loanType) 
  })
  
  #subset data for Map: to include state, year, loan amount
  selected_year <- reactive({
    filter(lc, as.numeric(issued_yr) == myYear()) %>% group_by(., addr_state, issued_yr) %>%
      summarise(total_loan = sum(loan_amnt), avg_int = mean(int_rate))
  }) #ends reactive statement 1
  
  myYear <- reactive({
    input$year
  })#ends reactive statement 2
  
  myMetric <- reactive({
    input$xinput
  })
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  }) # ends renderPlot1
  
  output$int_subgrade <- renderPlot({
    ggplot(subgrade_int, aes(x = reorder(sub_grade, avg_int), y = avg_int)) + 
      geom_bar(stat = "identity", aes(fill = grade)) + xlab("SUBGRADE") + ylab("AVERAGE INTEREST RATE") +
      theme(plot.background = element_rect(fill = 'lightgrey', colour = 'black'),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
  }) #ends renderPlot2
  
  output$dens_grade <- renderPlot ({
    ggplot(grade_facet, aes(x = int_rate)) + geom_density(aes(fill = grade)) + facet_grid(grade ~ .) + 
      xlab("INTEREST RATE") + ylab("DENSITY") +
      theme(plot.background = element_rect(fill = 'lightgrey', colour = 'black'),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
  })
  
  output$defaultBar <- renderPlot({
    ggplot(default_rate1, aes(x = issued_yr, y = prop))+ geom_bar(stat = "identity", aes(fill = (issued_yr <= input$defaultYear[2] & issued_yr >= input$defaultYear[1]))) + 
      facet_grid(term ~ grade) + 
      scale_fill_manual(values = c('white', 'blue')) + theme(legend.position="none") + ylab("default rate") +
      theme(plot.background = element_rect(fill = 'lightgrey', colour = 'black'),
            panel.background = element_rect(fill = 'lightgrey', colour = 'black'),
            panel.grid.minor = element_blank())

  })
  
  output$year <- renderText({
    paste("Loan Amount in", myYear())
  }) #ends renderText1
  
  output$year2 <- renderText({
    paste("Interest Rate in", myYear())
  }) #ends renderText2
  
  output$gvis <- renderGvis({
    
    gvisGeoChart(selected_year(), locationvar="addr_state", colorvar="total_loan",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=500, height=400, animate = TRUE
                              
                 ) #ends list in options
    ) #ends gvisGeoChart
  })
  
  output$map2 <- renderGvis({
    
    gvisGeoChart(selected_year(), locationvar="addr_state", colorvar="avg_int",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=500, height=400, animate = TRUE
                 ) #ends list in options
    ) #ends gvisGeoChart
  })
  
   output$monthlyTable <- renderGvis({
     gvisTable(scandal_table1, 
               formats = list(issued_yr="####"),
               options=list(page='enable', height='400', width='automatic'))
   })
  
  output$monthlyLoans <- renderPlotly({
    ggplot(scandal_plot1(), aes(x = issued_month, y = loanMetric)) + geom_bar(stat = "identity", aes(fill = loanMetric)) + 
       facet_grid(issued_yr ~ .) + scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
       scale_y_continuous(name=input$loanType, labels = scales::comma) +
       theme(axis.title.x=element_blank(),axis.title.y=element_blank(), legend.position="none",
             plot.background = element_rect(fill = 'lightgrey', colour = 'black'),
             panel.background = element_rect(fill = 'white', colour = 'black'),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank()) +
             ggtitle("Monthly loan distribution (2012 - 2017)")
             
    ggplotly()
  })
  
  
  output$evol <- renderPlot ({
    print(
      ggplot(selected_grades(), aes(x = issued_yr, y = int_rate)) + geom_smooth() + xlab("Date issued") + ylab("Interest Rate")
    )
  })
  
  output$loanGrowth <- renderDygraph ({
    dygraph(time_series2, main = "Growth in Loans funded") %>%
      dyEvent("2014-8-07", "IPO", labelLoc = "bottom") %>%
      dyEvent("2016-5-01", "CEO resigns", labelLoc = "bottom") %>%
      dySeries("total_loan", label = "Total Loan Amount") %>%
      dySeries("issue_d", label = "Issued Year") %>%
      dyOptions(drawGrid = T, maxNumberWidth = 21) %>%
      dyRangeSelector()
    # ggplot(amnt_evol, aes(x = issue_d, y = total_amount)) + geom_line(stat = "identity", color = "#56B4E9") + xlab("Date issued") + scale_y_continuous(name="Total Loaned Amount", labels = scales::comma) +
    #   geom_vline(xintercept = as.Date("01-Mar-2014",format="%d-%b-%Y"),linetype="dotted", color = "#000000", size=1)
  })
  
  # output$var_inter <- renderPlot ({
  #   ggplot(x_int_plot(), aes_string(x = myMetric(), y = "int_rate")) + geom_smooth() + scale_x_continuous(name = myMetric(), labels = scales::comma)
  # })
  
  output$var_inter <- renderPlotly({
    #gvisLineChart(x_int_plot(), xvar=myMetric(), yvar="avg_int")
    ggplot(x_int_plot(), aes_string(x = input$xinput, y = "avg_int")) + 
      geom_smooth() + 
      scale_x_continuous(labels = scales::comma) +
      labs(title = "Variable Interaction", x = myMetric(), y = "Interest Rate") + 
      theme(plot.background = element_rect(fill = 'lightgrey', colour = 'black'),
            panel.background = element_rect(fill = 'grey', colour = 'black'))
    ggplotly()
  })
  
  output$corr <- renderPrint(
    paste("Correlation coefficient: ", cor(lc[[input$xinput]],lc[['int_rate']]))
  )
  
  # class(lc$)
  output$wordcloud <- renderPlot ({
    wordcloud(lc$purpose,scale = c(2,.5),max.words = 100,
              random.order=T, 
              rot.per=0.15, 
              use.r.layout=F, 
              colors=brewer.pal(8,"Dark2"))
  })
  
  output$dygraph <- renderDygraph({
    dygraph(dyPlot1(), main = "Changes in interest rate over Time") %>%
      dySeries("avg_int", label = "Interest Rate") %>%
      dySeries("issue_d", label = "Issued Year") %>%
      dyOptions(drawGrid = T) %>%
      dyRangeSelector()
  })
                                

}