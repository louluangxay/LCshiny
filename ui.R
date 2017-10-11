dashboardPage(
  skin = "black",
  dashboardHeader(title = "Lending Club Analysis"),
  dashboardSidebar(
    sidebarUserPanel('Shubh Varma',
                    image = 'http://s3.amazonaws.com/garysguide/78e4baf3e87b4934a8b0028fbca0a595original.jpg'),
    #Sidebar content
    sidebarMenu(
      menuItem("Intro", tabName = "intro", icon = icon("tasks")),
      menuItem("How does LC work?", tabName = "tab2", icon = icon("tasks"),
               menuSubItem("Loan - Grade Analysis", tabName = "sub1"),
               menuSubItem("Variable Interaction", tabName = "sub2")),
      menuItem("Evolution over Time",tabName ="tab3",icon = icon("tasks"),
              menuSubItem("Geographic distribution", tabName = "geo"),
              menuSubItem("Growth", tabName = "evo")),
      menuItem("2016 Scandal",tabName ="tab4", icon = icon("tasks")),
      menuItem("Post scandal Analysis",tabName ="tab7", icon = icon("tasks"),
               menuSubItem("Default Rate Trends", tabName = "default")),
      menuItem("Outlook",tabName ="tab8", icon = icon("tasks")),
      menuItem("Lending Club Data", icon = icon("database"), 
               href = "https://www.lendingclub.com/info/download-data.action")
    ) #closes sidebar menu
  ), # closes dashboard - sidebar
  
  dashboardBody(
    #' tags$head(
    #'   tags$style(HTML("
    #'                   #@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    #'                   "))
    #'   ),
    #' 
    #' headerPanel(
    #'   h1("What is Lending Club?", 
    #'      style = "font-family: 'Lobster', cursive;
    #'      font-weight: 500; line-height: 1.1; 
    #'      color: #4d3a7d;")),
    tabItems(
      # First tab content
      tabItem(tabName = "intro", 
              fluidRow(
                column(6, h4(img(src="lc_ipo.jpg", height = 300))),
                column(6, h4(img(src="worldcloud_purpose1.png", height = 300)))
              ),
              h3(class = 'text-muted','What is Lending Club?'),
              h4("Lending Club is an online lending platform that connects borrowers to lenders. Since its establishment in 2007, lending club has revolutionized personal loans in the US and is the world’s largest peer to peer (P2P) lending platform. The company raised $1 billion in what became the largest technology IPO of 2014 in the US."),
              h4("It enables borrowers to create unsecured personal loans up to $40,000 on either a 3-year or 5-year term. Investors can then browse the loan listings on the website and select loans they want to invest in based on a myriad of variables such as amount of loan, purpose of loan, loan grade, fico score etc. Like most secondary marketplaces, Lending Club makes money by charging a “middle-man” fee for screening borrowers, facilitating transactions, and servicing the loans."),
              h4("So why are borrowers using lending club instead of traditional banks? Lending Club tends to offers lower rates than what the market would provide. As you can see in the wordcloud, it is common for borrowers to use these loans to consolidate debts."),  
              h3(class = 'text-muted','Is it STILL a good idea to invest in loans on Lending Club?'),
              h4("P2P is a new business model and like all business models it has to be tested, it has to go through some issues and come out well on the other side. In early 2016, Lending Club was involved in a scandal over some of the firm's loans and concerns by the board over CEO Renaud Laplanche's disclosures leading to a large drop in its share price and the CEO’s resignation.")
      ), #ends first tab
      
      # Loan-Grade Analysis Sub-tab1 (how does lc work?)
      tabItem(tabName = "sub1", 
              h3(class = 'text-muted',"Understanding the basics..." ),
              h4("Lending club assigns grades to loans as an indicator of risk. Grade A is considered the safest and Grade G is the riskiest. As expected, interest rates increase from A to G."),
              h4("Some Grade G loans have interest rates as high as 30%!"),
              fluidRow(
                column(width = 5, plotOutput("dens_grade")),
                column(width = 7, plotOutput("int_subgrade"))
              ) #ends fluidRow
      ), #ends tab (subtab1)
      
      #Variable Interaction SubTab - 2 (how does lc work?)
      tabItem(tabName = "sub2",
              h3(class = 'text-muted',"Which variables contribute to Lending Club's credit rating model?"),
              selectInput(inputId = "xinput", label = strong("Select Metric (x-axis): "),
                          choices = c("Annual Income" = "annual_inc","Fico Score" = "fico_range_low","Employment length (years)" = "emp_length","Debt to Ratio Income" = "dti","Delinquencies over past 2 years" = "delinq_2yrs",
                                      "Number of collections in 12 months excluding medical collections" = "collections_12_mths_ex_med","Number of installment accounts opened in past 24 months" = "open_il_24m",
                                      "Average current balance of all accounts" = "avg_cur_bal","Amount owed for the accounts on which the borrower is now delinquent" = "delinq_amnt","Number of accounts opened in past 12 months" = "num_tl_op_past_12m",
                                      "Number of public record bankruptcies" = "pub_rec_bankruptcies","Total high credit/credit limit" = "tot_hi_cred_lim","Credit Age (earliest reported credit line)" = "credit_age"),
                          selected = "annual_inc"),
              fluidRow(
                plotlyOutput(outputId = "var_inter")
              ),
              verbatimTextOutput("corr")
      ), #ends tabItem (subtab2)
      
      #Geographic Distribution SubTab1 (evolution over time)
      tabItem(tabName = "geo",
              h3(class = 'text-muted', "Where is the money going?"),
              sliderInput(inputId = "year", label = strong("Please select year"),
                          min = 2007, max = 2017, value = 2013, step=1, sep = '',
                          format="###0", animate=animationOptions(interval=3000)),
              fluidRow(
                box(h2(textOutput("year")), htmlOutput("gvis")
                ),# ends box
                box(h2(textOutput("year2")), htmlOutput("map2"))
              ), #ends fluidRow
              h4("California has the highest loaned amount and remains consistent from 2008 onwards. Texas has grown each year, especially in recent years."),
              h4("The highest interest rates are offered in Hawaii in more recent years, however, rates seem to be volatile from state to state. Overall, loan amounts along with interest rates have converged toward uniformity.")
      ), #ends tabItem (subtab1)
      
      #Growth Subtab2 (evolution over time)
      tabItem(tabName = "evo",
              h3(class = 'text-muted', "Changes as the company grows"),
              selectInput(inputId = "grade2", label = strong("Select grade"),
                          choices = sort(unique(lc$grade)),
                          selected = "A"),
              dygraphOutput("dygraph"),
              dygraphOutput("loanGrowth")
      ), #ends tab (subtab2)
      
      #2016 Scandal (4th Tab)
      tabItem(tabName = "tab4",
                h3(class = 'text-muted', "Identifying the scandal in 1Q 2016"),
                h2(img(src="lc_headline.png", height = 200, width = 800)),
              selectInput(inputId = "loanType", label = strong("Select Loan Metric"),
                          choices = c("Loan Volume" = "count", "Total Loan Amount" = "total_loan_amount"),
                          selected = "count"),
              fluidRow(
                column(width = 8, plotlyOutput(outputId = "monthlyLoans")),  
                column(width = 4, tableOutput(outputId = "monthlyTable"))
              ),
              h4("Historically, July and October have seen consistent spikes in loan amount, likely due to holiday seasons. However, 2016 saw an abnormal spike in 1st quarter, specifically in March – the month of the scandal. In May, their CEO stepped down and shares fell over 30%, resulting in a dip in loan amount.")
      ), #ends tabItem (4th tab)  
      
      #Default Rate trends Tab5 (post scandal analysis)
      tabItem(tabName = "default",
              h3(class = 'text-muted',"A closer look at Default Rates"),
                    sliderInput("defaultYear", label = "Highlight specific years:", 
                                min = 2007, max = 2017, value = c(2012,2014), step=1, sep = '')
                 #ends box1
              , #ends fluidRow
              hr(),
              fluidRow(
                width = 12, title = "Default Rate Analysis",
                  plotOutput("defaultBar", height = 600)
                 #ends box2
              ) #ends fluidRow2
      ), #this ends tabItem (tab 3) 
      
      
      #8th tab content
      tabItem(tabName = "tab8",
              h3(class = 'text-muted', "Final Outlook"),
              h4("Lending Club’s credit rating model seems to be consistent over various factors and allows investors to choose their risk level accurately. After the 2016 scandal and the CEO’s resignation, we noticed a dip in investments, but their investor base remained confident and investments have bounced back."),
              h4("While Lending Club is offering interest rates higher than ever before, one should be cautious over investing in loans on Lending Club, especially for riskier investments (Grades D through G). Default rates have been increasing indicating that Lending Club is accepting riskier borrowers. Now, more so than ever before, the investor must look at the variables linked to a loan to differentiate the good investments from the bad. ")
      )  
    ) #ends TabItemS
  ) #ends Dashboard Body
) #ends dashboard page