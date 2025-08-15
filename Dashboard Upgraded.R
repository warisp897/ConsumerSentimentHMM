# Dashboard_bs4Dash.R
# Upgraded to bs4Dash with vertical sidebar navigation

library(shiny)
library(bs4Dash)
library(plotly)
library(dygraphs)
library(zoo)
library(xts)
library(highcharter)
library(tidyverse)
library(DT)
library(visNetwork)
library(bslib)
library(fresh)
library(dplyr)
library(tidyr)
library(purrr)
library(RColorBrewer)
library(Rtsne)
library(shinyjs)
library(here)

# SHOULD NOT BE NEEDED
#library(depmixS4) 

# REPLACE AFTER UPLOADING TO GIT
setwd("C:\\Items\\Markov Chains Econ")

mint_theme <- bs_theme(
    bootswatch = "flatly",  # clean base
    primary = "#98FF98",    # mint green base
    secondary = "#2E8B57",  # optional darker green accent
    base_font = font_google("Nunito Sans"),
    heading_font = font_google("Quicksand")
)

# ------ Static content definitions (unchanged) ------
summary_string <- HTML(
    "<p>Consumer sentiment plays a critical role in the growth of the economy. The average consumer's opinion on the state of the economy can manifest directly in changes in spending, saving, investment, and the flow of capital, directly shifting the flow of the economy.</p>
  <p>However, consumer sentiment is noisy and serves as a response to economic conditions, so deeper potential relationships may not be fully captured by traditional time series analysis. This analysis serves as an alternate approach to identify these shifts in sentiment by applying a Hidden Markov Model (HMM) to categorize consumer sentiment into specific states, which can then be better understood using macroeconomic indicators.</p>
  <p>Unlike linear models that assume constant relationships between variables or variable independence, HMMs excel in modeling <em>non-stationary time series data</em>. Through analyzing the data, the model is able to identify unobserved states in the data that dictate the probability of transitioning into another state.</p>
  <p>By training the HMM on macroeconomic indicators, we can analyze which variables play the greatest role in shaping sentiment, estimate the likelihood of transitions between sentiment states, and predict future shifts more reliably than alternative models.</p>"
)

consumer_sentiment_text <- HTML(
    "<div style='font-size:15px; line-height:1.6;'>
     <p>The <strong>Consumer Sentiment Index</strong> is calculated by the University of Michigan every month using survey responses from citizens. Respondents are asked questions about their <em>current economic standing</em> and expectations for the <em>future of the economy</em>.</p>
     <p>Understanding consumer sentiment is vital to understanding the economy, as spending behavior directly reflects the health of the economy. Consumption accounts for over two-thirds of GDP. When sentiment improves, people are more likely to spend and invest, boosting economic activity. When sentiment falls, they tend to save more and cut back on purchases, slowing growth.</p>
  </div>"
)

cs_methodology <- withMathJax(HTML(
    "<div style='font-size:15px; line-height:1.6;'>
  <p>Once a month, the University of Michigan surveys around 500 citizens on their outlook of current and future economic prospects. The questions investigate how consumers feel about their personal finances and the direction of the economy.</p>
  <ul>
    <li>How are your current personal finances compared to a year ago?</li>
    <li>Do you think business conditions in the country are good or bad at present?</li>
    <li>Do you expect your personal finances to improve or worsen over the next year?</li>
    <li>Do you expect business conditions to improve or worsen in the next year?</li>
    <li>Over the next 5 years, do you think the economy will have good times or bad times?</li>
  </ul>
  <p>Responses are categorized as <strong>positive</strong>, <strong>neutral</strong>, or <strong>negative</strong>. For each question, the net value (percent positive minus percent negative) is calculated and used to compute the index:</p>
  <p>\\[ \text{Index}_{i} = \frac{\text{Current period score}}{\text{Base period score}} \times 100 \\]</p>
</div>"
))

cs_data_collection<- HTML(
    "<div style='font-size:15px; line-height:1.6;'>
  <p> The Federal Reserve of St. Louis uploads accurate, up to date, information on multiple sectors of the economy. </p>
  <p> For this analysis, a series of 20 macroeconomic indicators were collected from the year 1987 to 2024, as it provided a good balance between for the length of the dataset and the number of predictors.</p>
  <p> The indicators are categorized as the following:</p>
      <ul>
      <li>Output</li>
      <li>Labor Market</li>
      <li>Price Levels</li>
      <li>Monetary and Fiscal</li>
      <li>Housing and Construction</li>
      <li>Trade</li>
    </ul>
    
    <p> Detailed information about each category and their relationship to consumer sentiment is in <a href='javascript:void(0);' id='prelim_analysis'>Preliminary Analysis</a> </p>
</div>"
)

cs_interpretation <- HTML(
    "<div style='font-size:15px; line-height:1.6;'>
  <p>The calculated value shows how consumers feel about the overall direction of the economy and their finances, relative to 1966. Values above 100 indicate sentiment stronger than in the base period, while values below 100 indicate weaker sentiment.</p>
  <p>When comparing the values between two separate periods, the index is still interpretable. If period 1 had a score of 85, and period 2 had a score of 87, then it can be concluded that consumer sentiment has improved relative to the earlier period.</p>
</div>"
)

intro_hmm_text <- HTML(
    "<div style='font-size:17px; line-height:1.6;'>
  <p>
    A Hidden Markov Model (HMM) is a probabilistic framework used to analyze systems that evolve over time but whose underlying dynamics are not directly observable. Instead of seeing the true internal state of the system at each moment, we observe external signals or measurements that depend on these hidden states in a probabilistic manner. This structure makes HMMs especially effective for modeling time series data where the behavior is driven by latent regimes or processes.
  </p>
  <p>
    At the core of an HMM is a finite set of hidden states, each of which produces observations according to a specific probability distribution. The system transitions between these states at discrete time steps, with each transition governed by a fixed probability. In addition to the state transitions and emission mechanisms, the model also includes an initial state distribution that specifies where the system is likely to begin. As time progresses, the model generates a sequence of observations, each indirectly reflecting the system's movement through these hidden states.
  </p>
  <p>
    Because the true sequence of states is not observed, the model's utility lies in its ability to infer these states and estimate the parameters that govern their evolution. By linking observed data to underlying patterns that are otherwise hidden, HMMs offer a structured way to uncover shifts, transitions, and regime changes that standard time series models may not capture.
  </p>
</div>"
)

no_state_selected <- HTML(
    "<div style='font-size:15px; line-height:1.6;'>
  <p>
    The initial state in an HMM represents the model's starting point. It is not meant to capture long-term dynamics but rather serves to anchor the beginning of the observed sequence. Transition probabilities from the initial state indicate how likely the system is to enter each of the main hidden states. While it does not persist beyond the first step, its configuration can affect the model’s interpretation of early data points.
  </p>
</div>"
)

mint_dark_theme <- create_theme(
    theme = "superhero",  # matches your preset
    bs4dash_vars(
        #"navbar-light-bg"     = "#203734",
        "navbar-light-bg"     = "#A5A9B4",
        "navbar-light-color"  = "rgb(235, 235, 235)",
        "sidebar-light-bg"    = "#0F3F1E",
        "sidebar-light-color" = "rgb(235, 235, 235)"
    ),
    bs4dash_yiq(
        contrasted_threshold = 150,
        text_dark = "#42FF66",     # your intended primary accent
        text_light = "#ffffff"
    )
)

# ----- UI: bs4DashPage with vertical sidebar navigation -----
ui <- bs4DashPage(
    freshTheme = mint_dark_theme,
    title = "An Analysis of Consumer Sentiment States with a Hidden Markov Model",
    controlbar = NULL,
    #footer = dashboardFooter(disable = TRUE),
    #footer = "test",
    
    # Header/navbar
    header = bs4DashNavbar(
        title = bs4DashBrand(
            title = "An Analysis of Consumer Sentiment States with a Hidden Markov Model",
        ),
        status = "success",
        #brandColor = "success",
        border = TRUE
    ),
    
    # Sidebar with vertical menu
    sidebar = bs4DashSidebar(
        id   = "sidebar",
        skin = "light",
        #status = "success",
        #brandColor = "success",
        #brandColor = "teal",
        collapsed = TRUE,
        bs4SidebarMenu(
            bs4SidebarMenuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            bs4SidebarMenuItem("Preliminary Analysis", tabName = "preliminary_analysis", icon = icon("chart-line")),
            bs4SidebarMenuItem("Hidden Markov Model", tabName = "model_intro", icon = icon("project-diagram")),
            bs4SidebarMenuItem("Analysis", tabName = "model_analysis", icon = icon("chart-line")),
            bs4SidebarMenuItem("Conclusion", tabName = "Model Conclusion", icon = icon("chart-line"))
        )
    ),
    
    # Body with tab items
    body = bs4DashBody(
        useShinyjs(),
        tags$head(
            
            #THIS IS MAKING THE PILL STYLE TAB COLOR THE SAME COLOR AS THE THEME
            
            tags$style(HTML("
            /* Sidebar background */
              .main-sidebar {
              // background-color: #A5A9B4 !important;  /* Replace with your desired color */
              }
            
              /* Active tab item (the tab pill) */
              .main-sidebar .nav-sidebar .nav-item > .nav-link.active {
                //background-color: #A5A9B4 !important;  /* Change this to your theme color */
                color: white !important;
              }
              
                /* Hover over a tab item text color */
                  .main-sidebar .nav-sidebar .nav-item > .nav-link:hover {
                    background-color: #218838 !important;  /* Slightly darker */
                    color: white !important;
                  }
                
               /* Active tab background and text color */
              .nav-pills .nav-link.active, 
              .nav-pills .show > .nav-link {
                background-color: #28a745 !important; /* Replace with your theme color */
                color: white !important;
              }
              
                /* Hover state (non-active tabs) */
              .nav-pills .nav-link:hover {
                color: #0F3F1E !important;  /* Match your theme color */
              }

            .matrix-overlay { pointer-events: none; }
          "))
        ),
        
        # CURRENTLY DEAD CODE
        #-----------------------------------------------------------------------------
        # tags$script(type="text/x-mathjax-config", HTML("
        #     MathJax = {
        #       TeX: {
        #         extensions: ['[Contrib]/class.js']  // or whatever path T e X needs
        #       }
        #     };
        #   ")),
        #------------------------------------------------------------------------------
        
        tags$script(src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=TeX-AMS-MML_HTMLorMML"),
        
        # Custom CSS/JS
        
        bs4TabItems(
            bs4TabItem(
                tabName = "overview",
                fluidRow(
                    # Box 1: Introduction Text
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        title = "Introduction",
                        width = 12,
                        status = "success",
                        solidHeader = TRUE,
                        div(
                            style = "font-size:16px; font-family:Times-New-Roman, Helvetica; line-height:1.5;",
                            summary_string
                        )
                    )
                ),
                fluidRow(
                    # Box 2: Consumer Sentiment Over Time
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        title = HTML('<span style="margin-left: 300px;">Consumer Sentiment Over Time</span>'),
                        width = 12,
                        status = "success",
                        #solidHeader = TRUE,
                        style = "height: 425px;",
                        fluidRow(
                            column(width = 6, highchartOutput("consumer_sentiment_plot")),
                            column(width = 6,
                                   bs4Dash::tabsetPanel(
                                       id = "tabsetpanel2",
                                       type = "pills",
                                       tabPanel("About",
                                                div(style = "padding:10px; font-size:16px; line-height:1.5; height:398.5px; overflow-y:auto;",
                                                    consumer_sentiment_text)
                                       ),
                                       tabPanel("Methodology",
                                                div(style = "padding:10px; font-size:16px; line-height:1.5; height:398.5px; overflow-y:auto;",
                                                    cs_methodology)
                                       ),
                                       tabPanel("Interpretation",
                                                div(style = "padding:10px; font-size:16px; line-height:1.5; height:398.5px; overflow-y:auto;",
                                                    cs_interpretation)
                                       ),
                                       tabPanel("Data Collection",
                                                div(style = "padding:10px; font-size:16px; line-height:1.5; height:398.5px; overflow-y:auto;",
                                                    cs_data_collection)
                                       )
                                   )
                            )
                        )
                    )
                )
            ),
        
        
            
            # Preliminary Analysis Tab
            bs4TabItem(
                tabName = "preliminary_analysis",
                #id = "analysis",
                fluidRow(
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        width = 6,
                        status = "warning",
                        div(style = "height:800px; font-size:16px; line-height:1.5; display:flex; flex-direction:column;",
                            selectInput("select1", "Variable Category", choices = c(
                                "Output", "Labor Market", "Price Levels",
                                "Monetary and Fiscal", "Housing and Construction", "Trade"
                            )),
                            div(style = "flex:1 1 auto;", uiOutput("category_summary")),
                            div(style = "margin-top:auto; width:100%;", DT::dataTableOutput("summary_table"))
                        )
                    ),
                    column(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        width = 6,
                        highchartOutput("category_plot_hc", width = "100%", height = "450px"),
                        highchartOutput("pearsons_plot_hc", width = "100%", height = "450px")
                    )
                )
            ),
            
            # Hidden Markov Model Tab
            bs4TabItem(
                tabName = "model_intro",
                
                # Top explanatory card
                fluidRow(
                    column(
                        width = 12,
                        bs4Card(
                            collapsible = FALSE,
                            closable = FALSE,
                            title = "What is a Hidden Markov Model?",
                            width = 12,
                            status = "success",
                            solidHeader = TRUE,
                            intro_hmm_text
                        )
                    )
                ),
                
                # Fix: Wrap all three side-by-side elements in ONE flex container
                fluidRow(
                    column(
                        width = 12,
                        div(
                            style = "display: flex; align-items: flex-start; justify-content: space-between; width: 100%;",
                            
                            # Left card (intro text)
                            div(
                                style = "flex: 1; margin-right: 10px;",
                                bs4Card(
                                    collapsible = FALSE,
                                    closable = FALSE,
                                    title = "Introduction",
                                    width = 12,
                                    status = "success",
                                    solidHeader = TRUE,
                                    no_state_selected
                                )
                            ),
                            
                            # Middle plot
                            div(
                                style = "flex: 2; display: flex; justify-content: center;",
                                visNetworkOutput("hmm_vis", height = "400px", width = "100%")
                            ),
                            
                            # Right matrix
                            div(
                                style = "flex: 0 0 180px; margin-left: 10px;",
                                uiOutput("matrix_ui"),
                                tags$script(HTML("
                                    Shiny.addCustomMessageHandler('highlight-cells', function(message) {
                                      console.group('🔴 highlight-cells handler invoked');
                                      console.log('➤ Received message array:', message);
                        
                                      var allClasses = ['P11','P12','P13','P21','P22','P23','P31','P32','P33'];
                                      allClasses.forEach(function(cls) {
                                        var els = document.querySelectorAll('.' + cls);
                                        console.log('  • Reset', cls, 'found', els.length, 'elements');
                                        els.forEach(function(el) { el.style.color = 'black'; });
                                      });
                        
                                      message.forEach(function(id) {
                                        console.log('  → processing highlight for id:', id);
                                        var els = document.querySelectorAll('.' + id);
                                        console.log('      found', els.length, 'elements for class', id, els);
                                        if (els.length === 0) {
                                          console.warn('      ⚠️ No DOM elements found with class', id);
                                        }
                                        els.forEach(function(el) { el.style.color = 'red'; });
                                      });
                        
                                      console.groupEnd();
                                    });
                                  "))
                            )
                        )
                    )
                )
            ),
            
            # Model Analysis Tab
            bs4TabItem(
                tabName = "model_analysis",
                
                # right side (text, sig factors, analysis)
                fluidRow(
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        title = "State Plot",
                        width = 7,
                        status = "success",
                        solidHeader = TRUE,
                        div(
                            style = "flex: 2; display: flex; justify-content: center;",
                            intro_hmm_text 
                            # REPLACE WITH ACTUAL TEXT LATER
                        )
                    ),
                    
                    column(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        width = 5,
                        uiOutput("transition_mat", height = "600px", width = "100%"),
                        
                        # State plot, aligned to bottom
                        div(
                            style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: flex-end; height: calc(100vh - 200px);",  # 200px = height of header/navbar/etc
                            #plotlyOutput("state_plot", height = "650px", width = "100%")
                            highchartOutput("state_plot", height = "650px", width = "100%")
                        )
                    )
                ),
            )
        )
    )
)

# ----- Server (unchanged) -----
server <- function(input, output, session) {
    #---------------------------------------------------------------------------------------------------------
    output$category_summary <- renderUI({
        summary_text <- switch(input$select1,
                               
                               "Output" = HTML('
      <p><strong>Output indicators:</strong> variables that measure the U.S.\'s total economic production.</p>
      <ul>
        <li><strong>Nominal GDP:</strong> the raw dollar value of all goods and services produced. 
          An increase means more dollars are changing hands-often signaling higher incomes and business investment. 
          When Nominal GDP is rising, consumers generally feel richer and more willing to spend; a slowdown can sap confidence.</li>
        <li><strong>Real GDP:</strong> Nominal GDP adjusted for inflation. 
          Growth here reflects true increases in volume of production. 
          Strong Real GDP growth suggests rising real wages and employment, which typically boost consumer sentiment. 
          Conversely, flat or negative Real GDP growth often coincides with weaker confidence and lower spending.</li>
      </ul>
      <p>Because consumer spending accounts for roughly two‑thirds of GDP, shifts in these output measures quickly feed back into how households view their personal finances and the broader economy.</p>
    '),
                               
                               "Labor Market" = HTML('
      <p><strong>Labor market indicators:</strong> measures of how easily people find and keep jobs.</p>
      <ul>
        <li><strong>Unemployment rate:</strong> percent of the labor force without a job. 
          Rising unemployment often reflects businesses cutting back-this tends to dent consumer sentiment as job security erodes. 
          Falling unemployment generally lifts confidence and spending.</li>
        <li><strong>Participation rate:</strong> share of working‑age people in the labor force. 
          A rising participation rate can signal optimism that jobs are available, bolstering sentiment; 
          a falling rate may reflect discouraged workers leaving the market, dampening overall confidence.</li>
        <li><strong>Unemployment claims:</strong> weekly filings for jobless benefits. 
          Spikes in initial claims often presage layoffs and can spook consumers, while declines point to a stable labor market and support higher sentiment.</li>
      </ul>
      <p>Together, these indicators tell you how tight or slack the jobs market is, a key driver of household incomes and therefore consumer attitudes toward spending and saving.</p>
    '),
                               
                               "Price Levels" = HTML('
      <p><strong>Price levels/inflation indicators:</strong> gauges of how fast prices are rising.</p>
      <ul>
        <li><strong>CPI:</strong> tracks the cost of a fixed basket of goods for the average urban consumer. 
          Rapid CPI increases erode purchasing power and often hurt consumer sentiment; 
          moderate, predictable inflation is less damaging.</li>
        <li><strong>PCEPI:</strong> measures prices for what people actually buy (a dynamic basket). 
          Because it better reflects changing consumption patterns, PCEPI can give early warning of shifts in real household outlays and sentiment.</li>
        <li><strong>Robust PCEPI:</strong> excludes volatile food and energy. 
          This “core” measure helps isolate underlying inflation trends that more directly influence long‑term consumer expectations.</li>
        <li><strong>PPI:</strong> producer prices at the wholesale level. 
          Rising PPI often foreshadows higher CPI and can signal incoming price pressures that eventually filter through consumers’ wallets.</li>
      </ul>
      <p>When consumers see news of accelerating prices-especially core inflation, they may feel their real incomes shrinking, pulling down sentiment.  
      Conversely, stable or falling inflation tends to reassure.</p>
    '),
                               
                               "Monetary and Fiscal" = HTML('
      <p><strong>Monetary & fiscal indicators:</strong> measures of borrowing costs and government finances.</p>
      <ul>
        <li><strong>Federal funds rate:</strong> the Fed’s overnight bank‑to‑bank lending rate. 
          A higher rate makes all loans-home, auto, business-more expensive, cooling borrowing and spending. 
          Cutting the rate tends to spur activity and lift consumer confidence.</li>
        <li><strong>Three‑month Treasury yield:</strong> short‑term government borrowing cost. 
          Rising yields can reflect higher inflation expectations and tighter financial conditions, which may weaken sentiment.</li>
        <li><strong>Ten‑year Treasury yield:</strong> longer‑term borrowing cost, influenced by growth and inflation expectations. 
          An inverted yield curve (short rates above long rates) often signals recession risk and can pre‑empt lower consumer sentiment.</li>
        <li><strong>M2 money supply:</strong> the total of cash, checking deposits, and easily convertible near‑money instruments. 
          Rapid M2 growth can signal easy credit (boosting sentiment), while slow growth or contraction may reflect monetary tightening and dampen sentiment.</li>
      </ul>
      <p>Higher interest rates generally translate into higher borrowing costs and squeeze household budgets, whereas easier policy (lower rates, faster money growth) tends to buoy consumer confidence.</p>
    '),
                               
                               "Housing and Construction" = HTML('
      <p><strong>Housing & construction indicators:</strong> gauges of residential building and sales activity.</p>
      <ul>
        <li><strong>New private houses:</strong> volume of single‑family homes built. 
          A pickup in home construction suggests builders expect strong demand-often reflecting and reinforcing higher consumer sentiment.</li>
        <li><strong>New permits:</strong> approvals for future building. 
          Rising permits indicate confidence among developers and signal that the housing market-and thus consumer confidence-is on solid ground.</li>
        <li><strong>House sales:</strong> number of homes sold. 
          Strong sales often coincide with easy credit and consumer optimism; a slump may reflect tighter lending standards or weaker sentiment.</li>
        <li><strong>Case‑Shiller Index:</strong> a repeat‑sales index tracking home price changes. 
          Rising home prices build household wealth (the “wealth effect”), boosting consumer sentiment; falling prices can have the opposite effect.</li>
      </ul>
      <p>Because housing is a large share of household wealth and spending, swings in these indicators can move consumer attitudes sharply, especially on big‑ticket purchases.</p>
    '),
                               
                               "Trade" = HTML('
      <p><strong>Trade indicators:</strong> the flow of goods and services across borders.</p>
      <ul>
        <li><strong>Imports:</strong> value of foreign goods and services purchased domestically. 
          Rising imports can signal strong domestic demand (positive for sentiment), but persistent trade deficits may feed concerns about domestic manufacturing and jobs.</li>
        <li><strong>Exports:</strong> value of domestic goods sold abroad. 
          Higher exports support U.S. production and jobs, which can lift consumer sentiment, especially in manufacturing regions.</li>
        <li><strong>Current account balance:</strong> net of exports minus imports plus foreign income. 
          A surplus indicates net foreign demand for U.S. output-often a positive for aggregate production and therefore consumer confidence.</li>
      </ul>
      <p>Trade balances reflect global demand for U.S. goods and services.  
      Strong export growth often bolsters jobs and incomes, lifting sentiment, while widening trade deficits can raise questions about competitiveness and growth.</p>
    ')
        )
        
        div(
            style = "font-size:15px; font-family:Segoe UI, sans-serif; line-height:1.6;",
            summary_text
        )
    })
    
        
    #function to go to preliminary analysis
    # observeEvent(input$prelim_analysis, {
    #     updateTabsetPanel(session, "tabs", selected = "analysis")
    # })
    
    shinyjs::onclick("prelim_analysis", {
        updateTabsetPanel(session, "tabs", selected = "preliminary_analysis")
    })
    
    
    recessions <- data.frame(
        start = c("2000-03-01", "2007-12-01", "2020-03-01"),
        end   = c("2001-11-30", "2009-06-30", "2023-05-31"),
        name  = c("Dot-com Bubble", "Global Financial Crisis", "COVID-19 Pandemic"),
        color = c("rgba(255,0,0,0.3)", "rgba(255,0,0,0.3)", "rgba(255, 174, 66,0.3)"),
        stringsAsFactors = FALSE
    )
    
    
    #-------------------------------------------------------------------------------
    
    # CONSUMER SENTIMENT PLOT, PAGE 1
    scaled_data = readRDS(here("scaled_dataset.rds"))
    full_dataset = readRDS(here("full_dataset.rds"))
    
    print("1")
    output$consumer_sentiment_plot <- renderHighchart({
        
        cs_data <- data.frame(
            date = as.Date(paste0(scaled_data$Year, "-01-01")),
            sentiment = full_dataset$consumer_sentiment
        ) %>%
            arrange(date) %>%
            mutate(
                color = case_when(
                    sentiment > lag(sentiment) ~ "green",
                    sentiment < lag(sentiment) ~ "red",
                    TRUE ~ "gray"
                )
            )
        
        # series for black line
        line_series <- cs_data %>%
            transmute(
                x = datetime_to_timestamp(date),
                y = sentiment
            ) %>%
            list_parse2()
        
        # series for colored points
        scatter_series <- cs_data %>%
            transmute(
                x = datetime_to_timestamp(date),
                y = sentiment,
                color = color
            ) %>%
            pmap(function(x, y, color) {
                list(x = x, y = y, color = color)
            })
        
        # recession plot bands
        plot_bands <- lapply(seq_len(nrow(recessions)), function(i) {
            list(
                from = datetime_to_timestamp(as.Date(recessions$start[i])),
                to   = datetime_to_timestamp(as.Date(recessions$end[i])),
                color = recessions$color[i]
            )
        })
        
        highchart() %>%
            hc_chart(zoomType = "x") %>%
            hc_xAxis(
                type = "datetime",
                plotBands = plot_bands
            ) %>%
            hc_credits(enabled = FALSE) %>% 
            hc_yAxis(title = list(text = "Index Value")) %>%
            hc_add_series(
                data = line_series,
                type = "line",
                color = "black",
                lineWidth = 3,
                marker = list(enabled = FALSE),
                showInLegend = FALSE
            ) %>%
            hc_add_series(
                data = scatter_series,
                type = "scatter",
                name = NULL,
                showInLegend = FALSE
            ) %>%
            # dummy series for legend entries
            hc_add_series(
                data = list(),
                name = "Increase in Sentiment",
                type = "scatter",
                color = "green",
                marker = list(symbol = "circle", radius = 5)
            ) %>%
            hc_add_series(
                data = list(),
                name = "Decrease in Sentiment",
                type = "scatter",
                color = "red",
                marker = list(symbol = "circle", radius = 5)
            ) %>%
            #hc_title(text = "Consumer Sentiment Over Time") %>%
            hc_tooltip(formatter = JS(
                sprintf(
                    "
        function () {
          var x = this.x;
          var msg = '';
          if (x >= %s && x <= %s) {
            msg = '<br><b>Dot-com Bubble</b>';
          } else if (x >= %s && x <= %s) {
            msg = '<br><b>Global Financial Crisis</b>';
          } else if (x >= %s && x <= %s) {
            msg = '<br><b>COVID-19 Pandemic</b>';
          }
          return Highcharts.dateFormat('%%Y', x) + ': ' + this.y.toFixed(2) + msg;
        }
        ",
                    datetime_to_timestamp(as.Date("2000-03-01")),
                    datetime_to_timestamp(as.Date("2001-11-30")),
                    datetime_to_timestamp(as.Date("2007-12-01")),
                    datetime_to_timestamp(as.Date("2009-06-30")),
                    datetime_to_timestamp(as.Date("2020-03-01")),
                    datetime_to_timestamp(as.Date("2023-05-31"))
                )
            ))
        
    })
    
    #-----------------------------------------------------------------------------
    # Set up code for indicators plot
    
    output$recession_name <- renderText({
        range <- input$consumer_sentiment_plot_date_window  # provided by dygraph
        req(range)
        
        range <- as.Date(range)
        
        rec <- recessions %>%
            filter(as.Date(paste0(start, "-01-01")) <= range[2],
                   as.Date(paste0(end, "-12-31")) >= range[1])
        
        if (nrow(rec) > 0) {
            paste("Recession:", rec$name)
        } else {
            "No recession in selected range."
        }
    })
    
    format_value <- function(val, fmt) {
        if (is.na(val)) return(NA)
        if (fmt == "$") {
            return(paste0("$", formatC(val, format="f", digits=2, big.mark=",")))
        } else if (fmt == "%") {
            return(paste0(formatC(val, format="f", digits=2), "%"))
        } else {
            return(round(val, 3))
        }
    }
    
    format_true <- function(val, ind) {
        if (!ind %in% names(indicator_formats)) {
            message("Missing indicator_formats entry for: ", ind)
            return(round(val, 2))
        }
        
        fmt <- indicator_formats[[ind]]
        if (is.na(val)) return(NA)
        if (fmt == "$") {
            paste0("$", formatC(val, format = "f", digits = 2, big.mark = ","))
        } else if (fmt == "%") {
            paste0(formatC(val, format = "f", digits = 2), "%")
        } else {
            round(val, 2)
        }
    }
    
    # map for nicer indicator names
    indicator_names_map <- c(
        consumer_sentiment = "Consumer Sentiment",
        nominal_GDP = "Nominal GDP",
        real_GDP = "Real GDP",
        unemployment_rate = "Unemployment Rate",
        participation_rate = "Labor Participation Rate",
        claims = "Unemployment Claims Filed",
        CPI = "Consumer Price Index (CPI)",
        PCEPI = "PCEPI",
        PCEPI_ROBUST = "PCEPI (Adjusted for Volatile Prices)",
        federal_funds_rate = "Federal Funds Rate",
        three_month_rate = "3-Month Treasury Yield",
        M2 = "M2",
        FYFSD = "Federal Surplus / Deficit",
        federal_debt = "Federal Debt",
        new_houses = "New Private Houses Built",
        new_permits = "New Housing Permits",
        house_sales = "New Private Houses Sold",
        case_schiller_val = "Case-Schiller Index",
        export = "Total Exports",
        import = "Total Imports",
        account_balance = "Account Balance"
    )
    
    indicator_formats <- c(
        nominal_GDP = "$",
        real_GDP = "$",
        federal_debt = "$",
        FYFSD = "$",
        export = "$",
        import = "$",
        account_balance = "$",
        unemployment_rate = "%",
        participation_rate = "%",
        claims = "",
        CPI = "%",
        PCEPI = "%",
        PCEPI_ROBUST = "%",
        federal_funds_rate = "%",
        three_month_rate = "%",
        M2 = "$",
        new_houses = "",
        new_permits = "",
        house_sales = "",
        case_schiller_val = "",
        consumer_sentiment = ""
    )
    
    #- your map from raw names to pretty labels:
    nice_names <- c(
        consumer_sentiment = "Consumer Sentiment",
        nominal_GDP        = "Nominal GDP",
        real_GDP           = "Real GDP",
        unemployment_rate  = "Unemployment Rate",
        participation_rate = "Labor Participation Rate",
        claims             = "Unemployment Claims",
        CPI                = "CPI",
        PCEPI              = "PCEPI",
        PCEPI_ROBUST       = "PCEPI (Robust)",
        federal_funds_rate = "Federal Funds Rate",
        three_month_rate   = "3‑Month Rate",
        M2                 = "M2 Money Supply",
        FYFSD              = "Federal Spending",
        federal_debt       = "Federal Debt",
        new_houses         = "New Houses",
        new_permits        = "New Permits",
        house_sales        = "House Sales",
        case_schiller_val  = "Case‑Shiller Home Prices",
        export             = "Exports",
        import             = "Imports",
        account_balance    = "Current Account Balance"
    )
    
    #- which raw indicators belong to which category:
    category_map <- list(
        "Output"                 = c("consumer_sentiment", "nominal_GDP", "real_GDP"),
        "Labor Market"           = c("consumer_sentiment", "unemployment_rate", "participation_rate", "claims"),
        "Price Levels"           = c("consumer_sentiment", "CPI", "PCEPI", "PCEPI_ROBUST"),
        "Monetary and Fiscal"    = c("consumer_sentiment", "federal_funds_rate",
                                     "three_month_rate", "M2", "FYFSD", "federal_debt"),
        "Housing and Construction" = c("consumer_sentiment", "new_houses",
                                       "new_permits", "house_sales", "case_schiller_val"),
        "Trade"                  = c("consumer_sentiment", "export", "import", "account_balance")
    )
    
    palette_map <- list(
        "Output"                   = "Blues",
        "Labor Market"             = "Greens",
        "Price Levels"             = "Purples",
        "Monetary and Fiscal"      = "Reds",
        "Housing and Construction" = "Oranges",
        "Trade"                    = "PuBu"
    )
    
    # Build a distinct color palette per category
    pal_out    <- brewer.pal(length(category_map$`Output`), "Blues")
    pal_lab    <- brewer.pal(length(category_map$`Labor Market`), "Greens")
    pal_price  <- brewer.pal(length(category_map$`Price Levels`), "Purples")
    pal_mon    <- brewer.pal(length(category_map$`Monetary and Fiscal`), "Reds")
    pal_house  <- brewer.pal(length(category_map$`Housing and Construction`), "Oranges")
    pal_trade  <- brewer.pal(length(category_map$`Trade`), "PuBu")
    
    indicator_colors <- c(
        setNames(pal_out,     category_map$`Output`),
        setNames(pal_lab,     category_map$`Labor Market`),
        setNames(pal_price,   category_map$`Price Levels`),
        setNames(pal_mon,     category_map$`Monetary and Fiscal`),
        setNames(pal_house,   category_map$`Housing and Construction`),
        setNames(pal_trade,   category_map$`Trade`)
    )
    
    #default_cat <- isolate(input$select1)
    initial_cat  <- isolate(input$select1)
    initial_inds <- category_map[[ initial_cat ]]
    
    print("2")
    output$category_plot_hc <- renderHighchart({
        
        # 1) Pivot both data sets long
        scaled <- readRDS("scaled_dataset.rds") %>%
            pivot_longer(-Year, names_to = "Indicator", values_to = "Scaled")
        full   <- readRDS("full_dataset.rds") %>%
            pivot_longer(-Year, names_to = "Indicator", values_to = "True")
        
        # 2) Join, label, flag selection & formatting
        plot_df <- scaled %>%
            left_join(full, by = c("Year","Indicator")) %>%
            mutate(
                Label    = nice_names[Indicator],
                fmt      = indicator_formats[Indicator],
                #is_sel   = Indicator %in% category_map[[input$select1]]
            )
        
        # 3) Series order: everything except CS first, then CS last
        all_inds     <- unique(plot_df$Indicator)
        ordered_inds <- c(setdiff(all_inds, "consumer_sentiment"), "consumer_sentiment")
        
        # selected_raw <- category_map[[input$select1]]
        # ordered_inds <- c(
        #     intersect(setdiff(selected_raw, "consumer_sentiment"), selected_raw),
        #     "consumer_sentiment"
        # )
        
        # 5) Build the highcharter series list
        series_list <- map(ordered_inds, function(ind) {
            df_i <- filter(plot_df, Indicator == ind)
            r0 <- NA_real_
            
            list(
                id           = ind,
                name         = unique(df_i$Label),
                data         = pmap(
                    list(df_i$Year, df_i$Scaled, df_i$True, df_i$fmt),
                    function(x,y,true,fmt) list(x=x, y=y, trueValue=true, format=fmt)
                ),
                color = if (ind == "consumer_sentiment") "#000000" else indicator_colors[[ind]],
                visible      = ind %in% initial_inds,
                lineWidth    = 3,           # thicker lines
                marker       = list(radius = 2),  # smaller points
                showInLegend = TRUE
            )
        })
        
        default_cat <- isolate(input$select1)
        
        # 6) Assemble the chart
        highchart() %>%
            hc_chart(
                type    = "line",
                spacing = list(top=10, right=10, bottom=20, left=10)
            ) %>%
            hc_plotOptions(series = list(
                events = list(
                    legendItemClick = JS(
                        "function() {
           // find the bar chart
           var bc = Highcharts.charts.find(c => c.renderTo.id==='pearsons_plot_hc');
           if (!bc) return true;
           // find the matching bar series
           var bs = bc.get(this.options.id);
           if (bs) {
             // if the line *was* visible, we've just hidden it => make bar translucent
             // otherwise, we've just shown it => make bar solid
             var newColor = this.visible ? bs.options.inactiveColor : bs.options.origColor;
             bs.update({ color: newColor }, true);
           }
           return true;  // still toggle the line
         }"
                    )
                )
            )) %>%
            hc_add_series_list(series_list) %>%
            hc_legend(
                layout        = "horizontal",
                align         = "center",
                verticalAlign = "bottom",
                itemWidth     = 100,      # forces more columns
                itemStyle     = list(fontSize = "0.8em")
            ) %>%
            hc_tooltip(
                useHTML   = TRUE,
                formatter = JS(
                    "function() {",
                    "  var fmt = this.point.format || '';",
                    "  var num = Highcharts.numberFormat(this.point.trueValue, 2);",
                    "  var txt = fmt === '%' ? num + fmt : fmt + num;",
                    "  return '<b>' + this.series.name + '</b><br>' +",
                    "         'Year: ' + this.x + '<br>' +",
                    "         'Value: ' + txt;",
                    "}"
                )
            ) %>%
            hc_yAxis(
                labels = list(enabled = FALSE),
                title = list(text = "")
            ) %>%
            hc_xAxis(title = list(text = "Year")) %>%
            #hc_yAxis(title = list(text = "Scaled Value")) %>%
            hc_subtitle(text = "All indicators are scaled for visual cohesiveness") %>%
            hc_title(text = paste(default_cat, "Over Time"))
    })
    
    #updates line
    observeEvent(input$select1, {
        all_ids <- names(indicator_colors)
        # grab the already‐rendered line chart by its outputId
        proxy <- highchartProxy("category_plot_hc", session = session)
        
        for(ind in all_ids) {
            proxy <- proxy %>%
                hcpxy_update_series(
                    id      = ind,
                    visible = ind %in% category_map[[input$select1]]
                )
        }
    })
    
    #updates title
    observeEvent(input$select1, {
        highchartProxy("category_plot_hc", session) %>%
            # this will update only the chart title in place
            hcpxy_update(
                title = list(text = paste(input$select1, "Over Time"))
            )
    })
    
    
    summary_table_data <- reactive({
        selected_category <- input$select1
        
        # select the indicators for the chosen category
        indicators <- category_map[[selected_category]]
        indicators <- indicators[indicators != "consumer_sentiment"] # drop CS if needed
        
        years <- full_dataset$Year
        
        stats <- lapply(indicators, function(ind) {
            v <- full_dataset[[ind]]
            name_nice <- nice_names[[ind]] %||% ind
            fmt <- indicator_formats[[ind]] %||% ""
            
            data.frame(
                Indicator = name_nice,
                Mean = as.character(format_value(mean(v, na.rm=TRUE), fmt)),
                Median = as.character(format_value(median(v, na.rm=TRUE), fmt)),
                `Min (Year)` = sprintf("%s (%d)",
                                       format_value(min(v, na.rm=TRUE), fmt),
                                       years[which.min(v)]),
                `Max (Year)` = sprintf("%s (%d)",
                                       format_value(max(v, na.rm=TRUE), fmt),
                                       years[which.max(v)]),
                `Value 2000` = as.character(format_value(v[which(years == 2000)], fmt)),
                `Value 2008` = as.character(format_value(v[which(years == 2008)], fmt)),
                `Value COVID` = as.character(format_value(v[which(years == 2020)], fmt)),
                `Pearson r w/ CSI` = sprintf("%+.3f", cor(v, full_dataset$consumer_sentiment, use = "complete.obs")),
                stringsAsFactors = FALSE
            )
        })
        
        df <- do.call(rbind, stats)
        
        # transpose the table so Indicators are rows, stats are columns
        df_long <- tidyr::pivot_longer(
            df,
            cols = -Indicator,
            names_to = "Statistic",
            values_to = "Value"
        )
        
        df_wide <- tidyr::pivot_wider(
            df_long,
            names_from = Indicator,
            values_from = Value
        )
        
        as.data.frame(df_wide)
    })
    
    
    output$summary_table <- DT::renderDataTable({
        DT::datatable(summary_table_data(),
                      options = list(pageLength = 1000, 
                                     autoWidth = TRUE,
                                     dom = 't',
                                     responsive = TRUE),
                      rownames = FALSE
        )
        
    })
    
    indicator_to_category <- purrr::map_dfr(names(category_map), function(cat) {
        tibble::tibble(
            Indicator = category_map[[cat]],
            Category  = cat
        )
    })
    
    #creates data frame with r values for bar chart
    cor_df_reactive <- reactive({
        scaled_data %>%
            dplyr::select(-consumer_sentiment) %>%
            imap_dfr(~ tibble(
                Indicator = .y,
                r         = cor(.x, scaled_data$consumer_sentiment, use = "complete.obs")
            )) %>%
            mutate(
                Label = unname(nice_names[Indicator]),
                color = indicator_colors[Indicator]
            ) %>%
            filter(Indicator != "Year") %>%
            mutate(r = round(abs(r), 3)) %>%
            left_join(indicator_to_category, by = "Indicator") %>%
            mutate(
                color = RColorBrewer::brewer.pal(6, "Set1")[as.numeric(factor(Category))]
            )
    })
    
    #create bar chart of r values
    print(3)
    output$pearsons_plot_hc <- renderHighchart({
        cor_df <- cor_df_reactive()
        inactive_alpha <- 0.3
        
        # grab the initial combo‐box value just once
        initial_cat  <- isolate(input$select1)
        initial_inds <- category_map[[ initial_cat ]]
        
        series_list <- purrr::map(seq_len(nrow(cor_df)), function(i) {
            base_col     <- cor_df$color[i]
            inactive_col <- alpha(base_col, inactive_alpha)
            
            list(
                id            = cor_df$Indicator[i],
                name          = cor_df$Label[i],
                color         = if (cor_df$Indicator[i] %in% initial_inds) base_col else inactive_col,
                origColor     = base_col,
                inactiveColor = inactive_col,
                data          = list(list(x = i - 1, y = cor_df$r[i])),
                showInLegend  = FALSE
            )
        })
        
        highchart() %>%
            hc_chart(type = "bar") %>%
            hc_xAxis(categories = unname(cor_df$Label), reversed = TRUE) %>%
            hc_yAxis(title = list(text = "Pearson's r")) %>%
            hc_title(text = "Correlation to Consumer Sentiment") %>%
            hc_subtitle(text = "The greater the value, the more correlated the variable is with Consumer Sentiment") %>%
            hc_add_series_list(series_list) %>%
            hc_caption(
                text = paste0(
                    "<span>",
                    "<span style='display:block; text-align:center; width:100%;'>",
                    "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#1f78b4'>&#9632;</span> Output&nbsp;&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#33a02c'>&#9632;</span> Labor Market&nbsp;&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#6a3d9a'>&#9632;</span> Price Levels&nbsp;&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#e31a1c'>&#9632;</span> Monetary & Fiscal&nbsp;&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#ff7f00'>&#9632;</span> Housing & Construction&nbsp;&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#b15928'>&#9632;</span> Trade",
                    "</span>"
                ),
                useHTML = TRUE
            ) %>%
            hc_plotOptions(bar = list(pointWidth = 15))
    })
    
    category_colors <- c(
        Output                   = "#1f78b4",
        `Labor Market`           = "#33a02c",
        `Price Levels`           = "#6a3d9a",
        `Monetary & Fiscal`      = "#e31a1c",
        `Housing & Construction` = "#ff7f00",
        Trade                    = "#b15928"
    )
    
    #update bar chart when new combo box item is selected
    observeEvent(input$select1, {
        cor_df <- cor_df_reactive()
        inactive_alpha <- 0.3
        
        proxy <- highchartProxy("pearsons_plot_hc", session = session)
        
        purrr::walk(seq_len(nrow(cor_df)), function(i) {
            id          <- cor_df$Indicator[i]
            base_col    <- cor_df$color[i]
            inactive_col<- alpha(base_col, inactive_alpha)
            
            # pick opaque vs. translucent
            new_col <- if (id %in% category_map[[ input$select1 ]]) base_col else inactive_col
            
            proxy %>% hcpxy_update_series(
                id    = id,
                color = new_col
            )
        })
    })
    
    #SERVER CODE FOR PRELIMINARY ANALYSIS ABOVE, HMM BELOW
    
    #-----------------------------------------------------------------------------
    
    pre_rendered_matrix <- '
<div style="font-size:250%; text-align:center;">
  $$\n\\begin{bmatrix}
     P_{11} & P_{12} & P_{13} \\\\\n
     P_{21} & P_{22} & P_{23} \\\\\n
     P_{31} & P_{32} & P_{33}
   \\end{bmatrix}
  $$ 
  <table class="matrix-overlay" style="position:absolute; top:50%; left:50%;
                                        transform: translate(-50%, -50%);
                                        border:0; border-spacing:0;">
    <tr>
      <td><span id="P11" class="matrix-cell"></span></td>
      <td><span id="P12" class="matrix-cell"></span></td>
      <td><span id="P13" class="matrix-cell"></span></td>
    </tr>
    <tr>
      <td><span id="P21" class="matrix-cell"></span></td>
      <td><span id="P22" class="matrix-cell"></span></td>
      <td><span id="P23" class="matrix-cell"></span></td>
    </tr>
    <tr>
      <td><span id="P31" class="matrix-cell"></span></td>
      <td><span id="P32" class="matrix-cell"></span></td>
      <td><span id="P33" class="matrix-cell"></span></td>
    </tr>
  </table>
</div>'
    
    unicode_subs <- c("₀","₁","₂","₃","₄","₅","₆","₇","₈","₉")
    ascii_subs   <- c("0","1","2","3","4","5","6","7","8","9")
    
    convert_label_to_id <- function(lbl) {
        # e.g. "P₁₂" -> "P12"
        chartr(paste0(unicode_subs, collapse=""),
               paste0(ascii_subs,   collapse=""),
               lbl)
    }
    
    
    # Define states (nodes)
    nodes <- data.frame(
        id = c("Initial", "S1", "S2", "S3"),
        label = c(
            "Initial State",
            "State 1",
            "State 2",
            "State 3"
        ),
        size = 50,
        font.size = 18,
        shape = "circle",
        x     = c(0, -200, 0, 200),
        y     = c(-200, 0, 0, 0),
        # optional placeholders for customization:
        color = "#3EB489",
        title = "Hidden Markov Model Diagram"
    )
    nodes$x <- nodes$x * 2.5
    nodes$y <- nodes$y * 2.5
    
    nodes$x[nodes$id == "Initial"] <- 0
    nodes$y[nodes$id == "Initial"] <- -220
    
    nodes$x[nodes$id == "S1"] <- -300
    nodes$y[nodes$id == "S1"] <- -50
    
    nodes$x[nodes$id == "S2"] <- 0
    nodes$y[nodes$id == "S2"] <- -50
    
    nodes$x[nodes$id == "S3"] <- 300
    nodes$y[nodes$id == "S3"] <- -50
    
    nodes$size[nodes$id %in% c("S1", "S2", "S3")] <- 70
    
    # Define transitions (edges)
    edges <- data.frame(
        from = c(
            "Initial", "Initial", "Initial",
            "S1", "S1", "S1",
            "S2", "S2", "S2",
            "S3", "S3", "S3"
        ),
        to = c(
            "S1", "S2", "S3",
            "S1", "S2", "S3",
            "S1", "S2", "S3",
            "S1", "S2", "S3"
        ),
        label = c(
            "P₁", "P₂", "P₃",
            "P₁₁", "P₁₂", "P₁₃",
            "P₂₁", "P₂₂", "P₂₃",
            "P₃₁", "P₃₂", "P₃₃"
        ),
        arrows = "to",   # directed arrows
        # optional placeholders for customization:
        color = NA,
        width = NA,
        font.size = NA
    )
    
    edges$smooth <- vector("list", nrow(edges))
    edges$dashes <- FALSE
    
    edges <- edges %>%
        mutate(
            smooth = case_when(
                from == "Initial" ~ list(FALSE),
                from == to        ~ list(list(enabled=TRUE, type="curvedCW", roundness=0.3)),
                from == "S3" & to == "S1" ~ list(list(enabled=TRUE, type="curvedCW", roundness=0.3)),
                TRUE             ~ list(list(enabled=TRUE, type="curvedCW", roundness=0.2))
            ),
            dashes = from == to
        )
    
    # Build interactive HMM
    output$hmm_vis = renderVisNetwork({ visNetwork(nodes, edges) %>%
            visEdges(arrows = "to") %>%
            visNodes(fixed = TRUE) %>%
            visOptions(
                highlightNearest = TRUE,
                nodesIdSelection = list(enabled = TRUE, selected = "Initial")
            ) %>%
            visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
            visLayout(randomSeed = 42)  # fix layout for reproducibility
    })
    
    # helper to produce HTML matrix with specified highlights
    render_matrix <- function(highlight_cells) {
        mat <- matrix(paste0("P", rep(1:3, each=3), rep(1:3, times=3)), 3, 3, byrow=TRUE)
        html <- "$$ \\begin{bmatrix} "
        for (r in 1:3) {
            for (c in 1:3) {
                cell <- mat[r,c]
                if (cell %in% highlight_cells) {
                    html <- paste0(html, "\\color{red}{", cell, "}")
                } else {
                    html <- paste0(html, cell)
                }
                if (c < 3) html <- paste0(html, " & ")
            }
            if (r < 3) html <- paste0(html, " \\\\ ")
        }
        html <- paste0(html, " \\end{bmatrix} $$")
        html
    }
    
    output$matrix_ui <- renderUI({
        withMathJax(
            div(
                HTML("<h4 style='text-align:center;
                         padding: 0px;
                         height: 0px;'>Transition Matrix</h4>"),
                div(
                    style = "
                            display: flex;
                            justify-content: center;
                            align-items: flex-start;   /* or center */
                            height: 100px;             /* adjust container height */
                            padding-top: 10px;         /* optional: add some top padding */
                          ",
                    HTML(pre_rendered_matrix)
                )
            )
        )
    })
    
    
    observeEvent(list(input$hmm_vis_selected, input$hmm_vis_selectedEdges), {
        highlights <- character(0)
        
        if (!is.null(input$hmm_vis_selected) && input$hmm_vis_selected %in% c("S1","S2","S3")) {
            i <- substr(input$hmm_vis_selected, 2, 2) # "1", "2", "3"
            row_highlights <- paste0("P", i, 1:3)
            col_highlights <- paste0("P", 1:3, i)
            highlights <- unique(c(row_highlights, col_highlights))
        }
        
        if (!is.null(input$hmm_vis_selectedEdges) && length(input$hmm_vis_selectedEdges) > 0) {
            edge_label <- edges$label[match(input$hmm_vis_selectedEdges, edges$id)]
            if (!is.na(edge_label)) highlights <- edge_label
        }
        html_str <- render_matrix(highlights)
        
        session$sendCustomMessage("highlight-cells", highlights)
        
        message <- highlights
        cat("⚙️  [R] sending highlight-cells:", paste(message, collapse = ","), "\n")
        session$sendCustomMessage("highlight-cells", message)
    })
    
    
    #SERVER CODE FOR HMM ABOVE, ANALYSIS BELOW
    #-----------------------------------------------------------------------------
    
    #3D TSNE PLOT
    library(depmixS4)
    output$state_plot <- renderHighchart({
        
        # reading in data (no need to recalculate)
        sent_hmm = readRDS("sent_hmm.rds")
        fit_hmm = fit(sent_hmm)
        predicted_states <- posterior(fit_hmm)$state
        
        tsne_result = readRDS("tnse_data.rds")
        
        tsne_df <- data.frame(
            x = tsne_result$Y[, 1],
            y = tsne_result$Y[, 2],
            z = tsne_result$Y[, 3],
            group = factor(predicted_states)  # or use another grouping variable
        )
        
        hc <- highchart() %>%
            # Configure the chart to be a 3D scatterplot.
            # The 'options3d' list is key to enabling 3D functionality.
            hc_chart(
                type = "scatter3d",
                options3d = list(
                    enabled = TRUE,
                    alpha = 10, # Vertical rotation of the chart
                    beta = 30,  # Horizontal rotation of the chart
                    depth = 250, # Depth of the chart
                    viewDistance = 5, # Distance from the camera to the chart
                    # Enable mouse interaction for rotation
                    frame = list(
                        bottom = list(size = 1, color = 'transparent'),
                        back = list(size = 1, color = 'transparent'),
                        side = list(size = 1, color = 'transparent')
                    )
                ),
                # Add a load event to the chart that injects the JavaScript for dragging.
                # The 'this' in the JS code refers to the Highcharts chart object itself.
                events = list(
                    load = JS("function () {
                        // Add mouse and touch events for rotation
                        var chart = this;
                        var H = Highcharts;
                        function dragStart(eStart) {
                            eStart = chart.pointer.normalize(eStart);
                            const posX = eStart.chartX,
                                posY = eStart.chartY,
                                alpha = chart.options.chart.options3d.alpha,
                                beta = chart.options.chart.options3d.beta,
                                sensitivity = 5,
                                handlers = [];
                            function drag(e) {
                                // Get e.chartX and e.chartY
                                e = chart.pointer.normalize(e);
                                chart.update({
                                    chart: {
                                        options3d: {
                                            alpha: alpha + (e.chartY - posY) / sensitivity,
                                            beta: beta + (posX - e.chartX) / sensitivity
                                        }
                                    }
                                }, undefined, undefined, false);
                            }
                            function unbindAll() {
                                handlers.forEach(function (unbind) {
                                    if (unbind) {
                                        unbind();
                                    }
                                });
                                handlers.length = 0;
                            }
                            handlers.push(H.addEvent(document, 'mousemove', drag));
                            handlers.push(H.addEvent(document, 'touchmove', drag));
                            handlers.push(H.addEvent(document, 'mouseup', unbindAll));
                            handlers.push(H.addEvent(document, 'touchend', unbindAll));
                        }
                        H.addEvent(chart.container, 'mousedown', dragStart);
                        H.addEvent(chart.container, 'touchstart', dragStart);
                      }")
                )
            ) %>%
            # Set the main title and subtitle of the chart.
            hc_title(text = "t-SNE Results") %>%
            hc_subtitle(text = "Sample Data by Group") %>%
            hc_plotOptions(
                scatter = list(
                    width = 10,
                    height = 10,
                    depth = 10
                )
            ) %>%
            # Add the data series. The 'hcaes' function maps data columns to chart aesthetics.
            # The `group` variable is used to separate the points into different series,
            # allowing for different colors and legends.
            hc_add_series(
                data = tsne_df,
                type = "scatter3d",
                marker = list(radius = 6),
                hcaes(x = round(x, 3), y = round(y, 3), z = round(z, 3), group = group)
            ) %>%
            
            hc_xAxis(title = list(text = "t-SNE Axis 1")) %>%
            hc_yAxis(title = list(text = "t-SNE Axis 2")) %>%
            hc_zAxis(title = list(text = "t-SNE Axis 3")) %>%
            
            # Customize the tooltip that appears on hover.
            hc_tooltip(
                pointFormat = 'X: {point.x}<br>Y: {point.y}<br>Z: {point.z}<br>Group: {point.group}'
            )
        
        hc
        
    })
    
    # Transition matrix
    true_transition_matrix <- '
        <div style="font-size:250%; text-align:center;">
          $$\n\\begin{bmatrix}
             0.719 & 0.186 & 0.095 \\\\\n
             0.167 & 0.750 & 0.083 \\\\\n
             0.142 & 0.000 & 0.858
           \\end{bmatrix}
          $$ 
          <table class="matrix-overlay" style="position:absolute; top:50%; left:50%;
                                                transform: translate(-50%, -50%);
                                                border:0; border-spacing:0;">
            <tr>
              <td><span id="P11" class="matrix-cell"></span></td>
              <td><span id="P12" class="matrix-cell"></span></td>
              <td><span id="P13" class="matrix-cell"></span></td>
            </tr>
            <tr>
              <td><span id="P21" class="matrix-cell"></span></td>
              <td><span id="P22" class="matrix-cell"></span></td>
              <td><span id="P23" class="matrix-cell"></span></td>
            </tr>
            <tr>
              <td><span id="P31" class="matrix-cell"></span></td>
              <td><span id="P32" class="matrix-cell"></span></td>
              <td><span id="P33" class="matrix-cell"></span></td>
            </tr>
          </table>
        </div>'

    output$transition_mat <- renderUI({
        withMathJax(
            div(
                HTML("<h4 style='text-align:center;
                             padding: 0px;
                             height: 0px;'>Consumer Sentiment Transition Matrix</h4>"),
                div(
                    style = "
                                display: flex;
                                justify-content: center;
                                align-items: flex-start;   /* or center */
                                height: 100px;             /* adjust container height */
                                padding-top: 10px;         /* optional: add some top padding */
                              ",
                    HTML(true_transition_matrix)
                )
            )
        )
    })
    
    
}

# ----- Run App -----
shinyApp(ui, server)