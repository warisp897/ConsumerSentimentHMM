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
library(rhandsontable)

library(ggplot2)
library(depmixS4)

# SHOULD NOT BE NEEDED

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
    <b> A Hidden Markov Model (HMM) </b> is a statistical tool used to understand systems where you can't directly 
    observe the state of the system, but you can observe evidence or signals that are influenced by that state.
    </p>
    
    <p>
    Think of it like trying to guess the weather (a hidden state) by only knowing the temperature (an observation). You can't see if it's Sunny, 
    Cloudy, or Rainy from inside a windowless room, but if you're told the temperature is Hot, you can make a good guess that it's probably Sunny.
    </p>
    
    <p>
    The model has two layers: the unobservable hidden states (the weather types at the top of the diagram) and the observations we can see 
    (the temperatures at the bottom). The model assumes that the sequence of hidden states follows a specific type of random process 
    (a Markov chain), where the next state only depends on the current state.
    </p>
    
    <p>
    The primary goal of an HMM is to infer the most likely sequence of hidden states given a sequence of observations. 
    For example, if you observe a temperature pattern of Hot, Mild, Cold over three days, the HMM can calculate 
    the most probable weather sequence, such as Sunny, Cloudy, Rainy.
    </p>
    
    <p>
    The model trains by analyzing a large set of historical data where both the states and observations are known. 
    It learns the probabilities that connect these states and observations, which are the core components of the model.
    </p>
    
    </div>"
)

initial_state_selected <- HTML(
    "<div style='font-size:17px; line-height:1.6;'>
  <p>
   The <b> Initial State </b> represents the starting point of the model. It defines the probability of the system being in 
   each of the hidden states at the very beginning (at time zero), before any observations have been made.
  </p>
  
  <p>
  It answers the question: \"Without any other information, what is the likelihood of today's weather being Sunny, Cloudy, or Rainy?\" 
  For example, if you live in a desert, the initial probability for Sunny would be very high.
  </p>
  
  <p>
  In the diagram, the Initial State node points to each weather state. 
  The probabilities associated with these arrows (....) represent the chances that our very first hidden state is Sunny, Cloudy, or Rainy, 
  respectively. These probabilities must always add up to 1 (or 100%).
  </p>
  
</div>"
)

emission_prob <- HTML(
    "<div style='font-size:17px; line-height:1.6;'>
  <p>
    <b> Emission Probabilities </b> link the hidden states to the observations. They represent the probability of 
    seeing a particular observation given that the system is in a specific hidden state.
  </p>
  
  <p>
  This is the crucial link that allows us to infer the hidden state. For example, given that it is a Rainy day, 
  what is the probability that the temperature is Cold? This is an emission probability. 
  It's unlikely that the temperature would be Hot on a Rainy day, so that probability would be very low.
  </p>
  
  <p>
  These are also learned from historical data. For all the times the state was sunny, the model 
  calculates the proportion of times the observation were hot, mild, and cold.
  </p>
  
  <p>
  In the diagram, these are the dashed arrows pointing from the hidden weather states down to the observed temperature states. The label (...)
  represents the probability of observing hot temperature when the hidden state is Sunny.
  </p>
</div>"
)

transition_prob  <- HTML(
    "<div style='font-size:17px; line-height:1.6;'>
  <p>
    <b> Transition Probabilities </b> govern how the hidden states change over time. 
    They are the probabilities of moving from one hidden state to another in the next time step.
  </p>
  
  <p>
   These probabilities model the dynamics of the system. For instance, if the weather is sunny today, 
   what is the probability that it will be cloudy tomorrow? That's a transition probability. A system 
   often has inertia, meaning it's more likely to stay in its current state (e.g., Sunny -> Sunny).
  </p>
  
  <p>
  These are learned from historical data by counting how often each state follows another. For example, 
  the model would count how many times a sunny day was followed by a rainy day in the dataset to calculate 
  that specific probability.
  </p>
  
  <p>
  In the diagram, these are the curved arrows connecting the weather states. The label represents the probability 
  of transitioning from State 1 (Sunny) to State 2 (Cloudy). The probabilities in each row of the matrix below must sum to 1.
  </p>
  
</div>"
)

hmm_training  <- HTML(
    "<div style='font-size:16px; line-height:1.6;'>
  <p>
  The model is trained to find the set of internal probabilities that best explains the sequence of observations provided. 
  The goal of the training process is to maximize a value called the <b> log-likelihood.</b> This is a statistical measure of how well the 
  model's parameters (its initial state, transition, and emission probabilities) explain the observed data. A higher log-likelihood 
  score means the model provides a more plausible explanation for the data sequence provided. The model adjusts its internal probabilities 
  until this score is as high as it can be.
  </p>
  
  <p>
  To achieve this, the model uses a powerful algorithm called Expectation-Maximization (EM), which for HMMs is known as the Baum-Welch algorithm. 
  It's designed to solve a classic \"chicken-and-egg\" problem: to know the right probabilities, we need to know the sequence of hidden states, 
  but to know the hidden states, we need the right probabilities. The EM algorithm solves this by starting with a random guess and then 
  repeating a two-step process to gradually improve its solution.
  </p>
  
  <ol>
    <li> <b> The E-Step (Expectation): </b> First, using its current (initially random) set of probabilities, the model analyzes the data and calculates 
    the expected probability of being in each hidden state (e.g., Sunny, Cloudy) at every point in time. It doesn't decide the state was 
    definitely Sunny; instead, it might assign a 70% chance of it being sunny and a 30% chance of it being cloudy. This creates a complete 
    probabilistic map of the hidden states. </li>
    
    <li> <b> The M-Step (Maximization): </b> Next, using this new probabilistic map of the hidden states, the model updates its transition and 
    emission probabilities to maximize the likelihood of that map. For example, it will adjust the Sunny to Cloudy transition probability 
    based on the weighted likelihood of all such transitions occurring in the data. This newly calculated set of probabilities is guaranteed 
    to be a better fit for your data.</li>
  </ol>
  
  <p>
  The model repeats this E-Step and M-Step cycle over and over. With each cycle, the log-likelihood score improves, 
  and the model's parameters get closer to the optimal solution. The process stops when the improvement between cycles becomes very small, 
  a state known as convergence. At this point, the model has found a stable and mathematically optimized set of probabilities 
  that best describes the underlying patterns in the data.
  </p>
  
</div>"
)

emissions_mat <- '
<div style="font-size:250%; text-align:center;">
  $$\n\\begin{bmatrix}
     b_{1}(hot) & b_{2}(hot) & b_{3}(hot) \\\\\n
     b_{1}(mild) & b_{2}(mild) & b_{3}(mild) \\\\\n
     b_{1}(cold) & b_{2}(cold) & b_{3}(cold)
   \\end{bmatrix}
  $$ 
  <table class="matrix-overlay" style="position:absolute; top:50%; left:50%;
                                        transform: translate(-50%, -50%);
                                        border:0; border-spacing:0;">
    <tr>
      <td><span id="b1hot" class="matrix-cell"></span></td>
      <td><span id="b2hot" class="matrix-cell"></span></td>
      <td><span id="b3hot" class="matrix-cell"></span></td>
    </tr>
    <tr>
      <td><span id="b1mild" class="matrix-cell"></span></td>
      <td><span id="b2mild" class="matrix-cell"></span></td>
      <td><span id="b3mild" class="matrix-cell"></span></td>
    </tr>
    <tr>
      <td><span id="b1cold" class="matrix-cell"></span></td>
      <td><span id="b2cold" class="matrix-cell"></span></td>
      <td><span id="b3cold" class="matrix-cell"></span></td>
    </tr>
  </table>
</div>'

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
                                       id = "consumer_sent_tabs",
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
            
            #Hidden Markov Model Tab
            bs4TabItem(
                tabName = "model_intro",

                # Top explanatory card
                fluidRow(
                    column(
                        width = 5,
                        bs4Card(
                            collapsible = FALSE,
                            closable = FALSE,
                            title = "What is a Hidden Markov Model?",
                            width = 12,
                            status = "success",
                            solidHeader = TRUE,
                            #"",
            bs4Dash::tabsetPanel(
                id = "hmm_info_tabs",
                type = "pills",
                selected = "hmm_intro",
                tabPanel("Introduction", value = "hmm_intro", intro_hmm_text),
                tabPanel("Model Training", value = "hmm_train_exp", hmm_training),
                tabPanel("Initial State", value = "init_state", initial_state_selected),
                tabPanel("Transition Probability", value = "trans_prob",
                         div(style = "height:636px;",
                            transition_prob,
                            uiOutput("matrix_ui")
                         )
                ),
                tabPanel("Emission Probability", value = "emission_prob", 
                         div(style = "height:636px;",
                             emission_prob,
                             uiOutput("emissions_prob_mat"))
                        )
                    )
                )
            ),
            
            column(
                width = 7,
                div(
                    style = "flex: 2; display: flex; justify-content: center;",
                    bs4Card(
                        collapsible = FALSE,
                        closable = FALSE,
                        title = "Model Diagram",
                        width = 12,
                        status = "success",
                        solidHeader = TRUE,
                        #visNetworkOutput("hmm_vis", height = "700px", width = "100%")
                        #hmm_interactive_ui(id = "my_interactive_hmm")
                        sliderInput("hmm_T", "Sequence length", min = 15, max = 100, value = 30, step = 1),
                        actionButton("hmm_run_demo", "Generate & Fit (EM)", class = "btn btn-success btn-block"),
                        highchartOutput("hmm_demo_timeline", height = 280),
                        tableOutput("hmm_demo_metrics"),
                        column(
                            width = 4,
                            rHandsontableOutput("A_tbl", height = 180),
                            rHandsontableOutput("B_tbl", height = 180)
                        )
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
                            "" 
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
    
    observeEvent(input$hmm_info_tabs, ignoreInit = TRUE, {
        t_prob <- identical(input$hmm_info_tabs, "trans_prob")
        e_prob <- identical(input$hmm_info_tabs, "emission_prob")
        session$sendCustomMessage("hmmFlags", list(t_prob = t_prob, e_prob = e_prob))
        session$sendCustomMessage("redrawNet", list())
    })
    
    nodes <- data.frame(
        id   = c("Initial","S1","S2","S3"),
        label= c("Initial State","Sunny","Cloudy","Rainy"),
        shape= c("circle","icon","icon","icon"),
        x    = c(   0, -300,   0,  300),
        y    = c(-275,  -50, -50,  -50),
        size = c(110,   NA,  NA,   NA),
        font.size = 18,
        color = "#3EB489",              # initial circle border
        title = "Hidden Markov Model Diagram",
        stringsAsFactors = FALSE,
        font.size = 28,                # makes the box bigger because text is bigger
        margin    = NA                # padding inside the box (increase box size)
    )
    
    # Font Awesome icons (FA5)
    fa_codes <- c(S1 = "f185", S2 = "f0c2", S3 = "f73d")  # sun, cloud, cloud-rain
    mask <- nodes$id %in% names(fa_codes)
    
    nodes$icon.face  <- NA; nodes$icon.code  <- NA
    nodes$icon.size  <- NA; nodes$icon.color <- NA; nodes$icon.weight <- NA
    
    nodes$icon.face[mask]   <- "'Font Awesome 5 Free'"
    nodes$icon.code[mask]   <- unname(fa_codes[nodes$id[mask]])
    nodes$icon.weight[mask] <- 900
    
    # tuned icon sizes and colors
    nodes$icon.size[nodes$id=="S1"]  <- 96
    nodes$icon.size[nodes$id=="S2"]  <- 86
    nodes$icon.size[nodes$id=="S3"]  <- 100
    
    state_colors <- c(S1="#F6C54E", S2="#7A7A7A", S3="#4DA3FF")
    
    nodes$icon.color[nodes$id=="S1"] <- state_colors["S1"]  # sun (yellow)
    nodes$icon.color[nodes$id=="S2"] <- state_colors["S2"]  # cloud (grey)
    nodes$icon.color[nodes$id=="S3"] <- state_colors["S3"]  # rain (blue)
    
    # Nodes
    S <- c("S1","S2","S3")
    O <- c("O_hot","O_mild","O_cold")
    
    from_tr  <- c(rep("Initial", length(S)), rep(S, each = length(S)))
    to_tr    <- c(S,                        rep(S, times = length(S)))
    label_tr <- c("P₁","P₂","P₃",
                  "P₁₁","P₁₂","P₁₃",
                  "P₂₁","P₂₂","P₂₃",
                  "P₃₁","P₃₂","P₃₃")
    
    ## emissions: 9 (every S* -> every O_*)
    from_em <- rep(S, each = length(O))
    to_em   <- rep(O, times = length(S))
    
    from  <- c(from_tr, from_em)
    to    <- c(to_tr,   to_em)                  
    label <- c(label_tr, rep(NA_character_, length(from_em)))
    
    is_emission <- to %in% O
    is_initial  <- from == "Initial"
    
    ## Per edge attributes
    arrows    <- rep("to", length(from))
    dashes    <- is_emission    
    color <- ifelse(
        is_initial, 
        "#000000", 
        ifelse(
            is_emission, 
            state_colors[from],   # assumes `col` is your mapping: e.g., col <- c(S1="#F6C54E", S2="#7A7A7A", S3="#4DA3FF")
            NA_character_
        )
    )
    width     <- rep(2, length(from))
    font.size <- ifelse(is_emission, 14, NA_real_)
    
    ## ---- smooth list-col (single pass) ----
    smooth_rule <- function(f, t) {
        if (t %in% O || f == "Initial") return(FALSE)                               # straight
        if (f == t)                         return(list(enabled=TRUE,type="curvedCW",roundness=0.30))
        if (f == "S3" && t == "S1")         return(list(enabled=TRUE,type="curvedCW",roundness=0.25))
        return(list(enabled=TRUE,type="curvedCW",roundness=0.20))
    }
    smooth <- mapply(smooth_rule, from, to, SIMPLIFY = FALSE)
    
    edges <- data.frame(
        id        = paste0(from, "->", to),
        from      = from,
        to        = to,
        label     = label,
        arrows    = arrows,
        dashes    = dashes,
        color     = color,
        width     = width,
        font.size = font.size,
        stringsAsFactors = FALSE
    )
    edges$smooth <- smooth

    edges$endPointOffset <- vector("list", nrow(edges))
    edges$endPointOffset[edges$id == "Initial->S1"] <- list(list(from=14L, to=18L))
    edges$endPointOffset[edges$id == "Initial->S2"] <- list(list(from=14L, to=20L))
    edges$endPointOffset[edges$id == "Initial->S3"] <- list(list(from=14L, to=18L))
    
    
    #temp disable edge labels
    edges$label <- NULL

    # Observation nodes
    obs_nodes <- data.frame(
        id         = c("O_hot","O_mild","O_cold"),
        label      = c("Hot","Mild","Cold"),
        size       = 100,
        font.size  = 16,
        shape      = "box",
        x          = c(-300,   0,  300), 
        y          = c(100,   100, 100),
        color      = "#666666",
        title      = NA_character_,
        stringsAsFactors = FALSE,
        font.size = 28,                # makes the box bigger because text is bigger
        margin    = 20                # padding inside the box (increase box size)
    )
    
    fa_obs <- c(O_sun="f185", O_cloud="f0c2", O_rain="f73d")
    obs_nodes$icon.face   <- "'Font Awesome 5 Free'"
    obs_nodes$icon.code   <- unname(fa_obs[obs_nodes$id])
    obs_nodes$icon.weight <- 900
    obs_nodes$icon.size   <- 80
    obs_nodes$icon.color  <- c("#F6C54E","#7A7A7A","#4DA3FF")
    
    nodes <- rbind(nodes, obs_nodes)
    
    
    output$hmm_vis <- renderVisNetwork({
        visNetwork(nodes, edges) %>%
            addFontAwesome(version = "5.13.0") %>%
            visPhysics(enabled = FALSE) %>%
            visEdges(
                color  = list(color="rgba(0,0,0,0)",      # invisible by default; per-edge overrides show
                              hover="rgba(0,0,0,0)",
                              highlight="rgba(0,0,0,0)"),
                smooth = list(enabled=TRUE),
                arrowStrikethrough = FALSE,
                endPointOffset = list(from=0, to=10),
                selfReference   = list(size=30, angle=0.35, renderBehindTheNode=TRUE),
                font = list(size=20, background="rgba(255,255,255,0.96)", strokeWidth=0, vadjust=-6),
                dashes = T
            ) %>%
            # generate all gradient arrows
            visEvents(beforeDrawing = htmlwidgets::JS("
                function(ctx){
                  var net = this;
                
                  // --- colors for gradient & loops ---
                  var col = { S1:'#F6C54E', S2:'#7A7A7A', S3:'#4DA3FF' };
                
                  // --- state→state tweak table (control offset + trim) ---
                  var s_width = 2
                  var T = {
                    'S1->S2': {dx:0,  dy:-20, t0:0.10, t1:0.85, w:s_width},
                    'S2->S1': {dx:0,  dy:30, t0:0.08, t1:0.80, w:s_width},
                    'S1->S3': {dx:30, dy:-150, t0:0.05, t1:0.90, w:s_width},
                    
                    'S3->S1': {dx:10, dy:70,  t0:0.0, t1:0.91, w:s_width},
                    'S2->S3': {dx:0,  dy:-20,  t0:0.10, t1:0.82, w:s_width},
                    'S3->S2': {dx:0,  dy:30,   t0:0.0, t1:0.8, w:s_width}
                  };
                
                    function mkLoop(id, dy, ang){
                      var nd = net.body.nodes[id];
                      var sz = (nd && nd.options && nd.options.icon && nd.options.icon.size) ? nd.options.icon.size : 90;
                      var r  = sz/2 + 0;      // radius ~ icon radius + margin
                      var dx = sz/2 + 8;       // loop center to the right of node
                        return { 
                            dx: dx,                // horizontal shift of loop center
                            dy: (dy ?? -5),        // vertical shift of loop center
                            r: r,                  // loop radius
                            a0:210,                // start angle (degrees)
                            a1:450,                // end angle (degrees) → controls how “round” it is
                            w:2,                   // stroke width
                            tipBackPx: 9,          // how far back the loop stops before arrowhead
                            ang: (ang || 0)        // small manual rotation of arrowhead direction
                          };
                    }
                  var Lp = {
                    'S1->S1': mkLoop('S1', -5, -5),  // rotate ~1.2° clockwise
                    'S2->S2': mkLoop('S2', -5,  -5),  // rotate ~0.8° CCW
                    'S3->S3': mkLoop('S3', -5,  -5)
                  };
                
                  // --- nudge the label anchor for P13 (move vis's underlying via) ---
                  var labelNudge = { 'S1->S3': {dx: 4, dy: -4} };
                  var esData = net.body.data.edges.get();
                  window.__baseVia = window.__baseVia || {};
                  esData.forEach(function(e){
                    var nud = labelNudge[e.id]; if(!nud) return;
                    var eo = net.body.edges[e.id]; if(!eo) return;
                
                    if(!window.__baseVia[e.id]){
                      if(eo.via){
                        window.__baseVia[e.id] = {x:eo.via.x, y:eo.via.y};
                      } else {
                        var p = net.getPositions([e.from,e.to]), p0=p[e.from], p2=p[e.to];
                        window.__baseVia[e.id] = {x:(p0.x+p2.x)/2, y:(p0.y+p2.y)/2 - 0.001};
                        eo.options.smooth = {enabled:true,type:'curvedCW',roundness:0.001};
                      }
                    }
                    var b = window.__baseVia[e.id];
                    eo.via = {x:b.x+(nud.dx||0), y:b.y+(nud.dy||0)};
                  });
                
                  // --- helpers (minimal) ---
                  function B(t,p0,p1,p2){ var mt=1-t; return {
                    x: mt*mt*p0.x + 2*mt*t*p1.x + t*t*p2.x,
                    y: mt*mt*p0.y + 2*mt*t*p1.y + t*t*p2.y
                  }; }
                  function ctrl(p0,p2,opt){
                    var dx=p2.x-p0.x, dy=p2.y-p0.y, L=Math.hypot(dx,dy)||1, ux=dx/L, uy=dy/L;
                    var mx=(p0.x+p2.x)/2, my=(p0.y+p2.y)/2;
                    var depth = 0.60*L * ((opt&&opt.roundness)?opt.roundness:0.20);
                    var sgn   = (opt&&opt.type==='curvedCCW') ? 1 : -1;
                    return { x: mx + sgn*depth*(-uy), y: my + sgn*depth*(ux) };
                  }
                  function gradAtTip(p0,p2,tip,c0,c1){
                    function H(h){h=h.replace('#',''); if(h.length===3) h=h[0]+h[0]+h[1]+h[1]+h[2]+h[2];
                      var n=parseInt(h,16); return {r:(n>>16)&255,g:(n>>8)&255,b:n&255};}
                    function X(r,g,b){var z=x=>('0'+x.toString(16)).slice(-2); return '#'+z(r)+z(g)+z(b);}
                    var dx=p2.x-p0.x, dy=p2.y-p0.y, den=dx*dx+dy*dy||1;
                    var t=((tip.x-p0.x)*dx + (tip.y-p0.y)*dy)/den; t=Math.max(0,Math.min(1,t));
                    var a=H(c0), b=H(c1);
                    return X(Math.round(a.r+(b.r-a.r)*t), Math.round(a.g+(b.g-a.g)*t), Math.round(a.b+(b.b-a.b)*t));
                  }
                  function drawHead(ctx, tip, dir, fill, w){
                    var len=16, bw=12, round=-0.70, oCol='#000', oW=1.0, baseSeg=Math.max(5, 0.5*w);
                    var L=Math.hypot(dir.x,dir.y)||1, ux=dir.x/L, uy=dir.y/L, nx=-uy, ny=ux;
                    var bx=tip.x-ux*len, by=tip.y-uy*len, hw=bw/2;
                    var Lp={x:bx-nx*hw, y:by-ny*hw}, Rp={x:bx+nx*hw, y:by+ny*hw}, Cp={x:bx-ux*(round*hw), y:by-uy*(round*hw)};
                    
                    // fill
                    ctx.beginPath(); ctx.moveTo(tip.x, tip.y);
                    ctx.lineTo(Lp.x, Lp.y); ctx.quadraticCurveTo(Cp.x, Cp.y, Rp.x, Rp.y); ctx.closePath();
                    ctx.fillStyle=fill; ctx.fill();
                    
                    // side outlines + short base hints (no seam over shaft)
                    ctx.save(); ctx.lineJoin='round'; ctx.miterLimit=2; ctx.strokeStyle=oCol; ctx.lineWidth=oW;
                    ctx.beginPath(); ctx.moveTo(tip.x, tip.y); ctx.lineTo(Lp.x, Lp.y); ctx.stroke();
                    ctx.beginPath(); ctx.moveTo(tip.x, tip.y); ctx.lineTo(Rp.x, Rp.y); ctx.stroke();
                    function shortSeg(P0,P1,lenPx){ var vx=P1.x-P0.x, vy=P1.y-P0.y, L=Math.hypot(vx,vy)||1, ux=vx/L, uy=vy/L;
                      ctx.beginPath(); ctx.moveTo(P0.x,P0.y); ctx.lineTo(P0.x+ux*lenPx, P0.y+uy*lenPx); ctx.stroke(); }
                    shortSeg(Lp, {x:Cp.x,y:Cp.y}, baseSeg); shortSeg(Rp, {x:Cp.x,y:Cp.y}, baseSeg);
                    ctx.restore();
                  }
                
                  // --- draw gradients & loops BEFORE labels so labels stay on top ---
                  var pos = net.getPositions();
                  var all = net.body.data.edges.get();
                
                  // state→state gradient shafts + heads
                  all.forEach(function(e){
                    var sf=['S1','S2','S3'].indexOf(e.from)>=0, st=['S1','S2','S3'].indexOf(e.to)>=0;
                    if(!sf || !st || e.from===e.to) return;
                    var p0=pos[e.from], p2=pos[e.to]; if(!p0||!p2) return;
                    var eo=net.body.edges[e.id];
                    var via=(eo&&eo.via)?{x:eo.via.x,y:eo.via.y}:ctrl(p0,p2,e.smooth);
                    var tw=T[e.id]||{dx:0,dy:0,t0:0.08,t1:0.92,w:3};
                    via.x+=(tw.dx||0); via.y+=(tw.dy||0);
                
                    var t0=tw.t0, t1=tw.t1, W=tw.w, steps=48, dt=(t1-t0)/steps;
                
                    // path
                    var p=B(t0,p0,via,p2); ctx.beginPath(); ctx.moveTo(p.x,p.y);
                    for(var tt=t0+dt; tt<=t1+1e-6; tt+=dt){ p=B(tt,p0,via,p2); ctx.lineTo(p.x,p.y); }
                
                    // outline then gradient stroke
                    ctx.save(); ctx.lineCap='round'; ctx.lineJoin='round';
                    ctx.strokeStyle='#000'; ctx.lineWidth=W+1.7; ctx.stroke();
                    var g=ctx.createLinearGradient(p0.x,p0.y,p2.x,p2.y);
                    g.addColorStop(0, col[e.from]||'#3EB489'); g.addColorStop(1, col[e.to]||'#3EB489');
                    ctx.strokeStyle=g; ctx.lineWidth=W; ctx.stroke(); ctx.restore();
                
                    // --- head at tip ---
                    var dtBack = Math.min(0.12, 6 / (Math.hypot(p2.x - p0.x, p2.y - p0.y) || 1));  // ~6px in t-space
                    var tBack  = Math.max(t0 + 1e-3, t1 - dtBack);
                    
                    var back = B(tBack, p0, via, p2);
                    var tip  = B(t1,    p0, via, p2);
                    
                    var dirx = tip.x - back.x, diry = tip.y - back.y;
                    
                    // nudge the head a couple of pixels forward so it kisses the stroke end
                    var len  = Math.hypot(dirx, diry) || 1;
                    var shiftPx = 5;              // tweak 1.5–3.0 if needed
                    tip.x += shiftPx * (dirx / len);
                    tip.y += shiftPx * (diry / len);
                    
                    // fill = gradient color at the (shifted) tip
                    var fill = gradAtTip(p0, p2, tip, col[e.from] || '#3EB489', col[e.to] || '#3EB489');
                    
                    drawHead(ctx, tip, {x: dirx, y: diry}, fill, W);

                  });
                
                  // self-loops (size-aware) with back-off so heads sit on the arc
                  ['S1','S2','S3'].forEach(function(s){
                    var p=pos[s]; if(!p) return;
                    var t=Lp[s+'->'+s]; var cx=p.x+t.dx, cy=p.y+t.dy, r=t.r, lw=t.w;
                    var a0=t.a0*Math.PI/180, a1=t.a1*Math.PI/180;
                    var back = Math.min(0.5, (t.tipBackPx||9)/r);  // radians
                    var aEnd = a1 - back;
                
                    ctx.save(); ctx.lineCap='round';
                    ctx.beginPath(); ctx.arc(cx,cy,r,a0,aEnd,false);
                    ctx.strokeStyle='#000'; ctx.lineWidth=lw+1.7; ctx.stroke();
                    ctx.strokeStyle=col[s]||'#3EB489'; ctx.lineWidth=lw; ctx.stroke();
                    ctx.restore();
                
                    // head at end angle
                    var tip = { x: cx + r*Math.cos(a1), y: cy + r*Math.sin(a1) };
                    
                    // tangent at a1
                    var tx = -Math.sin(a1), ty = Math.cos(a1);
                    
                    // optional per-loop micro-rotation (degrees)
                    var key = s + '->' + s;
                    var angDeg = (Lp[key] && Lp[key].ang) || 0;
                    if (angDeg) {
                      var a = angDeg * Math.PI / 180, ca = Math.cos(a), sa = Math.sin(a);
                      var rx = tx*ca - ty*sa, ry = tx*sa + ty*ca;
                      tx = rx; ty = ry;
                    }

                    // draw head with rotated direction
                    drawHead(ctx, tip, { x: tx, y: ty }, col[s] || '#3EB489', lw);
                    
                    
                    //var tip={x:cx+r*Math.cos(a1), y:cy+r*Math.sin(a1)};
                    //var dir={x:-Math.sin(a1), y: Math.cos(a1)};
                    //drawHead(ctx, tip, dir, col[s]||'#3EB489', lw);
                  });
                }
            ")) %>%
            
            # Drawing labels and slightly adjusting vis canvas placement
            visEvents(afterDrawing = htmlwidgets::JS("
                function(ctx){
                  var net = this;
                
                  // expose network for redraws
                  window.__vn = this;
                
                  // one-time wiring for flag-driven redraw
                  if (!window.__hmmFlagsWired){
                    window.__hmmFlagsWired = true;
                    window.hmmFlags = { t_prob:false, e_prob:false };
                
                    Shiny.addCustomMessageHandler('hmmFlags', function(flags){
                      window.hmmFlags = flags || { t_prob:false, e_prob:false };
                      if (window.__vn && typeof window.__vn.redraw === 'function') window.__vn.redraw();
                    });
                
                    Shiny.addCustomMessageHandler('redrawNet', function(){
                      if (window.__vn && typeof window.__vn.redraw === 'function') window.__vn.redraw();
                    });
                  }
                
                  if (!this.__movedOnce) {
                    this.__movedOnce = true;
                    this.moveTo({ position: { x: 0, y: -60 }, scale: 1.05, animation: false });
                  }
                
                  // ---- CUSTOM EDGE LABELS (manual placement) ----
                  (function(){
                    var net  = this;
                    var ctx  = arguments[0];
                    var pos  = net.getPositions();
                    var edges= net.body.data.edges.get();
                
                    // helpers
                    function roundRect(ctx, x, y, w, h, r){
                      var rr = Math.min(r, Math.min(w, h)/2);
                      ctx.beginPath();
                      ctx.moveTo(x+rr, y);
                      ctx.arcTo(x+w, y,   x+w, y+h, rr);
                      ctx.arcTo(x+w, y+h, x,   y+h, rr);
                      ctx.arcTo(x,   y+h, x,   y,   rr);
                      ctx.arcTo(x,   y,   x+w, y,   rr);
                      ctx.closePath();
                    }
                    function B(t,p0,p1,p2){ var mt=1-t; return {
                      x: mt*mt*p0.x + 2*mt*t*p1.x + t*t*p2.x,
                      y: mt*mt*p0.y + 2*mt*t*p1.y + t*t*p2.y
                    }; }
                    function controlPoint(p0,p2,opt){
                      var dx=p2.x-p0.x, dy=p2.y-p0.y, L=Math.hypot(dx,dy)||1, ux=dx/L, uy=dy/L;
                      var mx=(p0.x+p2.x)/2, my=(p0.y+p2.y)/2;
                      var depth = 0.60*L * ((opt&&opt.roundness)?opt.roundness:0.20);
                      var sgn   = (opt&&opt.type==='curvedCCW') ? 1 : -1;
                      return { x: mx + sgn*depth*(-uy), y: my + sgn*depth*(ux) };
                    }
                    function drawLabel(x, y, txt, opt){
                      opt = opt || {};
                      var fs = opt.size || 18;
                      ctx.save();
                      ctx.font = (opt.bold?'600 ':'') + fs + 'px Helvetica, Arial';
                      ctx.textAlign = 'center';
                      ctx.textBaseline = 'middle';
                      var pad = (opt.pad!=null?opt.pad:4), rad = (opt.radius!=null?opt.radius:4);
                      var w = ctx.measureText(txt).width + 2*pad;
                      var h = fs*1.15 + 2*pad;
                      var bx = x - w/2, by = y - h/2;
                      ctx.fillStyle   = opt.bg    || 'rgba(255,255,255,0.98)';
                      ctx.strokeStyle = opt.border|| 'rgba(0,0,0,0)';
                      ctx.lineWidth   = opt.borderWidth || 0;
                      roundRect(ctx, bx, by, w, h, rad);
                      ctx.fill();
                      if (ctx.lineWidth>0) ctx.stroke();
                      ctx.fillStyle = opt.color || '#2b2b2b';
                      ctx.fillText(txt, x, y);
                      ctx.restore();
                    }
                
                    // transition labels
                    var edgeLabelsTrans = {
                      'Initial->S1': { text: 'P₁',   t: 0.55, dx:  15, dy: -35 },
                      'Initial->S2': { text: 'P₂',   t: 0.55, dx: -10, dy: -14 },
                      'Initial->S3': { text: 'P₃',   t: 0.55, dx: -25, dy:   5 },
                
                      'S1->S1':      { text: 'P₁₁',  x: pos.S1.x + 35, y: pos.S1.y - 65 },
                      'S1->S2':      { text: 'P₁₂',  t: 0.42, dx:  90, dy: -10 },
                      'S1->S3':      { text: 'P₁₃',  t: 0.50, dx: -40, dy: -70 },
                
                      'S2->S1':      { text: 'P₂₁',  t: 0.55, dx:  80, dy:  14 },
                      'S2->S2':      { text: 'P₂₂',  x: pos.S2.x + 38, y: pos.S2.y - 40 },
                      'S2->S3':      { text: 'P₂₃',  t: 0.55, dx: -20, dy: -10 },
                
                      'S3->S1':      { text: 'P₃₁',  t: 0.52, dx:  10, dy:  38 },
                      'S3->S2':      { text: 'P₃₂',  t: 0.52, dx:   0, dy:  14 },
                      'S3->S3':      { text: 'P₃₃',  x: pos.S3.x + 52, y: pos.S3.y - 44 }
                    };
                
                    // emission labels (example placeholders)
                    var edgeLabelsEmit = {
                      'S1->O_hot':   { text: 'b₁(hot)',  t: 0.50, dx: -10, dy:  8 },
                      'S1->O_mild':  { text: 'b₁(mild)', t: 0.50, dx:   0, dy:  8 },
                      'S1->O_cold':  { text: 'b₁(cold)', t: 0.50, dx:  10, dy:  8 },
                
                      'S2->O_hot':   { text: 'b₂(hot)',  t: 0.50, dx: -10, dy:  8 },
                      'S2->O_mild':  { text: 'b₂(mild)', t: 0.50, dx:   0, dy:  8 },
                      'S2->O_cold':  { text: 'b₂(cold)', t: 0.50, dx:  10, dy:  8 },
                
                      'S3->O_hot':   { text: 'b₃(hot)',  t: 0.50, dx: -10, dy:  8 },
                      'S3->O_mild':  { text: 'b₃(mild)', t: 0.50, dx:   0, dy:  8 },
                      'S3->O_cold':  { text: 'b₃(cold)', t: 0.50, dx:  10, dy:  8 }
                    };
                
                    // decide which set to draw
                    var flags = window.hmmFlags || { t_prob:false, e_prob:false };
                    if (!flags.t_prob && !flags.e_prob) return;
                    var labelSet = flags.t_prob ? edgeLabelsTrans : edgeLabelsEmit;
                
                    // draw labels
                    edges.forEach(function(e){
                      var key = e.id || (e.from + '->' + e.to);
                      var def = labelSet[key];
                      if (!def) return;
                
                      var x, y;
                      if (typeof def.x === 'number' && typeof def.y === 'number'){
                        x = def.x; y = def.y;
                      } else if (typeof def.t === 'number'){
                        var p0 = pos[e.from], p2 = pos[e.to];
                        if(!p0 || !p2) return;
                        var eo  = net.body.edges[e.id];
                        var via = (eo && eo.via) ? eo.via : controlPoint(p0,p2, e.smooth);
                        var P   = B(Math.max(0, Math.min(1, def.t)), p0, via, p2);
                        x = P.x + (def.dx||0);
                        y = P.y + (def.dy||0);
                      } else {
                        return;
                      }
                
                      drawLabel(x, y, def.text, {
                        size: 15,
                        bold: true,
                        bg: 'rgba(255,255,255,0.98)',
                        border: '#000000',
                        borderWidth: 1.0
                      });
                    });
                
                    // console helper
                    window.setEdgeLabel = function(id, obj){
                      var target = ( (window.hmmFlags||{}).t_prob ? edgeLabelsTrans : edgeLabelsEmit );
                      target[id] = Object.assign(target[id]||{}, obj||{});
                      net.redraw();
                    };
                  }).apply(this, arguments);
                }
                ")) %>%
            visNodes(fixed = TRUE) %>%
            visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
            visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)
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
                            //padding-top: 10px;         /* optional: add some top padding */
                          ",
                    HTML(pre_rendered_matrix)
                )
            )
        )
    })
    
    output$emissions_prob_mat <- renderUI({
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
                            //padding-top: 10px;         /* optional: add some top padding */
                          ",
                    HTML(emissions_mat)
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
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    .norm  <- function(v) { s <- sum(v); if (!is.finite(s) || s<=0) rep(1/length(v), length(v)) else v/s }
    
    # defaults (tweak if you like)
    pi0 <- c(0.6, 0.3, 0.1)
    A0  <- matrix(c(0.75,0.20,0.05,
                    0.20,0.65,0.15,
                    0.10,0.20,0.70), 3, byrow=TRUE)
    B0  <- matrix(c(0.70,0.20,0.10,   # S1 -> Hot/Mild/Cold
                    0.20,0.60,0.20,   # S2 -> …
                    0.10,0.25,0.65), 3, byrow=TRUE)
    
    # reactives with safe fallbacks
    R_pi <- reactive({
        .norm(c(input$hmm_pi1 %||% pi0[1],
                input$hmm_pi2 %||% pi0[2],
                input$hmm_pi3 %||% pi0[3]))
    })
    
    .norm <- function(v){ s <- sum(v, na.rm=TRUE); if(!is.finite(s) || s<=0) rep(1/length(v), length(v)) else v/s }
    
    make_mat_df <- function(M, rowlabs, collabs){
        df <- as.data.frame(M); names(df) <- collabs; rownames(df) <- rowlabs; df
    }
    
    # initial values you already have (A0, B0)
    output$A_tbl <- renderRHandsontable({
        rhandsontable(round(make_mat_df(A0, c("S1","S2","S3"), c("S1","S2","S3")), 2),
                      rowHeaders = TRUE, stretchH = "all") %>%
            hot_validate_numeric(cols = 1:3, min = 0, max = 1, allowInvalid = TRUE) %>%
            hot_cols(format = "0.00")
    })
    
    output$B_tbl <- renderRHandsontable({
        rhandsontable(round(make_mat_df(B0, c("S1","S2","S3"), c("Hot","Mild","Cold")), 2),
                      rowHeaders = TRUE, stretchH = "all") %>%
            hot_validate_numeric(cols = 1:3, min = 0, max = 1, allowInvalid = TRUE) %>%
            hot_cols(format = "0.00")
    })
    
    A_values <- reactiveVal(A0)
    B_values <- reactiveVal(B0)
    
    observeEvent(input$A_tbl, {
        M <- as.matrix(hot_to_r(input$A_tbl))
        M[is.na(M)] <- 0
        A_values(t(apply(M, 1, .norm)))
    })
    
    observeEvent(input$B_tbl, {
        M <- as.matrix(hot_to_r(input$B_tbl))
        M[is.na(M)] <- 0
        B_values(t(apply(M, 1, .norm)))
    })
    
    # Plug these into your HMM reactives:
    R_A <- reactive(A_values())
    R_B <- reactive(B_values())
    
    draw_cat <- function(p) sample.int(length(p), 1L, prob = p)
    
    demo_data <- eventReactive(input$hmm_run_demo, {
        # Use your existing parameter sources
        pi <- R_pi()     # numeric length 3
        A  <- R_A()      # 3x3 transition matrix
        B  <- R_B()      # 3x3 emission matrix (rows=states; cols=Hot/Mild/Cold)
        
        Tn <- input$hmm_T
        states <- c("S1","S2","S3")
        obs_lv  <- c("Hot","Mild","Cold")
        
        # simulate from HMM
        z <- x <- integer(Tn)
        z[1] <- draw_cat(pi); x[1] <- draw_cat(B[z[1],])
        for (t in 2:Tn) {
            z[t] <- draw_cat(A[z[t-1],])
            x[t] <- draw_cat(B[z[t],])
        }
        
        # fit with depmixS4
        df  <- data.frame(obs = factor(obs_lv[x], levels = obs_lv))
        mod <- depmix(obs ~ 1, data = df, nstates = 3, family = multinomial("identity"))
        fit <- fit(mod, verbose = FALSE)
        zh  <- posterior(fit)$state
        
        list(
            t  = seq_len(Tn),
            x  = factor(obs_lv[x],     levels = obs_lv),
            z  = factor(states[z],     levels = states),
            zh = factor(states[zh],    levels = states)
        )
    }, ignoreInit = TRUE)
    
    
    output$hmm_demo_timeline <- renderHighchart({
        dd <- demo_data(); req(dd)
        library(highcharter)
        
        # MODIFICATION: New color palettes for states and observations
        cols_state <- c(S1="#F6C54E", S2="#A9A9A9", S3="#4DA3FF")       # Sunny, Cloudy, Rainy
        cols_obs   <- c(Hot="#FF7F50", Mild="#9ACD32", Cold="#778899")  # Hot, Mild, Cold
        
        n <- length(dd$t)
        
        # MODIFICATION: Scale marker and cross size. Increased cross multiplier for size.
        dot   <- max(6, min(18, 150 / n))
        cross <- round(dot * 3.0)      # font px for ×, larger to extend past circle
        
        # y rows (use categories)
        cats <- c("Observation","True state","Decoded")
        yObs <- 0; yTrue <- 1; yDec <- 2
        
        # helper to build point objects
        mkpt <- function(x, y, fill)
            list(x = x, y = y,
                 marker = list(radius = dot, symbol = "circle",
                               fillColor = fill, lineColor = "#222", lineWidth = 1))
        
        obs_data  <- Map(function(t, cl) mkpt(t, yObs, cl),  dd$t, cols_obs[as.character(dd$x)])
        true_data <- Map(function(t, cl) mkpt(t, yTrue, cl), dd$t, cols_state[as.character(dd$z)])
        dec_data  <- Map(function(t, cl) mkpt(t, yDec, cl),  dd$t, cols_state[as.character(dd$zh)])
        
        # MODIFICATION: decode errors → red × adjusted for size and vertical alignment
        wrong <- which(as.character(dd$z) != as.character(dd$zh))
        err_data <- lapply(dd$t[wrong], function(t) {
            list(x = t, y = yDec,
                 dataLabels = list(enabled = TRUE, useHTML = TRUE, crop = FALSE, overflow = "none",
                                   align = "center", verticalAlign = "middle",
                                   y = round(cross * -0.05), # Nudge down to center large X
                                   style = list(color = "#cc0000", fontWeight = "900",
                                                fontFamily = "Arial, sans-serif",
                                                fontSize = paste0(cross, "px"),
                                                textOutline = "none", pointerEvents = "none"),
                                   format = "&times;"),
                 marker = list(enabled = FALSE))
        })
        
        # MODIFICATION: Hardcoded HTML legend
        legend_html <- paste0(
            '<div style="font-size: 13px; font-family: sans-serif;">',
            '  <style>',
            '    .legend-table td { padding: 2px 0; }',
            '    .legend-swatch { height: 12px; width: 12px; border-radius: 50%; display: inline-block; border: 1px solid #222; vertical-align: middle; }',
            '    .legend-label { padding-left: 6px; }',
            '    .legend-x { color:#cc0000; font-weight: 900; font-size: 22px; text-align: center; vertical-align: middle; line-height: 1; }',
            '  </style>',
            '  <table class="legend-table" style="border-spacing: 0 4px; border-collapse: separate;">',
            '    <tr><td colspan="2" style="font-weight: bold;">States</td><td colspan="2" style="padding-left: 20px; font-weight: bold;">Observations</td></tr>',
            '    <tr>',
            '      <td><span class="legend-swatch" style="background-color:', cols_state["S1"], ';"></span></td><td class="legend-label">Sunny</td>',
            '      <td style="padding-left: 20px;"><span class="legend-swatch" style="background-color:', cols_obs["Hot"], ';"></span></td><td class="legend-label">Hot</td>',
            '    </tr>',
            '    <tr>',
            '      <td><span class="legend-swatch" style="background-color:', cols_state["S2"], ';"></span></td><td class="legend-label">Cloudy</td>',
            '      <td style="padding-left: 20px;"><span class="legend-swatch" style="background-color:', cols_obs["Mild"], ';"></span></td><td class="legend-label">Mild</td>',
            '    </tr>',
            '    <tr>',
            '      <td><span class="legend-swatch" style="background-color:', cols_state["S3"], ';"></span></td><td class="legend-label">Rainy</td>',
            '      <td style="padding-left: 20px;"><span class="legend-swatch" style="background-color:', cols_obs["Cold"], ';"></span></td><td class="legend-label">Cold</td>',
            '    </tr>',
            '    <tr>',
            '      <td><div class="legend-x">&times;</div></td><td class="legend-label">Decode Error</td>',
            '      <td></td><td></td>',
            '    </tr>',
            '  </table>',
            '</div>'
        )
        
        # MODIFICATION: Chart construction with custom HTML legend
        hc <- highchart() %>%
            hc_chart(
                type = "scatter",
                spacingRight = 260, # Increased space for the new legend
                spacingLeft = 0,
                events = list(
                    # Render the custom HTML legend on chart load
                    load = JS(sprintf(
                        "function() { this.renderer.html('%s', this.chartWidth - 205, 60).add(); }",
                        gsub("'", "\\\\'", legend_html) # Escape quotes for JS
                    ))
                )
            ) %>%
            hc_title(text = "HMM demo (simulate \u2192 EM fit via depmixS4)") %>%
            hc_xAxis(title = list(text = "Time"), min = 1, max = n, tickInterval = 1) %>%
            hc_yAxis(categories = cats, tickPositions = list(yObs, yTrue, yDec),
                     min = -0.5, max = 2.5, gridLineWidth = 0, title = list(text = NULL)) %>%
            hc_plotOptions(scatter = list(stickyTracking = FALSE, enableMouseTracking = FALSE)) %>%
            hc_add_series(data = obs_data,  name = "Observation", showInLegend = FALSE) %>%
            hc_add_series(data = true_data, name = "True state",  showInLegend = FALSE) %>%
            hc_add_series(data = dec_data,  name = "Decoded",     showInLegend = FALSE)
        
        if (length(err_data)) {
            hc <- hc %>% hc_add_series(type = "scatter", data = err_data, name = "Decode error",
                                       showInLegend = FALSE, enableMouseTracking = FALSE)
        }
        
        # MODIFICATION: Removed old legend series and hc_legend call
        hc %>%
            hc_tooltip(enabled = FALSE) %>%
            hc_credits(enabled = FALSE)
    })
    
    
    
    
    
    
    #SERVER CODE FOR HMM ABOVE, ANALYSIS BELOW
    #-----------------------------------------------------------------------------
    
    #3D TSNE PLOT
    #library(depmixS4)
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
            group = factor(predicted_states)
        )
        
        hc <- highchart() %>%
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
                pointFormat = 'X: {point.x}<br>
                               Y: {point.y}<br>
                               Z: {point.z}<br>
                               Group: {point.group}'
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
    
    output$card_content <- renderUI({
        selected_node <- input$hmm_vis_selected
        
        #node selection text
        if (!is.null(selected_node) && selected_node %in% c("S1", "S2", "S3", "Initial")) {
            if (selected_node == "S1") {
                return(emission_prob)
            } else if (selected_node == "S2") {
                return(transition_prob)
            } else if (selected_node == "S3") {
                return(p("This is the text for state S3."))
            } else if (selected_node == "Initial") {
                return(initial_state_selected)
            }
        }
        
        #Default text
        p(no_state_selected)
    })
    
    
}

# ----- Run App -----
shinyApp(ui, server)