# Dashboard_bs4Dash.R
# Upgraded to bs4Dash

# Imported libraries ----
library(shiny)
library(bs4Dash)
library(highcharter)

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)

library(DT)
library(visNetwork)
library(fresh)
library(RColorBrewer)
library(shinyjs)
library(reactable)
library(depmixS4)
library(sparkline)

# Data Files Read in ----

scaled_data <- readRDS("scaled_dataset.rds")
full_dataset <- readRDS("full_dataset.rds")
plot_df <- readRDS("plot_top5_delta_ll.rds")
posterior_all_df <- readRDS("posterior_all_models.rds")
emiss_obs <- readRDS("emissions_observations.rds")
bundle <- readRDS("hmm_models_bundle.rds")
cons_sent_monthly <- readRDS("cons_sent_monthly.rds")
b_frozen_m4 <- readRDS("post_2007_model.rds")
tsne_data <- readRDS("tsne_M4.rds")

# All Text Used in Dashboard ----

## Introduction Text ----

consumer_sentiment_text <- HTML(
    
    "<p> 
    <p>  </p>
    <b> Consumer sentiment </b> plays a critical role in the growth of the economy, as the average consumer's opinion on the state of the 
    economy can directly manifest in changes in spending, saving, investment, and the flow of capital. Understanding this sentiment is 
    vital to understanding the economy, as consumption accounts for over two-thirds of GDP. When 
    sentiment improves, people are more likely to spend and invest, boosting economic activity. When it falls, they tend to save more 
    and cut back on purchases, slowing growth.</p>
    
     <p>However, this value is also is noisy, and serves as a response to economic conditions, so deeper potential relationships may not 
    be fully captured by traditional time series analysis. This analysis serves as an alternate approach to identify these shifts in 
    sentiment by applying a Hidden Markov Model (HMM) to categorize consumer sentiment into specific states, which can then be better 
    understood using macroeconomic analysis.</p> 
    
     <p>Unlike linear models that assume constant relationships between variables or variable independence, 
    HMMs excel in modeling non-stationary time series data. Through analyzing the data, the model can identify 
    unobserved conditions that dictate the probability of transitioning into another state. By training the HMM on 
    macroeconomic indicators, we can analyze which variables play the greatest role in shaping sentiment, 
    estimate the likelihood of transitions between sentiment states, and predict future shifts more reliably than alternative models.</p>
    
    "
)

cs_methodology <- withMathJax(HTML('
<p>  </p>
    
<p>Once a month, the University of Michigan surveys around 800 citizens on their outlook of current and future economic prospects. 
The questions investigate how consumers feel about their personal finances and the direction of the economy, including questions like:

</p>
   <ul> 
   <li>How are your current personal finances compared to a year ago?</li> 
   <li>Do you think business conditions in the country are good or bad at present?</li> 
   <li>Do you expect your personal finances to improve or worsen over the next year?</li> 
   <li>Do you expect business conditions to improve or worsen in the next year?</li> 
   <li>Over the next 5 years, do you think the economy will have good times or bad times?</li> 
   </ul> 
   <p>Responses are categorized as positive, neutral, or negative. For each question, the net value (percent positive minus percent negative)
  is calculated and used to compute the index:</p> 
  
    <p>
  $$
    \\text{Index}_{i} = \\frac{\\text{Current period score}}{\\text{Base period score}}
  $$
  </p>
  
   <p>The calculated value showes how consumers feel about the overall direction of the economy and their finances, relative to 
   how consumers felt in 1966. 
  <b> Values above 100 </b> indicate sentiment stronger than in the base period, while <b> values below 100 </b> indicate weaker sentiment. 
  The index is still interpretable when comparing the values between two separate periods. For example, if period 1 had a score of 85 and 
  period 2 had a score of 87, it can be concluded that consumer sentiment has improved relative to the earlier period.</p>
  

'))

cs_data_collection<- HTML(
    "
    <p> </p>
    
    <p>For this analysis, a set of 17 macroeconomic indicators were collected from the 
    <strong>Federal Reserve of St. Louis (FRED)</strong>, a widely respected source for accurate, up-to-date economic data.
    Data from 1987 to 2024 was collected for modeling and analysis, striking a good balance for the amount of time to learn from
    and the quality of available data, ensuring many economic sectors were well represented. The selection of indicators is critical as they 
    serve as the <strong>observed variables</strong> in the HMM, providing the data to train the model and identify the complex, 
    non-linear relationships.</p>
    
    
     <p>The indicators are categorized as the following:</p>
     <ul>
     <li><strong>Output</strong>: Measures of economic productivity which reflect the overall health of the economy.</li>
     
     <li> <strong>Labor Market</strong>: Indicators measuring employment, which directly impact people's financial 
     security and future outlook.
     
     </li>
     
     <li><strong>Price Levels</strong>: Inflation metrics which greatly affect purchasing power and consumer confidence.</li>
     
     <li><strong>Monetary and Fiscal</strong>: Interest rates and government spending, which 
     shape the financial environment and economic policy.</li>
     
     <li><strong>Housing and Construction</strong>: Indicators such as housing starts and building permits, which serve as leading 
     indicators of economic cycles and household confidence.</li>
     
     </ul>
     <p>Detailed information about each category and their relationship to consumer sentiment is in 
     <a href='javascript:void(0);' id='prelim_analysis'>Economic Indicators</a>.</p>
     "
)

## Preliminary Analysis Text ----

output_analysis <- HTML('
      <p><strong>Output indicators:</strong> variables that measure the U.S.\'s total economic production.</p>
      <ul>
      
        <li><strong>Nominal GDP:</strong> The raw dollar value of all goods and services produced. 
          An increase means more dollars are changing hands-often signaling higher incomes and business investment. 
          When Nominal GDP is rising, consumers generally feel richer and more willing to spend, while a slowdown can sap confidence.</li>
          
        <li><strong>Real GDP:</strong> Nominal GDP adjusted for inflation. 
          Growth here reflects true increases in volume of production. 
          Strong Real GDP growth suggests rising real wages and employment, which typically boost consumer sentiment. 
          Conversely, flat or negative Real GDP growth often coincides with weaker confidence and lower buying power.</li>
          
      </ul>
      <p>Because consumer spending accounts for roughly two‑thirds of GDP, shifts in these output measures quickly 
      feed back into how households view their personal finances and the broader economy.</p>
    ')

labor_analysis <- HTML('
      <p><strong>Labor market indicators:</strong> measures of how easily people find and keep jobs.</p>
      <ul>
      
        <li><strong>Unemployment rate:</strong> Percent of the labor force without a job. 
          Rising unemployment often reflects businesses cutting back. This tends to dent consumer sentiment as job security erodes. 
          Falling unemployment generally lifts confidence and spending.</li>
          
        <li><strong>Participation rate:</strong> Share of working‑age people in the labor force. 
          A rising participation rate can signal optimism that jobs are available, bolstering sentiment, while 
          a falling rate may reflect discouraged workers leaving the market, dampening overall confidence.</li>
          
        <li><strong>Unemployment claims:</strong> Weekly filings for jobless benefits. 
          Spikes in initial claims often presage layoffs and can spook consumers, while declines point to a stable 
          labor market.</li>
          
      </ul>
      <p>Together, these indicators demonstrate how tight or slack the jobs market is, a key driver of household incomes and therefore consumer attitudes 
      toward spending and saving.</p>
    ')

price_analysis <- HTML('
      <p><strong>Price levels/inflation indicators:</strong> gauges of how fast prices are rising.</p>
      <ul>
      
        <li><strong>CPI:</strong> Tracks the cost of a fixed basket of goods for the average urban consumer. 
          Rapid CPI increases erode purchasing power and often hurt consumer sentiment.
          Moderate, predictable inflation is less damaging. (Baseline: 1982-1984 = 100)</li>
          
        <li><strong>PCEPI:</strong> Measures prices for what people actually buy (a dynamic basket). 
          Because it better reflects changing consumption patterns, PCEPI can give early warning of 
          shifts in real household outlays and sentiment. (Baseline: 2017 = 100)</li>
          
        <li><strong>Robust PCEPI:</strong> Excludes volatile food and energy. 
          This focused measure helps isolate underlying inflation trends that more 
          directly influence long‑term consumer expectations. (Baseline: 2017 = 100)</li>
          
      </ul>
      <p>When consumers see news of accelerating prices-especially core inflation, they may feel their buying power shrinking, pulling down sentiment.  
      Conversely, stable or falling inflation tends to reassure people.</p>
    ')

monetary_analysis <- HTML("
      <p><strong>Monetary and fiscal indicators:</strong> measures of borrowing costs and government finances.</p>
      <ul>
      
        <li><strong>Federal funds rate:</strong> The Fed’s overnight bank‑to‑bank lending rate. 
          A higher rate makes all loans (home, auto, business) more expensive, reducing borrowing and spending. 
          Cutting the rate tends to spur activity and lift consumer confidence.</li>
          
        <li><strong>Three‑month Treasury yield:</strong> Short‑term government borrowing cost. 
          Rising yields can reflect higher inflation expectations and tighter financial conditions, which may weaken sentiment.</li>
          
        <li><strong>M2 money supply:</strong> The total of cash, checking deposits, and easily convertible near‑money instruments. 
          Rapid M2 growth can signal easy credit (boosting sentiment), while slow growth or contraction may reflect 
          monetary tightening and dampen sentiment.</li>
          
        <li><strong>Federal Spending:</strong> The sum of government expenditures in a given year. This can show growth in the economy
        though the volume of government contracts and gontracts, or demonstrate retractionary policy with spending cuts.</li>
        
        <li><strong>Federal Debt:</strong> The total amount of debt the federal government owes to other governments or institutions.
        This metric is used in tandem with federal spending to analyze how sustainable government spending practices are, and to contextualize
        spending habits.  </li>
          
      </ul>
      
      <p> These metrics largely affect interest rates and the cost of investment, giving insight to how confident people feel in putting
      money into the economy.</p>
    ")

housing_analysis <- HTML('
      <p><strong>Housing & construction indicators:</strong> gauges of residential building and sales activity.</p>
      <ul>
      
        <li><strong>New private houses:</strong> Volume of single‑family homes built. 
          A pickup in home construction suggests builders expect strong demand, often reinforcing higher consumer sentiment.</li>
          
        <li><strong>New permits:</strong> Approvals for future building. 
          Rising permits indicate confidence among developers and signal that the housing market, and thus consumer confidence, is on solid ground.</li>
          
        <li><strong>House sales:</strong> Number of homes sold. 
          Strong sales often coincide with easy credit and consumer optimism, while a slump may reflect tighter lending standards or weaker sentiment.</li>
          
        <li><strong>Case‑Shiller Index:</strong> A repeat‑sales index tracking home price changes. 
          Rising home prices build household wealth, boosting consumer sentiment, 
          while falling prices can have the opposite effect. (Baseline: 2000 = 100)</li>
          
      </ul>
      <p>Because housing is a large share of household wealth and spending, swings in these indicators can move consumer attitudes sharply, especially on big‑ticket purchases.</p>
    ')

## Hidden Markov Model Text ----

intro_hmm_text <- HTML(
    "
    <p> </p>
    <p>
    <b> A Hidden Markov Model (HMM) </b> is a statistical tool used to understand systems where you can't directly 
    observe the state of the system, but you can observe evidence or signals that are influenced by that state.
    </p>
    
    <p>
    Think of it like trying to guess the weather somewhere on another planet. You can't see the weather from where you are, 
    but if you are told the humidity is low, there is a high-pressure system, and wind speeds are low, 
    so you can make a good guess that it is likely calm weather at the moment.
    </p>
    
    <p>
    There is two layers to this: the unobservable hidden states (the weather type) and the observations we can see 
    (the meteorological indicators). The model assumes that the sequence of hidden states follows a specific type of random process 
    called a <b> Markov chain </b>, where the next state only depends on the current state.
    </p>
    
    <p>
    The primary goal of a Hidden Markov Model is to infer the most likely sequence of hidden states given a sequence of observations. 
    For example, if you observe a pattern of <b> {Arid, Arid, Humid} </b> over three days, the HMM can calculate 
    the most probable weather sequence, such as <b> {Sunny, Sunny, Rainy} </b>.
    </p>
    "
)

emission_prob <- HTML(
    "
    <p> </p>
    
  <p>
    <b> Emission Probabilities </b> link the hidden states to the observations. They represent the probability of 
    seeing a particular observation given that the system is in a specific hidden state. For example, given that it is Rainy, 
      what is the probability that the air is Arid? These are also learned from historical data. For all the times the 
      state was sunny, the model calculates the proportion of times the observations were arid and humid.
  </p>
  
  <p>
  In the matrix below, each entry represents the likelihood of observing a condition given a state. Each row shows the probability
  of seeing a condition given every state in the system. This gives a mathematically useful way of identifying trends in our observations,
  such as analyzing how useful an indicator is. 
  </p>
  
  <p>
  
  For example, if an indicator initially seemed to be useful, but its emissions matrix
  did not demonstrate any relationships (all values are about equal), then that indicator cannot give any predictive insight on the states.
  </p>
"
)

transition_prob  <- HTML(
    "
    <p> </p>
  <p>
    <b> Transition Probabilities </b> govern how the hidden states change over time. 
    They are the probabilities of moving from one hidden state to another in the next time step.
  </p>
  
  <p>
   These probabilities model the dynamics of the system. For instance, if the weather is sunny today, 
   what is the probability that it will be cloudy tomorrow? These are learned from historical data by counting how 
   often each state follows another. For example, the model would count how many times a sunny day was followed 
   by a rainy day in the dataset to calculate that specific probability.
  </p>
  
  <p>
  In the matrix below, each transition likelihood is stored as a value between 0 and 1. Each row represents the likelihood of moving from
  one state to every other state in the system. Using this matrix, we can identify trends in time. For example, the value from Sunny to
  Rainy may be low, and the value from Rainy to Sunny may be high, meaning that it is unlikely on any Sunny day that it will become
  Rainy, and once it does it is very likely to become Sunny the next day.
  </p>
  
"
)

hmm_training <- HTML(
    "
    <p> </p>
    
    <p>
      The model trains by analyzing a large set of historical data where both the states and observations are known. 
      At this stage, its central task is to determine the values of its internal probabilities (the initial state, transition likelihood, 
      and emission probabilities) that best explain the given data. To measure what 'best' means, a statistical score called the 
      <b>log-likelihood</b> is used, which numerically measures how well the model explains the data.
    </p>
    
    <p>
      To calculate the transition and emission probabilities, we need to know the sequence of hidden states. However, 
      that sequence is unknown. To solve this, the HMM uses a powerful iterative method called the <b>Expectation-Maximization (EM)</b> 
      algorithm. The algorithm starts with an initial guess for the probabilities and then repeatedly refines them through a two-step cycle, 
      guaranteeing that the log-likelihood improves with each iteration.
    </p>
    
    <p>
      This cycle of expectation and maximization repeats until the model <b>converges</b>, when the improvements in the 
      log-likelihood score become negligibly small. At this point, the algorithm has found a stable, optimized set of probabilities 
      that best describes the hidden dynamics within the data. 
      
      </p>
      
    </p>"
)

trans_matrix_text <- HTML(
    '
    <p>
    Use the transition matrix to see how the probability values changes the states in the simulation.
    Increasing the value of a cell means that transition is more likely to occur.
    </p>

    <p>
    <b> (Note: All probability rows must sum to 1) </b>
    </p>
    '
    
)

emission_matrix_text <- HTML(
    '
    <p>
    Use the emissions matrix to see how the probability values changes the observations in the simulation.
    Increasing the value of a cell means that observation is more likely to occur.
    </p>

    <p>
    <b> (Note: All probability rows must sum to 1) </b>
    </p>
    '
)

## Model Analysis Text ----

    model_selection_text <- withMathJax(HTML(
        "
        <p>
        To optimize the interpretability and numerical stability of the model, the macroeconomic indicators were first filtered. 
        This ensured that no single economic category was overrepresented, which would inadvertently bias the model. This process 
        involved an initial screening to find the single indicator from each category that exhibited the highest predictive power 
        with consumer sentiment. As part of this screening, each indicator was evaluated both contemporaneously \\(t\\) and with a 
        one-year delay (12 month lag) to reflect how information typically reaches households and markets. This helped avoid look-ahead 
        effects and capture genuine lead-lag dynamics.
        </p>

        <p>
        Following this preliminary filtering, a feature selection process was applied to identify the optimal combination of three 
        indicators. Each candidate model was then rigorously tested using out-of-sample validation via a rolling cross-validation scheme, 
        using training and testing windows to ensure the model's performance is not artificially inflated by period-specific anomalies. 
        The final specification therefore balances signal across categories and timing, with lags included only when they improved 
        real-time predictability.
        </p>

        <p><b>Model selection relied on two core metrics:</b></p>
        <ul>
          <li><b>Mean Log-Likelihood per Observation:</b> how well a candidate explains unseen test-set data, judged relative to a 
          baseline model with no time-varying drivers.</li>
          
          <li><b>Standard Deviation Across Folds:</b> the model’s stability and robustness across different training periods.</li>
        </ul>
        
        <p>
        Preference was given to models that deliver a higher average log-likelihood, indicating better predictive performance, with 
        lower variability across folds, highlighting their ability to generalize reliably across different economic cycles. 
        Where lags were selected, they consistently improved these out-of-sample scores.
        </p>
        "
    ))

model_result_info <- withMathJax(HTML(
    "<div style='font-size:18px; line-height:1.6; margin-top:30px; font-family: Helvetica;'>

    <p>
    The model utilizing <b>Lagged Real GDP (12 month lag)</b>, 
    the <b>PCE Price Index </b>, and the 
    <b>Lagged Federal Surplus/Deficit (12 month lag)</b> (<b>Model 1</b>) 
    was identified as the most effective combination.
    It consistently delivered the strongest out-of-sample improvement
    over the baseline, proving superior and more consistent at anticipating Consumer Sentiment regime shifts
    in unseen historical periods than alternative models. The strength of this combination lies in its ability to non-redundantly capture the three key economic
    levers influencing households:
    </p>

    <ul>
      <li><b>Real Productivity</b> (lagged Real GDP captures income/jobs momentum as consumers feel it)</li>
      <li><b>Cost of Living</b> (PCE Price Index reflects inflation/price pressure)</li>
      <li><b>Public Finance </b> (lagged Federal Surplus/Deficit proxies for policy stance and stress as people learn about it)</li>
    </ul>

    <p>
    t-SNE compresses the three selected indicators <b> (Real GDP, PCE Price Index, and Federal Surplus/Deficit </b>) into a 
    3-D map by preserving local neighborhoods. Years that look similar across those variables are placed near each other, and dissimilar 
    years are pushed apart. By analyzing the clustering and separation, we can get a better understanding of the relationship of the 
    variables. In our plot, the groups align with the model's High and Low states, indicating the chosen indicators naturally form two 
    regimes with limited overlap. Points between or on the fringes of clusters likely mark transition years and lower classification 
    confidence. This demonstrates separability, and that the regimes are truly two distinct periods.
    
    </p>

  </div>"
))

## Model Conclusion Text ----

regime_conclusion <- HTML(
    "<div style='font-size:18px; line-height:1.6; margin-left: 85px; margin-right: 85px; font-family: Helvetica; margin-top:20px'>
    
    <p>
    The Hidden Markov Model was able to successfully identify two distinct regimes in the Consumer Sentiment Index, a High state associated 
    with steady growth and tailwinds, and a Low state tied to stress periods. The model’s regimes line up with economic downturns and their 
    immediate aftermath, but it also catches transitions on the way in and out, not just the troughs. The use of a state to state analysis 
    strengthened the algorithm to time dependent explanatory data and response, allowing it to better analyze the relationships and trends
    without the typical issues of a strongly related dataset.
    </p>
    
    <p>
    The High regime clusters well above the long-run baseline while the Low regime sits significantly below it. Consistent with most Markov 
    models, states persist for long stretches, as self-transition probabilities dominate, and switches are asymmetric. Moves into Low tend 
    to be abrupt during stress events, while recoveries back to High are steadier as activity firms and price pressure cools paired 
    with cascading effect that lasts years with changes in interest rates, employment, and productivity. The inferred state often turns 
    before official recession dates and remains informative through the early expansion, which gives a peek at where the economy is heading.
    </p>
        </div>
    "
)

# Matrices for HMM tab

A0  <- matrix(c(0.75,0.25,
                0.20,0.80), 2, byrow = TRUE)
B0  <- matrix(c(0.70,0.30,
                0.40,0.60), 2, byrow = TRUE)


# Dashboard Construction ----

mint_dark_theme <- create_theme(
    theme = "superhero",
    bs4dash_vars(
        "navbar-light-bg"     = "#A5A9B4",
        "navbar-light-color"  = "rgb(235, 235, 235)",
        "sidebar-light-bg"    = "#0F3F1E",
        "sidebar-light-color" = "rgb(235, 235, 235)"
    ),
    bs4dash_yiq(
        contrasted_threshold = 150,
        text_dark = "#42FF66",
        text_light = "#ffffff"
    )
)

ui <- bs4DashPage(
    freshTheme = mint_dark_theme,
    controlbar = NULL,
    
    dark = NULL,
    help = NULL,
    
    # Header/navbar
    header = bs4DashNavbar(
        status = "success",
        border = TRUE
    ),
    
    ## Sidebar with vertical menu ----
    sidebar = bs4DashSidebar(
        id   = "sidebar",
        skin = "light",
        collapsed = TRUE,
        bs4SidebarUserPanel(
            image = "github logo.png",
            name = tags$a(
                href = "https://github.com/warisp897/ConsumerSentimentHMM",
                target = "_blank",
                "Waris Popal"
            )
        ),
        
        bs4SidebarMenu(
            id = "dashboard_tabs", # ID of Sidebar menu
            bs4SidebarHeader("Dashboard Navigation"),
            bs4SidebarMenuItem("Overview", 
                               tabName = "overview", 
                               icon = icon("dashboard")),
            
            bs4SidebarMenuItem("Economic Indicators", 
                               tabName = "indicator_analysis", 
                               icon = icon("money-bill-trend-up")),
            
            bs4SidebarMenuItem("Hidden Markov Model", 
                               tabName = "model_intro", 
                               icon = icon("project-diagram")),
            
            bs4SidebarMenuItem("Analysis", 
                               tabName = "model_analysis", 
                               icon = icon("chart-line")),
            
            bs4SidebarMenuItem("Conclusion", 
                               tabName = "model_conclusion", 
                               icon = icon("newspaper")),
            
            bs4SidebarMenuItem("Live Forecasting", 
                               tabName = "model_forecasting", 
                               icon = icon("square-poll-vertical"))
        )
            
    ),
    
    ## Custom CSS Adjustments ----
    body = bs4DashBody(
        useShinyjs(),
        tags$head(
            
            tags$link(
                rel  = "stylesheet",
                type = "text/css",
                href = "custom.css"
            ),
            
            tags$script(src = "custom.js")
            
        ),
        
        ## All Tab Content ----
        
        ### Introduction ----
        bs4TabItems(
            bs4TabItem(
                tabName = "overview",
                fluidRow(
                    # Consumer Sentiment Over Time
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        title = HTML("<b>Analysis of Consumer Sentiment Using Hidden Markov Model Regimes</b>"),
                        width = 12,
                        status = "success",
                        fluidRow(
                            column(width = 5,
                                   div(style = "height: 80vh; font-size:18px; line-height:1.5; overflow-y:auto; font-family: Helvetica;",
                                       
                                       bs4Dash::tabsetPanel(
                                           id = "consumer_sent_tabs",
                                           type = "pills",
                                           tabPanel("Introduction", consumer_sentiment_text),
                                           tabPanel("Methodology", cs_methodology),
                                           tabPanel("Data Collection", cs_data_collection)
                                       )
                                   )
                                ),
                            column(width = 7, 
                                   highchartOutput("consumer_sentiment_plot", height = "80vh"))
                        )
                    )
                )
            ),
        
        
            
            ### Preliminary Analysis Tab ----
            bs4TabItem(
                tabName = "indicator_analysis",
                fluidRow(
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        width = 5,
                        status = "success",
                        title = HTML('<b> Economic Indicators </b>'),

                            div(
                                style = "min-height: 80vh; font-size:18px; line-height:1.5; display:flex; flex-direction:column; font-family: Helvetica;",
                                
                                # selector stays on top
                                selectInput("select1", "Indicator Category", choices = c(
                                    "Output", "Labor Market", "Price Levels",
                                    "Monetary and Fiscal", "Housing and Construction"
                                )),
                                
                                # TEXT SHELL (default)
                                div(
                                    id = "cat_text_shell",
                                    style = "flex:1 1 auto; display:flex; flex-direction:column;",
                                    div(style = "flex:1 1 auto;", uiOutput("category_summary")),
                                    div(style = "margin-top:.5rem; display:flex; justify-content:flex-end;",
                                        actionButton("show_summary_table", "More details", icon = icon("table"), class = "btn btn-outline-success"))
                                ),
                                
                                # TABLE SHELL (hidden until button click)
                                shinyjs::hidden(
                                    div(
                                        id = "cat_table_shell",
                                        style = "flex:1 1 auto; display:flex; flex-direction:column; min-height:0;",
                                        div(
                                            id    = "dt_wrap",
                                            style = "flex:1 1 auto; min-height:0; overflow-x:auto; width:100%;",
                                            DT::dataTableOutput("summary_table", width = "100%")
                                        ),
                                        div(style = "margin-top:.5rem; display:flex; justify-content:flex-end;",
                                            actionButton("back_to_summary", "Back", icon = icon("arrow-left")))
                                    )                                    )
                            )
                        ),
                    
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        width = 7,
                        status = "success",
                        highchartOutput("category_plot_hc", width = "100%", height = "45vh"),
                        highchartOutput("pearsons_plot_hc", width = "100%", height = "45vh")
                        )
                )
            ),
            
            ### Hidden Markov Model Tab ----
            bs4TabItem(
                tabName = "model_intro",
                
                # Top explanatory card
                bs4Card(
                    collapsible = FALSE,
                    closable = FALSE,
                    title = HTML('<b> The Hidden Markov Model </b>'),
                    width = 12,
                    status = "success",
                    fluidRow(
                        column(
                            width = 5,
                            div(style = "font-size:18px;font-family: Helvetica;",
                                bs4Dash::tabsetPanel(
                                    id = "hmm_info_tabs",
                                    type = "pills",
                                    selected = "hmm_intro",
                                    tabPanel("Introduction", value = "hmm_intro",
                                             div(style = "min-height: 50vh; font-size:18px; font-family:Helvetica;",
                                                 intro_hmm_text
                                             )
                                    ),
                                    tabPanel("Training", value = "hmm_train_exp", 
                                             div(style = "min-height: 63.2vh; font-size:18px; font-family:Helvetica;",
                                                 hmm_training
                                             )
                                    ),
                                    tabPanel("Transition Probability", value = "trans_prob",
                                             div(style = "min-height:50vh; font-size:18px; font-family:Helvetica;",
                                                 transition_prob,
                                                 uiOutput("matrix_ui")
                                             )
                                    ),
                                    tabPanel("Emission Probability", value = "emission_prob", 
                                             div(style = "min-height:50vh; font-size:18px; font-family:Helvetica;",
                                                 emission_prob,
                                                 uiOutput("emissions_mat")
                                             )
                                    )
                                )
                            )
                        ),
                        
                        column(
                            width = 7,
                            div(
                                id = "sim_shell",
                                
                                conditionalPanel(
                                    condition = "input.hmm_info_tabs == 'hmm_intro'",
                                    uiOutput("hmm_overlay_pi")                 
                                ),
                                
                                conditionalPanel(
                                    condition = "input.hmm_info_tabs == 'hmm_train_exp'",
                                    fluidRow(
                                        column(
                                            width = 6,
                                            div(class = "p-2",
                                                visNetworkOutput("em_flow", height = "70vh", width = "100%"),
                                            )
                                        ),
                                        column(
                                            width = 6,
                                            div(
                                                class = "d-flex align-items-center",
                                                style = "min-height:70vh;",
                                                div(style = "width:100%;", uiOutput("em_step_details"))
                                            )
                                        )                                    )
                                ),
                                
                                conditionalPanel(
                                    condition = "input.hmm_info_tabs != 'hmm_train_exp'",
                                    fluidRow(
                                        # LEFT column
                                        column(
                                            width = 12,
                                            
                                            conditionalPanel(
                                                condition = "input.hmm_info_tabs == 'trans_prob'",
                                                div(class = "mx-row",
                                                    div(class = "mx-lead", trans_matrix_text),
                                                    div(class = "mx-mat",
                                                        numericInput("A11", NULL, value = round(A0[1,1], 2), min = 0, max = 1, step = 0.01, width = "90px"),
                                                        numericInput("A12", NULL, value = round(A0[1,2], 2), min = 0, max = 1, step = 0.01, width = "90px"),
                                                        numericInput("A21", NULL, value = round(A0[2,1], 2), min = 0, max = 1, step = 0.01, width = "90px"),
                                                        numericInput("A22", NULL, value = round(A0[2,2], 2), min = 0, max = 1, step = 0.01, width = "90px")
                                                    )
                                                )
                                            ),
                                            
                                            conditionalPanel(
                                                condition = "input.hmm_info_tabs == 'emission_prob'",
                                                div(class = "mx-row",
                                                    div(class = "mx-lead", emission_matrix_text),
                                                    div(class = "mx-mat",
                                                        numericInput("B11", NULL, value = round(B0[1,1], 2), min = 0, max = 1, step = 0.01, width = "90px"),
                                                        numericInput("B12", NULL, value = round(B0[1,2], 2), min = 0, max = 1, step = 0.01, width = "90px"),
                                                        numericInput("B21", NULL, value = round(B0[2,1], 2), min = 0, max = 1, step = 0.01, width = "90px"),
                                                        numericInput("B22", NULL, value = round(B0[2,2], 2), min = 0, max = 1, step = 0.01, width = "90px")
                                                    )
                                                )
                                            ),
                                            
                                            conditionalPanel(
                                                condition = "input.hmm_info_tabs == 'trans_prob' || input.hmm_info_tabs == 'emission_prob'",
                                                div(
                                                    class = "mt-2",
                                                    sliderInput("hmm_T", "Sequence length", min = 15, max = 45, value = 20, step = 1),
                                                    actionButton("hmm_run_demo", "Simulate Hidden Markov Model", class = "btn btn-success btn-block"),
                                                    
                                                    div(id = "hmm_chart_shell",
                                                        style = "height:40vh; min-height:420px;",
                                                        highchartOutput("hmm_demo_timeline", height = "100%")
                                                    ),
                                                    
                                                    uiOutput("more_btn")
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            
                            shinyjs::hidden(
                                div(
                                    id = "details_shell",
                                    reactable::reactableOutput("hmm_detail_table", height = "69vh"),
                                    div(style = "margin-top:.5rem; display:flex; justify-content:flex-end;",
                                        actionButton("back_to_chart", "Back", icon = icon("arrow-left"))
                                    )
                                )
                            )
                        )
                    )
                )
            ),

            
            ### Model Analysis Tab ----
            bs4TabItem(
                tabName = "model_analysis",
                fluidRow(
                    bs4Card(
                        collapsible = FALSE,
                        closable = FALSE,
                        title = HTML("<b> Consumer Sentiment Model </b>"),
                        width = 12,
                        status = "success",
                        
                        div(style = "font-size:18px;font-family: Helvetica;",
                            bs4Dash::tabsetPanel(
                                id   = "model_analysis_tabs",
                                type = "pills",
                                
                                tabPanel(
                                    "Model Selection",
                                    fluidRow(
                                        column(
                                            width = 6,
                                            div(
                                                style = "padding:10px; font-size:18px; line-height:1.5; overflow-y:auto; font-family: Helvetica;",
                                                model_selection_text
                                            )
                                        ),
                                        column(
                                            width = 6,
                                            div(
                                                style = "display:flex; flex-direction:column; height: calc(100vh - 220px);",
                                                highchartOutput("overview_ts", height = "100%", width = "100%")
                                            )
                                        )
                                    )
                                ),
    
                                tabPanel(
                                    "Model Metrics",
                                    fluidRow(
                                        
                                        column(
                                            width = 12,
                                            div(
                                                class = "segmented-control",
                                                actionButton("mod_one", "Model 1", class = "btn active"),
                                                actionButton("mod_two", "Model 2", class = "btn"),
                                                actionButton("mod_three", "Model 3", class = "btn"),
                                                actionButton("mod_four", "Model 4", class = "btn"),
                                                actionButton("mod_five", "Model 5", class = "btn")
                                            ),
                                            div(
                                                class = "segmented-control",
                                                actionButton("trans_btn", "Transition", class = "btn active"),
                                                actionButton("emiss_btn", "Emission", class = "btn")
                                            )
                                        )
                                    ),
                                        
                                    fluidRow(
                                        column(
                                            width = 6,
                                            highchartOutput("model_selection_plot", height = "850px"),
                                        ),
                                        column(
                                            width = 6,
                                            div(
                                                id = "transition_probs",
                                                highchartOutput("cs_regime_chart", height = "300px"),
                                                highchartOutput("tpm_time", height = "300px"),
                                                highchartOutput("tpm_avg", height = "300px")
                                            ),
    
                                            div(
                                                id = "emission_probs",
                                                
                                                # this sets the highchart output to not display initially 
                                                # (and display once the button is pressed)
                                                style = "display: none;", 
                                                highchartOutput("emis_dens", height = "340px"),
                                                highchartOutput("state_means_scaled", height = "340px"),
                                                highchartOutput("state_prob_heatmap", height = "340px")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            
            ### Model Conclusion ----
            bs4TabItem(
                tabName = "model_conclusion",
                bs4Card(
                    title = HTML('<span style="margin-left: 0%; font-size:36px
                                     "> <b> Conclusion </b> </span>'),
                    width = 12,
                    fluidRow(
                        column(
                            width = 12,
                            div(style = "font-size:18px; font-family: Helvetica;",
                                    tabsetPanel(
                                        id   = "conclusion_tabs",
                                        type = "pills",
                                        tabPanel(
                                            "Sentiment Regimes",
                                            fluidRow(
                                                column(
                                                    width = 12,
                                                    highchartOutput("consumer_sent_monthly")
                                                )
                                            ),
                                            regime_conclusion
                                        ),
                                        tabPanel(
                                            "Best Model",
                                            fluidRow(
                                                column(
                                                    width = 6,
                                                    model_result_info
                                                ),
                                                column(
                                                    width = 6,
                                                    
                                                    highchartOutput("state_means_real_M4"),
                                                    highchartOutput("state_plot")
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
            
            ### Model Forecasting ----
            bs4TabItem(
                tabName = "model_forecasting",
                fluidRow(
                    bs4Card(
                        collapsible = FALSE,
                        closable = FALSE,
                        title = HTML("<b>Live Forecasting Page</b>"),
                        width = 12,
                        status = "success",
                        fluidRow(
                            column(
                                width = 12,
                                
                                div(style = "margin-bottom: 20px;", uiOutput("status_box")),
                                div(style = "margin-bottom: 20px;", highchartOutput("regime_forecast_monthly")),
                                div(style = "margin-bottom: 10px;", uiOutput("summary_metrics")),
                                
                                # How to read this panel
                                div(
                                    style = "margin-bottom: 10px; display: flex; justify-content: flex-end;",
                                    actionButton("toggle_help", label = "How to read this table", icon = icon("question-circle"), 
                                                 class = "btn-xs btn-outline-secondary", 
                                                 style = "border: none; background: transparent; color: #6c757d; font-weight: 500;")
                                ),
                                
                                
                                conditionalPanel(
                                    condition = "input.toggle_help % 2 == 1",
                                    div(
                                        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #dee2e6; font-size: 0.9em;",
                                        h6(strong("Table Definitions"), style="margin-top:0; color: #495057;"),
                                        tags$ul(style = "padding-left: 20px; color: #6c757d;",
                                                tags$li(strong("Current (Z):"), " Where the indicator is right now (in standard deviations)."),
                                                tags$li(strong("Baseline (Z):"), " Where the indicator usually is during this economic regime."),
                                                tags$li(strong("Difference:"), " The distance between Current and Baseline. If this is greater than 1.0, it is flagged as Inconsistent with the current regime."),
                                                tags$li(strong("Status:"), " 'Inconsistent' means the indicator is behaving abnormally for this specific regime.")
                                        )
                                    )
                                ),
                                
                                div(
                                    style = "height: 300px; overflow-y: auto; border: 1px solid #ddd;",
                                    sparklineOutput("spark_placeholder", height = 0), 
                                    DT::dataTableOutput("table_drivers")
                                )
                            )
                        )
                    )                    )
                )
            )
        )
    )


# Server function ----
options(highcharter.offline = FALSE)
server <- function(input, output, session) {
    
    ## Overview Code ----
    
    # link hyperlink blue prelim analysis text to tab
    onclick("prelim_analysis", {
        updateTabsetPanel(session, "dashboard_tabs", selected = "indicator_analysis")
    })
    
    
    recessions <- data.frame(
        start = c("2000-03-01", "2007-12-01", "2020-03-01"),
        end   = c("2001-11-30", "2009-06-30", "2023-05-31"),
        name  = c("Dot-com Bubble", "Global Financial Crisis", "COVID-19 Pandemic"),
        color = c("rgba(255,0,0,0.3)", "rgba(255,0,0,0.3)", "rgba(255, 174, 66,0.3)"),
        stringsAsFactors = FALSE
    )
    
    plot_bands <- lapply(seq_len(nrow(recessions)), function(i) {
        list(
            from   = datetime_to_timestamp(as.Date(recessions$start[i])),
            to     = datetime_to_timestamp(as.Date(recessions$end[i])),
            color  = recessions$color[i],
            zIndex = 0,  # keep behind the lines
            label  = list(
                text      = recessions$name[i],
                align     = "center",    # center within the band
                rotation  = 0,
                y         = 12,          # tweak up/down as you like (positive = down)
                style     = list(
                    color = "#e74c3c",     # red label text
                    fontWeight = "bold",
                    fontSize = "11px",
                    textOutline = "none"
                )
                # useHTML = TRUE   # uncomment if you want HTML in the label
            )
        )
    })
    
    ### Consumer Sentiment TS Plot ----
    output$consumer_sentiment_plot <- renderHighchart({
        cs_data <- data.frame(
            date = as.Date(cons_sent_monthly$observation_date), 
            sentiment = cons_sent_monthly$UMCSENT
        ) %>%
            arrange(date)
        
        line_series <- cs_data %>%
            transmute(x = datetime_to_timestamp(date),
                      y = sentiment) %>%
            list_parse2()
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(zoomType = "x") %>%
            hc_title(text = "<b> Consumer Sentiment Over Time </b>",
                     style = list(fontSize = "28px")) %>%
            hc_xAxis(type = "datetime", 
                     plotBands = plot_bands,
                     gridLineWidth = 0) %>%
            hc_yAxis(
                title = list(text = "Index Value"),
                plotLines = list(list(
                    value = 100, color = "grey", dashStyle = "Dash", width = 3, zIndex = 5,
                    label = list(text = "<b> Index Baseline: 1966 = 100 </b>", align = "left",
                                 style = list(color = "red", fontSize = "16px"))
                ))
            ) %>%
            hc_add_series(
                data = line_series, type = "line", color = "black",
                lineWidth = 2, marker = list(enabled = FALSE), showInLegend = FALSE
            ) %>%
            hc_tooltip(
                useHTML = TRUE,
                formatter = JS(sprintf("
                                    function () {
                                      var x = this.x, y = this.y;
                                      var msg = '';
                                      if (x >= %s && x <= %s) {
                                        msg = '<br><span style=\"color:#e74c3c\"><b>Dot-com Bubble</b></span>';
                                      } else if (x >= %s && x <= %s) {
                                        msg = '<br><span style=\"color:#e74c3c\"><b>Global Financial Crisis</b></span>';
                                      } else if (x >= %s && x <= %s) {
                                        msg = '<br><span style=\"color:#e74c3c\"><b>COVID-19 Pandemic</b></span>';
                                      }
                                      return Highcharts.dateFormat('%%B %%e, %%Y', x) +
                                             '<br>Index: <b>' + Highcharts.numberFormat(y, 2) + '</b>' +
                                             msg;
                                    }
      ",
                                       datetime_to_timestamp(as.Date("2000-03-01")),
                                       datetime_to_timestamp(as.Date("2001-11-30")),
                                       datetime_to_timestamp(as.Date("2007-12-01")),
                                       datetime_to_timestamp(as.Date("2009-06-30")),
                                       datetime_to_timestamp(as.Date("2020-03-01")),
                                       datetime_to_timestamp(as.Date("2023-05-31"))
                ))
            ) %>%
            hc_credits(enabled = FALSE)
    })
    
    ## Preliminary Analysis Code ----
    
    scaled <- scaled_data %>%
        pivot_longer(-Year, names_to = "Indicator", values_to = "Scaled")
    
    full   <- full_dataset %>%
        pivot_longer(-Year, names_to = "Indicator", values_to = "True")
    
    
    ### Preliminary Analysis Switching Functionality ----
    output$category_summary <- renderUI({
        summary_text <- switch(input$select1,
                               
                               # Text displayed based on selected predictor type in combo box
                               
                               "Output" = output_analysis,
                               "Labor Market" = labor_analysis,
                               "Price Levels" = price_analysis,
                               "Monetary and Fiscal" = monetary_analysis,
                               "Housing and Construction" = housing_analysis#,
                               #"Trade" = trade_analysis
        )
        
        div(
            style = "font-size:18px; font-family:Helvetica; line-height:1.6;",
            summary_text
        )
    })
    
    format_value <- function(val, fmt) {
        if (is.na(val)) return(NA)
        switch(fmt,
               "$" = {
                   s <- formatC(abs(val), format = "f", digits = 2, big.mark = ",")
                   paste0(ifelse(val < 0, "-", ""), "$", s)
               },
               "%" = paste0(formatC(val, format = "f", digits = 2), "%"),
               round(val, 3)
        )
    }
    
    indicator_formats <- c(
        nominal_gdp = "$",
        real_gdp = "$",
        federal_debt = "$",
        fyfsd = "$",
        export = "$",
        import = "$",
        account_balance = "$",
        unemployment_rate = "%",
        participation_rate = "%",
        claims = "",
        cpi = "%",
        pcepi = "%",
        pcepi_robust = "%",
        federal_funds_rate = "%",
        three_month_rate = "%",
        m2 = "$",
        new_houses = "",
        new_permits = "",
        house_sales = "",
        case_schiller_val = "",
        consumer_sentiment = ""
    )
    
    # map from raw names to pretty labels:
    nice_names <- c(
        consumer_sentiment = "Consumer Sentiment",
        nominal_gdp        = "Nominal GDP",
        real_gdp           = "Real GDP",
        unemployment_rate  = "Unemployment Rate",
        participation_rate = "Labor Participation Rate",
        claims             = "Unemployment Claims",
        cpi                = "CPI",
        pcepi              = "PCEPI",
        pcepi_robust       = "PCEPI (Robust)",
        federal_funds_rate = "Federal Funds Rate",
        three_month_rate   = "3‑Month Rate",
        m2                 = "M2 Money Supply",
        fyfsd              = "Federal Spending",
        federal_debt       = "Federal Debt",
        new_houses         = "New Private Houses",
        new_permits        = "New Permits",
        house_sales        = "House Sales",
        case_schiller_val  = "Case‑Shiller Home Prices"#,
        #export             = "Exports",
        #import             = "Imports",
        #account_balance    = "Current Account Balance"
    )
    
    #- which raw indicators belong to which category:
    category_map <- list(
        "Output"                 = c("consumer_sentiment", "nominal_gdp", "real_gdp"),
        "Labor Market"           = c("consumer_sentiment", "unemployment_rate", "participation_rate", "claims"),
        "Price Levels"           = c("consumer_sentiment", "cpi", "pcepi", "pcepi_robust"),
        "Monetary and Fiscal"    = c("consumer_sentiment", "federal_funds_rate",
                                     "three_month_rate", "m2", "fyfsd", "federal_debt"),
        "Housing and Construction" = c("consumer_sentiment", "new_houses",
                                       "new_permits", "house_sales", "case_schiller_val")#,
        #"Trade"                  = c("consumer_sentiment", "export", "import", "account_balance")
    )
    
    palette_map <- list(
        "Output"                   = "Blues",
        "Labor Market"             = "Greens",
        "Price Levels"             = "Purples",
        "Monetary and Fiscal"      = "Reds",
        "Housing and Construction" = "BuGn",
        "Trade"                    = "PuBu"
    )
    
    # Distinct color palette per category
    pal_out    <- brewer.pal(length(category_map$`Output`), "Blues")
    pal_lab    <- brewer.pal(length(category_map$`Labor Market`), "Greens")
    pal_price  <- brewer.pal(length(category_map$`Price Levels`), "Purples")
    pal_mon    <- brewer.pal(length(category_map$`Monetary and Fiscal`), "Reds")
    pal_house  <- brewer.pal(length(category_map$`Housing and Construction`), "BuGn")
    #pal_trade  <- brewer.pal(length(category_map$`Trade`), "PuRd")
    
    indicator_colors <- c(
        setNames(pal_out,     category_map$`Output`),
        setNames(pal_lab,     category_map$`Labor Market`),
        setNames(pal_price,   category_map$`Price Levels`),
        setNames(pal_mon,     category_map$`Monetary and Fiscal`),
        setNames(pal_house,   category_map$`Housing and Construction`)#,
        #setNames(pal_trade,   category_map$`Trade`)
    )
    
    #default_cat <- isolate(input$select1)
    initial_cat  <- isolate(input$select1)
    initial_inds <- category_map[[ initial_cat ]]
    
    norm <- function(x) tolower(trimws(gsub("[^A-Za-z0-9_]", "_", x)))
    
    # make sure lookup names are normalized
    names(indicator_formats) <- norm(names(indicator_formats))
    names(nice_names)        <- norm(names(nice_names))
    
    # make sure the Indicators in your long frames are normalized
    scaled <- scaled %>% mutate(Indicator = norm(Indicator))
    full   <- full   %>% mutate(Indicator = norm(Indicator))
    
    CAT_PLOT_LOADED <- F
    #### Selected Indicators over Time Plot ----
    output$category_plot_hc <- renderHighchart({
        CAT_PLOT_LOADED <<- T
        
        fmt_lookup <- data.frame(
            Indicator = names(indicator_formats),
            fmt_string = as.character(indicator_formats),
            stringsAsFactors = FALSE
        )
        
        plot_df <- scaled %>%
            mutate(Indicator = as.character(Indicator)) %>%
            left_join(full, by = c("Year", "Indicator")) %>%
            # 2. Join the formatting instructions safely
            left_join(fmt_lookup, by = "Indicator") %>% 
            mutate(
                Label = nice_names[Indicator],
                # 3. Use the column from the join, replacing NAs with empty strings
                fmt = ifelse(is.na(fmt_string), "", fmt_string) 
            )
        
        print(plot_df)
        
        
        all_inds     <- unique(plot_df$Indicator)
        ordered_inds <- c(setdiff(all_inds, "consumer_sentiment"), "consumer_sentiment")

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
        
        dec_year <- function(d) {
            y <- year(d)
            start <- as.Date(paste0(y, "-01-01"))
            y + as.numeric(d - start) / (365 + leap_year(d))
        }
        
        # build plot bands in decimal-year space (no labels)
        plot_bands_year <- lapply(seq_len(nrow(recessions)), function(i) {
            s <- as.Date(recessions$start[i]); e <- as.Date(recessions$end[i])
            list(
                from  = dec_year(s),
                to    = dec_year(e),
                color = recessions$color[i],
                zIndex = 0
            )
        })
        
        bands_js <- {
            items <- vapply(seq_len(nrow(recessions)), function(i) {
                s <- dec_year(as.Date(recessions$start[i]))
                e <- dec_year(as.Date(recessions$end[i]))
                nm <- gsub("(['\\\\])", "\\\\\\1", recessions$name[i])
                sprintf("{from:%s,to:%s,name:'%s'}", format(s, scientific = FALSE), format(e, scientific = FALSE), nm)
            }, character(1))
            paste0("[", paste(items, collapse = ","), "]")
        }
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(zoomType = "x") %>%
            hc_chart(type = "line",
                     spacing = list(top = 10, right = 10, bottom = 20, left = 10)) %>%
            hc_plotOptions(series = list(
                events = list(
                    legendItemClick = JS(
                                        "function() {
                           var bc = Highcharts.charts.find(c => c.renderTo.id==='pearsons_plot_hc');
                           if (!bc) return true;
                           var bs = bc.get(this.options.id);
                           if (bs) {
                             var newColor = this.visible ? bs.options.inactiveColor : bs.options.origColor;
                             bs.update({ color: newColor }, true);
                           }
                           return true;
                         }"
                    )
                )
            )) %>%
            hc_add_series_list(series_list) %>%
            hc_legend(
                layout = "horizontal",
                align = "center",
                verticalAlign = "bottom",
                alignColumns  = FALSE,
                itemWidth = 100,
                itemStyle = list(fontSize = "0.8em")
            ) %>%
            hc_tooltip(
                useHTML = TRUE,
                shared = TRUE,
                formatter = JS(sprintf("
                                    (function(){
                                      var bands = %s;
                                      return function () {
                                        
                                
                                        var yr = this.x; 
                                        
                                        var s = '<b>Year: ' + yr + '</b><br/>'; 
                                
                                        this.points.forEach(function(p) {
                                        
                                          var fmt = p.point.format || '';
                                          var v   = p.point.trueValue;
                                          var txt;
                                          
                                          if (fmt === '$') {
                                            var sign = (v < 0 ? '-' : '');
                                            var absv = Highcharts.numberFormat(Math.abs(v), 2);
                                            txt = sign + '$' + absv;
                                          } else if (fmt === '%%') {
                                            txt = Highcharts.numberFormat(v, 2) + '%%';
                                          } else {
                                            txt = Highcharts.numberFormat(v, 2);
                                          }
                                
                                          s += '<span style=\"color:' + p.series.color + '\">' + p.series.name + '</span>: ' +
                                               '<b>' + txt + '</b><br/>';
                                        });
                                
                                        var tag = '';
                                        for (var i=0;i<bands.length;i++){
                                          if (yr >= bands[i].from && yr <= bands[i].to) {
                                            tag = '<br><span style=\"color:#e74c3c\"><b>'+bands[i].name+'</b></span>';
                                            break;
                                          }
                                        }
                                        
                                        s += tag; // Add the tag to the bottom
                                        
                                        return s; // Return the final, combined string
                                      }
                                    })()", bands_js))) %>%
            hc_yAxis(labels = list(
                enabled = FALSE), 
                title = list(text = ""),
                crosshair = list(
                    color = "darkgrey",
                    width = 1,
                    dashStyle = "Solid"
                )) %>%
            hc_xAxis(
                title = list(text = "Year"),
                gridLineWidth = 0,
                crosshair = list(
                    color = "darkgrey",
                    width = 1,
                    dashStyle = "Solid"
                ),
                plotBands = plot_bands_year    # precise bands (decimal-year)
            ) %>%
            hc_subtitle(text = "All Indicators are Scaled for Visual Cohesiveness <br> (Z-Score: Mean of 0, SD of 1)") %>%
            hc_title(text = paste(default_cat, "Over Time"),
                     style = list(fontWeight = "bold", fontSize = "16px"))
    })
    
    #### Econ Plot Updates ----
    observeEvent(input$select1, {
        req(isTRUE(CAT_PLOT_LOADED))
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
        req(isTRUE(CAT_PLOT_LOADED))
        highchartProxy("category_plot_hc", session) %>%
            # this will update only the chart title in place
            hcpxy_update(
                title = list(text = paste(input$select1, "Over Time"))
            )
    })
    
    
    #### Summary Table ----
    
    summary_table_data <- reactive({
        selected_category <- input$select1
        
        # select the indicators for the chosen category
        indicators <- category_map[[selected_category]]
        
        # drop CS
        indicators <- indicators[indicators != "consumer_sentiment"] 
        
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
        
        # sanitize names before displaying
        df_wide$Statistic <- gsub("\\s*\\(Year\\)", ". Year", df_wide$Statistic)
        df_wide$Statistic <- gsub("\\.+", ".", df_wide$Statistic)
        df_wide$Statistic <- sub("\\.$","", df_wide$Statistic)
        
        as.data.frame(df_wide)
    })
    
    # only make summary table visible if the user selects to it
    table_visible <- reactiveVal(FALSE)

    observeEvent(input$show_summary_table, { table_visible(TRUE) })
    observeEvent(input$back_to_summary,   { table_visible(FALSE) })
    
    output$summary_table <- DT::renderDataTable({
        req(table_visible())
        df <- summary_table_data()
        DT::datatable(
            df, 
            rownames = FALSE, 
            class = "compact stripe hover row-border",
            extensions = "FixedColumns",
            options = list(
                dom = 't', 
                paging = FALSE, 
                searching = FALSE, 
                ordering = FALSE,
                autoWidth = TRUE,
                scrollX = TRUE, 
                fixedColumns = list(leftColumns = 1),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            )
        ) %>%
            DT::formatStyle(colnames(df)[-1], `min-width` = '120px', `padding` = '6px 12px') %>%
            DT::formatStyle(colnames(df)[1],  `min-width` = '180px', fontWeight = 'bold')
    })
    
    indicator_to_category <- purrr::map_dfr(names(category_map), function(cat) {
        tibble::tibble(
            Indicator = category_map[[cat]],
            Category  = cat
        )
    })
    
    #allows table to display after clicking button
    observeEvent(input$select1, {
        shinyjs::show("cat_text_shell")
        shinyjs::hide("cat_table_shell")
    }, ignoreInit = TRUE)
    
    # show table
    observeEvent(input$show_summary_table, {
        shinyjs::hide("cat_text_shell")
        shinyjs::show("cat_table_shell")
    })
    
    # back to text
    observeEvent(input$back_to_summary, {
        shinyjs::show("cat_text_shell")
        shinyjs::hide("cat_table_shell")
    })
    
    #creates data frame with r values for bar chart
    cor_df_reactive <- reactive({
        scaled_data %>%
            dplyr::select(-consumer_sentiment, -Year) %>%
            purrr::imap_dfr(~tibble(
                Indicator = .y,
                r = cor(.x, scaled_data$consumer_sentiment, use = "complete.obs")
            )) %>%
            dplyr::mutate(
                Label = unname(nice_names[Indicator])
            ) %>%
            # You already have this mapping
            dplyr::left_join(indicator_to_category, by = "Indicator") %>%
            # Normalize category labels to match legend keys
            dplyr::mutate(
                Category_norm = dplyr::recode(
                    Category,
                    "Monetary and Fiscal"      = "Monetary & Fiscal",
                    "Housing and Construction" = "Housing & Construction",
                    .default = Category
                ),
                color = unname(category_colors[Category_norm]),
                r = round(abs(r), 3)
            )
    })
    
    #### Pearson's R Bar Chart ----
    
    category_colors <- c(
        Output                   = "#1f78b4",
        `Labor Market`           = "#33a02c",
        `Price Levels`           = "#6a3d9a",
        `Monetary & Fiscal`      = "#e31a1c",
        `Housing & Construction` = "#2D7C66"#,
        #Trade                    = "#E40078"
    )
    
    # function to lower opacity of unselected bars
    with_alpha <- function(hex, a = 0.30){
        rgb <- grDevices::col2rgb(hex)
        sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], a)
    }
    
    output$pearsons_plot_hc <- renderHighchart({
        
        cor_df <- cor_df_reactive()
        initial_inds <- category_map[[ isolate(input$select1) ]]
        
        series_list <- lapply(seq_len(nrow(cor_df)), function(i){
            id   <- cor_df$Indicator[i]
            base <- cor_df$color[i]                   # category color
            dimm <- with_alpha(base, 0.30)
            
            list(
                id            = id,
                name          = cor_df$Label[i],
                color         = if (id %in% initial_inds) base else dimm,
                origColor     = base,
                inactiveColor = dimm,
                data          = list(list(x = i - 1, y = cor_df$r[i])),
                showInLegend  = FALSE
            )
        })
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(type = "bar") %>%
            hc_xAxis(categories = unname(cor_df$Label), 
                     reversed = TRUE,
                     gridLineWidth = 0) %>%
            hc_yAxis(title = list(
                text = "Pearson's r (Absolute Value)"),
                gridLineWidth = 0) %>%
            hc_title(text = "Correlation to Consumer Sentiment",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_subtitle(text = "The greater the value, the more correlated the indicator is with Consumer Sentiment") %>%
            hc_add_series_list(series_list) %>%
            hc_caption(
                useHTML = TRUE,
                align = "center", 
                text = paste0(
                    "<div style='text-align:center;'>",
                    "<span style='font-weight:bold;color:#1f78b4'>&#9632;</span> Output&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#33a02c'>&#9632;</span> Labor Market&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#6a3d9a'>&#9632;</span> Price Levels&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#e31a1c'>&#9632;</span> Monetary & Fiscal&nbsp;&nbsp;",
                    "<span style='font-weight:bold;color:#ff7f00'>&#9632;</span> Housing & Construction",
                    "</div>"
                )
            ) %>%
            hc_plotOptions(bar = list(pointWidth = 15))
    })
    
    #update bar chart when new combo box item is selected
    observeEvent(input$select1, {
        req(isTRUE(CAT_PLOT_LOADED))
        cor_df <- cor_df_reactive()
        sel_inds <- category_map[[ input$select1 ]]
        
        proxy <- highchartProxy("pearsons_plot_hc", session = session)
        
        for (i in seq_len(nrow(cor_df))) {
            id   <- cor_df$Indicator[i]
            base <- cor_df$color[i]              # category color from DF
            dimm <- with_alpha(base, 0.30)
            
            proxy %>% hcpxy_update_series(
                id    = id,
                color = if (id %in% sel_inds) base else dimm
            )
        }
    })
    
    ## Hidden Markov Model Code ----
    
    ### Algorithm VisNetwork Diagram ----
    
    nodes_core <- data.frame(
        id    = c("init","estep","mstep","ll","check","done","rep_em"),
        label = c(
            "Initialize\nθ",
            "E-step",
            "M-step",
            "Compute\nlog-likelihood",
            "Convergence?",
            "Done",
            "Repeat\n(E/M)"
        ),
        stringsAsFactors = FALSE
    )
    
    # Per-node styling (defaults)
    nodes_core$shape <- "box"
    nodes_core$`color.background` <- "#f8f9fa"
    nodes_core$`color.border`     <- "#6c757d"
    nodes_core$`color.highlight.background` <- "#d4edda"
    nodes_core$`color.highlight.border`     <- "#28a745"
    nodes_core$font.size  <- 32
    nodes_core$font.face  <- "Helvetica"
    nodes_core$font.align <- "center"
    
    # Keep Repeat as an ellipse
    nodes_core$shape[nodes_core$id == "rep_em"] <- "ellipse"
    nodes_core$`color.background`[nodes_core$id == "rep_em"] <- "#fafafa"
    
    lvl_map <- c(init=0, estep=1, mstep=2, ll=3, rep_em=4, check=5, done=6)
    nodes_core$level <- unname(lvl_map[nodes_core$id])
    nodes_core$group <- ifelse(nodes_core$id == "rep_em", "rep_em_pill", "main")
    
    # Forward spine
    edges_fwd <- data.frame(
        from   = c("init","estep","mstep","ll","check"),
        to     = c("estep","mstep","ll","check","done"),
        label  = c("","","","","Yes"),
        arrows = "to",
        dashes = FALSE,
        smooth = FALSE,
        stringsAsFactors = FALSE
    )
    
    # Loop via Repeat (dashed)
    edges_loop <- data.frame(
        from   = c("check","rep_em"),
        to     = c("rep_em","estep"),
        label  = c("No",""),
        arrows = "to",
        dashes = TRUE,
        smooth = TRUE,
        stringsAsFactors = FALSE
    )
    
    edges_all <- rbind(edges_fwd, edges_loop)
    
    
    dy  <- 160
    x0  <- 0
    
    pos <- data.frame(
        id = c("init","estep","mstep","ll","rep_em","check","done"),
        x  = c(x0,  x0,   x0,   x0,   x0 + 340, x0,   x0),
        y  = c(0,    dy,  2*dy, 3*dy, 3*dy + 30, 4*dy, 5*dy)
    )
    
    # merge positions & lock them
    nodes_plot <- nodes_core |>
        dplyr::select(-dplyr::any_of("level")) |>
        dplyr::left_join(pos, by = "id") |>
        dplyr::mutate(fixed.x = TRUE, fixed.y = TRUE) 
    
    output$em_flow <- renderVisNetwork({
        visNetwork(nodes_plot, edges_all, height = "60vh", width = "100%") %>%
            visPhysics(enabled = FALSE) %>%
            visNodes(
                shape = "box",
                margin = list(top=14,right=18,bottom=14,left=18),
                widthConstraint = list(minimum = 220, maximum = 340),
                color = list(
                    background = "#f8f9fa", border = "#6c757d",
                    highlight  = list(background = "#d4edda", border = "#28a745")
                ),
                font  = list(size = 32, face = "Helvetica", align = "center")
            ) %>%
            visGroups(groupname = "rep_em_pill", font = list(size = 26), inherit = FALSE) %>%
            visEdges(
                color  = list(color = "#6c757d"),
                smooth = list(enabled = TRUE, type = "cubicBezier", roundness = 0.35),
                font   = list(size = 20, align = "middle")
            ) %>%
            visInteraction(dragNodes = FALSE, dragView = F, zoomView = F, hover = T) %>%
            # Select 'init' once after first paint (no physics => no 'stabilized' event)
            
            visEvents( selectNode = "function(p){ if(window.Shiny && p.nodes && p.nodes.length){ Shiny.setInputValue('em_node_selected', 
                       p.nodes[0], {priority:'event'}); 
                       } 
                       }", 
                       
                       deselectNode = "function(p){ if(window.Shiny){ Shiny.setInputValue('em_node_selected', null, {priority:'event'}); 
                            } 
                       }",
                      afterDrawing = "
                      function(){
                        if(!this._didInit){
                          this._didInit = true;
                          this.selectNodes(['init']);
                          if(window.Shiny){
                            Shiny.setInputValue('em_node_selected','init',{priority:'event'});
                          }
                        }
                      }
    ")
    })
    
    ### Algorithm text ----
    em_text <- lapply(list(
        init = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 1: Initialize Parameters</h4>
    <p>
      The algorithm begins by making an initial guess for the model\'s parameters, collectively known as \\(\\theta\\). 
      These parameters define the model\'s starting beliefs about the system.
      $$ \\theta = \\{\\pi, A, B \\} $$
      This includes \\(\\pi\\), the probability of starting in each hidden state; <b>A</b>, the probability of transitioning 
      between hidden states; and <b>B</b>, the probability of seeing an observation from a given hidden state. For our weather 
      example, we might guess there\'s a 50% chance the first day is <em>Sunny</em> (\\(\\pi\\)), a 10% chance a <em>Sunny</em> 
      day is followed by a <em>Rainy</em> one (in matrix A), and an 80% chance a <em>Sunny</em> day results in an <em>Arid</em> 
      observation (in matrix B).
    </p>
        </div>',
        
        estep = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 2: The E-Step (Expectation)</h4>
    <p>
      Using its current parameters, the model calculates the probability of each hidden state being the true state for every point in 
      our observed data sequence. It does this using a formula for \\(\\gamma_t(i)\\):
      $$ \\gamma_t(i) = P(z_t=i | X, \\theta) $$
      This equation calculates the probability that the hidden state \\(z\\) at time \\(t\\) was state \\(i\\) (e.g., <em>Sunny</em>), 
      given the entire sequence of observations \\(X\\) (e.g., Arid, Humid, Humid...). Rather than making a hard decision, it creates 
      a soft, probabilistic map. For a day we observed <em>Arid</em> conditions, this step might conclude there\'s an 85% probability 
      the underlying state was <em>Sunny</em> and a 15% probability it was <em>Rainy</em>.
    </p>
        </div>',
        
        mstep = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 3: The M-Step (Maximization)</h4>
    <p>
      With the probabilistic map from the E-step, the model updates its parameters to better explain the data. For example, the 
      transition probabilities in matrix <b>A</b> are re-calculated based on the expected number of transitions that occurred.
       $$ A_{ij} = \\frac{\\text{Expected # of transitions from }i \\text{ to } j}{\\text{Expected # of transitions from } i} $$
      If the E-step frequently found that a high-probability <em>Sunny</em> day was followed by a high-probability <em>Rainy</em> day, 
      this M-step will increase the value for the <em>Sunny</em> to <em>Rainy</em> transition. The same logic is applied to update the 
      initial state (\\(\\pi\\)) and emission (<b>B</b>) probabilities, ensuring the new parameters are a better fit for the data.
    </p>
        </div>',
        
        ll = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 4: Compute Log-Likelihood</h4>
    <p>
      After updating the parameters, the algorithm "scores" how well the new model explains the observed data by calculating the 
      log-likelihood.
      $$ L(\\theta) = \\log P(X|\\theta) $$
      This value, \\(L(\\theta)\\), is the logarithm of the total probability of observing our specific sequence of data 
      (e.g., Arid, Humid, Humid...) given the current model parameters. In a properly functioning EM algorithm, this score 
      should increase with each iteration, signaling that the model is getting progressively better.
    </p>
        </div>',
        
        check = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 5: Check for Convergence</h4>
    <p>
      The algorithm must decide whether to stop or perform another iteration. It stops if the improvement in the log-likelihood score becomes 
      negligible, or if a maximum number of cycles is reached. $$ \\Delta L < \\epsilon $$
      This condition checks if the change in log-likelihood (\\(\\Delta L\\)) is less than a tiny tolerance value (\\(\\epsilon\\)). 
      When the probabilities for <em>Sunny/Rainy</em> transitions and <em>Arid/Humid</em> emissions barely change from one cycle to the
      next, we can be confident the model has converged on a stable, locally optimal solution.
    </p>
        </div>',
        
        rep_em = '
        <div style="font-size: 18px; font-family: Helvetica;">
        <h4>Repeat Cycle: Iterative Refinement</h4>
  <p>
    If the model has not yet converged, the algorithm loops back to the E-step to begin a new cycle. This iterative process is the heart 
    of how the 
    model learns. The refined parameters from the previous M-step are now used as the new "current" parameters for the E-step.
  </p>
  <p>
    $$ \\theta_{old} \\rightarrow \\text{E-Step} \\rightarrow \\text{M-Step} \\rightarrow \\theta_{new} $$
            Because the new parameters (e.g., the updated probability of a <i>Sunny</i> to <i>Rainy</i> transition) are guaranteed to be a better 
            fit for the data, the subsequent E-step will produce a more accurate probabilistic map of the hidden states. This improved map, in turn, 
            allows the next M-step to find an even better set of parameters, further increasing the log-likelihood score. 
            This cycle of refinement continues, with each loop bringing the model closer to an optimal solution.
        </p>
        </div>
        ',
        
        done = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 6: Algorithm Complete</h4>
    <p>
      Once convergence is reached, the algorithm terminates. The final set of parameters, \\(\\theta^*\\), 
      represents the fully trained model. These optimized probabilities capture the underlying structure of the training data. 
      For example, the final model might have learned that a <em>Sunny</em> day has a 95% chance of being followed by another 
      <em>Sunny</em> day, and an 85% chance of producing an <em>Arid</em> observation. This trained model can now be used for 
      analysis or to predict hidden states from new data.
    </p>
        </div>'
    ), HTML)
    
    output$em_step_details <- renderUI({
        id <- input$em_node_selected
        if (is.null(id)) return(div(style="color:#6c757d;", htmltools::tags$em("Click a step to see details.")))
        withMathJax(HTML(
            em_text[[id]]
            
            ))
    })
    
    ### Probability Matrices ----
    
    # Transition probabilities matrix
    output$matrix_ui <- renderUI({
        withMathJax(HTML('
          <div id="trans-mx" class="mx" style="font-size:160%; text-align:center;">
            \\[
              \\left[
              \\begin{array}{c|cc}
                  & ☀ & 🌧 \\\\ \\hline
                ☀  & P_{☀to☀} & P_{☀to🌧} \\\\
                🌧 & P_{🌧to☀} & P_{🌧to🌧}
              \\end{array}
              \\right]
            \\]
          </div>
          '))
    })

    output$emissions_mat <- renderUI({
        withMathJax(HTML('
          <div id="emiss-mx" class="mx" style="font-size:160%; text-align:center;">
            \\[
              \\left[
              \\begin{array}{c|cc}
                  & ☀ & 🌧 \\\\ \\hline
                ☀  & B_{SS} & B_{SR} \\\\
                🌧 & B_{RS} & B_{RR}
              \\end{array}
              \\right]
            \\]
          </div>
          '))
    })
    
    ### HMM Simulation Code ----
    
    pi0 <- c(0.5, 0.5)  
    A0  <- matrix(c(0.75,0.25,
                    0.20,0.80), 2, byrow = TRUE)
    B0  <- matrix(c(0.70,0.30,
                    0.40,0.60), 2, byrow = TRUE)
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    .draw_cat <- function(prob) {
        p <- as.numeric(prob)
        p[!is.finite(p) | p < 0] <- 0
        s <- sum(p)
        if (s <= 0) p[] <- 1/length(p) else p <- p/s
        sample.int(length(p), 1L, prob = p)
    }
    
    # error checking for probability matrices
    .validate_matrix <- function(M, name, tol = 1e-8) {
        errs <- character(0)
        if (any(!is.finite(M)))            errs <- c(errs, sprintf("%s has non-finite entries.", name))
        if (any(M < 0 | M > 1, na.rm=TRUE))errs <- c(errs, sprintf("%s entries must be in [0,1].", name))
        rs <- rowSums(M)
        bad <- which(abs(rs - 1) > tol)
        if (length(bad)) errs <- c(errs, sprintf("%s row(s) %s must sum to 1 (got %s).",
                                                 name, paste(bad, collapse=", "),
                                                 paste(round(rs[bad], 4), collapse=", ")))
        errs
    }
    
    A_input <- reactive({
        matrix(as.numeric(c(
            input$A11 %||% A0[1,1], input$A12 %||% A0[1,2],
            input$A21 %||% A0[2,1], input$A22 %||% A0[2,2]
        )), nrow = 2, byrow = TRUE)
    })
    B_input <- reactive({
        matrix(as.numeric(c(
            input$B11 %||% B0[1,1], input$B12 %||% B0[1,2],
            input$B21 %||% B0[2,1], input$B22 %||% B0[2,2]
        )), nrow = 2, byrow = TRUE)
    })
    
    # equal initial probabilities (no UI for π)
    R_pi <- reactive(pi0)
    
    # central gatekeeper: check everything BEFORE sim
    prob_errors <- reactive({
        c(.validate_matrix(A_input(), "Transition (A)"),
          .validate_matrix(B_input(), "Emission (B)"))
    })
    
    demo_data <- eventReactive(input$hmm_run_demo, {
        errs <- prob_errors()
        if (length(errs)) {
            showNotification(HTML(paste(errs, collapse = "<br>")), type = "error", duration = 6)
            return(NULL)
        }
        
        pi <- R_pi()
        A  <- A_input()
        B  <- B_input()
        
        Tn     <- input$hmm_T
        states <- c("S1","S2")
        obs_lv <- c("Humid","Arid")
        
        # --- simulate HMM path ---
        z <- x <- integer(Tn)
        z[1] <- .draw_cat(pi)
        x[1] <- .draw_cat(B[z[1], ])
        for (t in 2:Tn) {
            z[t] <- .draw_cat(A[z[t - 1], ])
            x[t] <- .draw_cat(B[z[t], ])
        }
        
        # --- robust EM fit (avoid 'contrasts' crash) ---
        df <- data.frame(obs = factor(obs_lv[x], levels = obs_lv))
        
        if (length(unique(df$obs)) < 2L) {
            miss <- setdiff(obs_lv, as.character(unique(df$obs)))
            df2  <- rbind(df, data.frame(obs = factor(miss[1], levels = obs_lv)))
            nt   <- c(nrow(df), 1L)  # two sequences: main + 1 dummy row
        } else {
            df2 <- df
            nt  <- nrow(df)
        }
        
        mod <- depmixS4::depmix(
            obs ~ 1, data = df2, nstates = 2,
            family = depmixS4::multinomial("identity"),
            ntimes = nt
        )
        
        fit  <- depmixS4::fit(mod, verbose = FALSE)
        post <- depmixS4::posterior(fit, type = "viterbi")
        zh   <- head(post$state, nrow(df))
        
        list(
            t  = seq_len(Tn),
            x  = factor(obs_lv[x],  levels = obs_lv),
            z  = factor(states[z],  levels = states),
            zh = factor(states[zh], levels = states),
            A  = A,             
            B  = B
        )
    }, ignoreInit = TRUE)
    
    ### Simulation Plot ----
    
    output$hmm_demo_timeline <- renderHighchart({
        dd <- demo_data(); req(dd)
        
        cols_state <- c(S1 = "#F6C54E", S2 = "#4DA3FF")      
        cols_obs   <- c(Humid = "#808080", Arid = "#D35400") 
        
        n <- length(dd$t)
        dot   <- max(6, min(18, 150 / n))
        cross <- round(dot * 4.0)
        
        cats <- c("Observation","True state","Decoded")
        yObs <- 0; yTrue <- 1; yDec <- 2
        
        mkpt <- function(x, y, fill)
            list(x = x, y = y,
                 marker = list(radius = dot, symbol = "circle",
                               fillColor = fill, lineColor = "#222", lineWidth = 1))
        
        obs_data  <- Map(function(t, cl) mkpt(t, yObs,  cl), dd$t, cols_obs[as.character(dd$x)])
        true_data <- Map(function(t, cl) mkpt(t, yTrue, cl), dd$t, cols_state[as.character(dd$z)])
        dec_data  <- Map(function(t, cl) mkpt(t, yDec,  cl), dd$t, cols_state[as.character(dd$zh)])
        
        wrong <- which(as.character(dd$z) != as.character(dd$zh))
        err_data <- lapply(dd$t[wrong], function(t) {
            list(x = t, y = yDec,
                 dataLabels = list(enabled = TRUE, useHTML = TRUE, crop = FALSE, overflow = "none",
                                   align = "center", verticalAlign = "middle",
                                   y = round(cross * -0.05),
                                   style = list(color = "#cc0000", fontWeight = "900",
                                                fontFamily = "Arial, sans-serif",
                                                fontSize = paste0(cross, "px"),
                                                textOutline = "none", pointerEvents = "none"),
                                   format = "&times;"),
                 marker = list(enabled = FALSE))
        })
        
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
            '      <td style="padding-left: 20px;"><span class="legend-swatch" style="background-color:', cols_obs["Humid"], ';"></span></td><td class="legend-label">Humid</td>',
            '    </tr>',
            '    <tr>',
            '      <td><span class="legend-swatch" style="background-color:', cols_state["S2"], ';"></span></td><td class="legend-label">Rainy</td>',
            '      <td style="padding-left: 20px;"><span class="legend-swatch" style="background-color:', cols_obs["Arid"], ';"></span></td><td class="legend-label">Arid</td>',
            '    </tr>',
            '    <tr>',
            '      <td><div class="legend-x">&times;</div></td><td class="legend-label">Decode Error</td>',
            '      <td></td><td></td>',
            '    </tr>',
            '  </table>',
            '</div>'
        )
        
        hc <- highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(
                type = "scatter",
                spacingLeft = 8, spacingRight = 8,
                spacingBottom = 140,                      # room for legend below the plot
                events = list(
                    load = JS(sprintf(
                        "function(){
          var html = '%s';
          var L = this.renderer.html(html, 0, 0).add();
          this.customLegend = L;

          // helper to place legend centered under the plot area
          var place = function(){
            var node = L.element.firstChild;               // outer <div> of your legend
            var W = node ? node.getBoundingClientRect().width  : 260;
            var H = node ? node.getBoundingClientRect().height : 60;

            // ensure enough bottom spacing for the measured legend height
            var need = H + 22;                              // legend height + breathing room
            if (this.options.chart.spacingBottom < need) {
              this.update({ chart: { spacingBottom: need } }, false);
            }

            // center under the plot box (not the whole chart)
            var x = this.plotLeft + (this.plotWidth  - W) / 2;
            var y = this.plotTop  +  this.plotHeight + 10;  // a bit under the axis line
            L.attr({ x: x, y: y });
          }.bind(this);

          place();
        }",
                        gsub("'", "\\\\'", legend_html)
                    )),
                    render = JS("
        function(){
          if (!this.customLegend) return;
          var L = this.customLegend;
          var node = L.element.firstChild;
          var W = node ? node.getBoundingClientRect().width  : 260;
          var H = node ? node.getBoundingClientRect().height : 60;

          var x = this.plotLeft + (this.plotWidth  - W) / 2;
          var y = this.plotTop  +  this.plotHeight + 50;
          L.attr({ x: x, y: y });
        }
      ")
                )
            ) %>%
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
        
        hc %>% hc_tooltip(enabled = FALSE) %>% hc_credits(enabled = FALSE)
    })
    
    ### HMM Diagram with labels ----
    
    items <- list(
        # initial π
        list(id="pi_sun", cls="pi",   left=35, top=30, col="#ADD8E6",
             label="π(Sunny)", info="<b>π(Sunny)</b> = The likelihood the model will start at <b> Sunny </b>"),
        list(id="pi_rain", cls="pi",  left=65, top=30, col="#ADD8E6",
             label="π(Rainy)", info="<b>π(Rainy)</b> = The likelihood the model will start at <b> Rainy </b>"),
        
        # transitions A
        list(id="p11", cls="trans", left=10, top=57, col="#e74c3c",
             label="P(Sunny to Sunny)",
             info="<b>P(Sunny to Sunny)</b> = The probability it will be <b> Sunny </b> tomorrow
             given that it is <b> Sunny </b> today"),
        list(id="p12", cls="trans", left=50, top=46.5, col="#e74c3c",
             label="P(Sunny to Rainy)",
             info="<b>P(Sunny to Rainy)</b> = The probability it will be <b> Rainy </b> tomorrow
             given that it is <b> Sunny </b> today"),
        list(id="p21", cls="trans", left=50, top=70, col="#e74c3c",
             label="P(Rainy to Sunny)",
             info="<b>P(Rainy to Sunny)</b> = The probability it will be <b> Sunny </b> tomorrow
             given that it is <b> Rainy </b> today"),
        list(id="p22", cls="trans", left=80, top=57, col="#e74c3c",
             label="P(Rainy to Rainy)",
             info="<b>P(Rainy to Rainy)</b> = The probability it will be <b> Rainy </b> tomorrow
             given that it is <b> Rainy </b> today"),
        
        # emissions B
        list(id="b11", cls="emit", left=63, top=85, col="#27ae60",
             label="Pr(Humid | Sunny)",
             info="<b>Pr(Humid | Sunny)</b> = The likelihood the air is <b> Humid </b> given that it is  <b> Sunny </b>"),
        list(id="b12", cls="emit", left=22, top=80, col="#27ae60",
             label="Pr(Arid | Sunny)",
             info="<b>Pr(Arid | Sunny)</b> = The likelihood the air is <b> Arid </b> given that it is <b> Sunny </b>"),
        list(id="b21", cls="emit", left=77, top=75, col="#27ae60",
             label="Pr(Humid | Rainy)",
             info="<b>Pr(Humid | Rainy)</b> = The likelihood the air is <b> Humid </b> given that it is <b> Rainy </b>"),
        list(id="b22", cls="emit", left=38, top=85, col="#27ae60",
             label="Pr(Arid | Rainy)",
             info="<b>Pr(Arid | Rainy)</b> = The likelihood the air is <b> Arid </b> given that it is <b> Rainy </b>")
    )
    
    
    svg_file <- "HMM_Diagram.svg"
    
    output$hmm_overlay_pi <- renderUI({
        tags$div(
            id = "hmmwrap_all",
            tags$img(
                src = svg_file,
                class = "base-svg no-dark-invert",  # <- add this
                alt = "HMM diagram"
            ),
            lapply(items, function(hs){
                tags$div(
                    id = paste0("hs_", hs$id),
                    class = paste("hs", hs$cls),
                    tabindex = "0",
                    style = sprintf("left:%s%%; top:%s%%;", hs$left, hs$top),
                    tags$span(class = "tag", hs$label),
                    tags$div(class = "callout", HTML(hs$info))
                )
            })
        )
    })    
    ### Detailed Simulation Table ----
    
    state_name <- c(S1 = "Sunny", S2 = "Rainy")
    state_sym  <- c(S1 = "☀",     S2 = "🌧")
    obs_name   <- c(Humid = "Humid", Arid = "Arid")
    
    prob_style <- function(value) {
        if (is.na(value)) return(list())
        bg <- if (value >= 0.8) "#e8f7e4" else if (value <= 0.2) "#fde8e8" else "transparent"
        list(background = bg)
    }
    
    detail_df <- reactive({
        dd <- demo_data(); req(dd)
        
        A <- dd$A; B <- dd$B
        n <- length(dd$t)
        
        # factor to strings/indices
        z_chr  <- as.character(dd$z)
        x_chr  <- as.character(dd$x)
        zh_chr <- as.character(dd$zh)
        
        z_i <- as.integer(dd$z)
        x_i <- as.integer(dd$x)
        
        trans_prob <- c(NA_real_, vapply(2:n, function(t) A[z_i[t-1], z_i[t]], numeric(1)))
        emit_prob  <- vapply(1:n,  function(t) B[z_i[t],   x_i[t]],   numeric(1))
        
        state_sym  <- c(S1 = "☀",  S2 = "🌧")
        state_name <- c(S1 = "Sunny", S2 = "Rainy")
        obs_name   <- c(Humid = "Humid", Arid = "Arid")
        
        data.frame(
            Time        = dd$t,
            Transition  = c("-", paste0(state_sym[z_chr[-n]], " → ", state_sym[z_chr[-1]])),
            A_prob      = round(trans_prob, 3),
            Observation = paste0(obs_name[x_chr], " | ", state_name[z_chr]),
            B_prob      = round(emit_prob, 3),
            True        = state_name[z_chr],
            Decoded     = state_name[zh_chr],
            Error       = ifelse(z_chr != zh_chr, "✗", ""),
            check.names = FALSE
        )
    })
    
    output$hmm_detail_table <- reactable::renderReactable({
        df <- detail_df()
        req(df)
        
        reactable::reactable(
            df,
            pagination = FALSE,
            bordered   = TRUE,
            striped    = TRUE,
            highlight  = TRUE,
            defaultColDef = reactable::colDef(align = "center"),
            columns = list(
                Transition  = reactable::colDef(header = "z(t−1) to z(t)"),
                A_prob      = reactable::colDef(
                    name  = "A[z(t−1), z(t)]",
                    style = function(value) prob_style(value),
                    format = reactable::colFormat(digits = 3)
                ),
                Observation = reactable::colDef(header = "x(t) | z(t)"),
                B_prob      = reactable::colDef(
                    name  = "B[z(t), x(t)]",
                    style = function(value) prob_style(value),
                    format = reactable::colFormat(digits = 3)
                ),
                Error       = reactable::colDef(html = TRUE,
                                                cell = function(value)
                                                    if (nzchar(value))
                                                        htmltools::tags$span(style="color:#cc0000;font-weight:900;", "\u00D7")
                )
            ),
            columnGroups = list(
                reactable::colGroup(name = "Transition", columns = c("Transition","A_prob")),
                reactable::colGroup(name = "Emission",   columns = c("Observation","B_prob"))
            ),
            theme = reactable::reactableTheme(
                borderColor    = "#ddd",
                stripedColor   = "#fafafa",
                highlightColor = "#f5f9ff"
            )
        )
    })
    
    output$more_btn <- renderUI({
        req(demo_data())
        div(
            actionButton("show_details", "More details", icon = icon("table")),
            style = "margin-top:.5rem; display:flex; justify-content:flex-end;"
        )
    })
    
    observeEvent(input$show_details, {
        shinyjs::hide("sim_shell")
        shinyjs::show("details_shell")
    })
    
    observeEvent(input$back_to_chart, {
        shinyjs::show("sim_shell")
        shinyjs::hide("details_shell")
    })
    
    
    
    ## Analysis Code ----
    pretty_map <- c(
        real_gdp = "Real GDP (t−1)",
        pcepi = "PCE Price Index (t)",
        fyfsd = "Federal Surplus/Deficit (t−1)",
        unemployment_rate = "Unemployment Rate (t−1)",
        case_schiller_val = "Case–Shiller Index (t−1)"
    )
    
    six_indicator_formats <- c(
        real_gdp = "$",
        pcepi = "%",
        fyfsd = "$",
        unemployment_rate = "%",
        case_schiller_val = "",
        consumer_sentiment = ""
    )
    
    
    ### Indicator Gallery ----
    
    plot_bands <- lapply(seq_len(nrow(recessions)), function(i) {
        list(
            from   = datetime_to_timestamp(as.Date(recessions$start[i])),
            to     = datetime_to_timestamp(as.Date(recessions$end[i])),
            color  = recessions$color[i],  # red/red/yellow with alpha
            zIndex = 0,                    # behind the lines
            label  = list(                 # optional on-band label
                text     = recessions$name[i],
                rotation = 0,
                align    = "center",
                y        = -6,
                style    = list(color = "#444", fontSize = "10px", fontWeight = "bold")
            )
        )
    })
    
    output$overview_ts <- renderHighchart({
        overview_vars <- c("real_gdp", "pcepi", "fyfsd", "unemployment_rate", "case_schiller_val")
        lag_map <- c(real_gdp = 1, pcepi = 0, fyfsd = 1, unemployment_rate = 1, case_schiller_val = 1)
        
        dat <- full_dataset[order(full_dataset$Year), ]
        x_ts <- datetime_to_timestamp(as.Date(paste0(dat$Year, "-01-01")))
        
        series_list <- list()
        for (nm in overview_vars) {
            v <- dat[[nm]]
            L <- if (nm %in% names(lag_map)) lag_map[[nm]] else 0L
            if (L > 0) v <- c(rep(NA_real_, L), head(v, -L))
            series_list[[nm]] <- v
        }
        cs <- dat$consumer_sentiment
        
        df_all <- cbind(cs = cs, as.data.frame(series_list))
        keep    <- stats::complete.cases(df_all)
        df_all <- df_all[keep, , drop = FALSE]
        x_keep <- x_ts[keep]
        
        z_all <- as.data.frame(lapply(df_all, function(col) as.numeric(scale(col))))
        
        col_cs <- "#000000" 
        cols_ind <- c("#66C7B4", "#FFB45A", "#8D6E63", "#EA7369", "#8A9AFB")
        
        hc <- highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>% 
            hc_title(
                text = "Consumer Sentiment and Selected Economic Indicators",
                style = list(fontWeight = "bold", fontSize = "16px")
            ) %>%
            hc_subtitle(
                text = "All Indicators are Scaled for Visual Cohesiveness <br> (Z-Score: Mean of 0, SD of 1) <br>ㅤ"
            ) %>%
            hc_chart(zoomType = "x") %>%
            hc_credits(enabled = FALSE) %>%
            hc_xAxis(
                type = "datetime",
                gridLineWidth = 0,
                plotBands = plot_bands,
                crosshair = list(
                    color = "darkgrey",
                    width = 1,
                    dashStyle = "Solid"
                )
            ) %>%
            hc_yAxis(
                labels = list(enabled = FALSE),
                gridLineDashStyle = "Dot",
                gridLineColor = "darkgrey",
                crosshair = list(
                    color = "darkgrey",
                    width = 1,
                    dashStyle = "Solid"
                )
            ) %>%
            hc_legend(
                align = "center",
                verticalAlign = "bottom",
                layout = "horizontal"
            ) %>%
            hc_plotOptions(
                series = list(
                    marker = list(enabled = FALSE),
                    states = list(
                        hover = list(
                            lineWidthPlus = 1.5 
                        ),
                        inactive = list(
                            opacity = 0.2
                        )
                    )
                )
            )
        
        hc <- hc %>% hc_add_series(
            type = "line", 
            name = "Consumer Sentiment",
            data = purrr::pmap(
                list(x = x_keep, y = z_all$cs, true = df_all$cs),
                function(x, y, true) {
                    list(x = x, y = y, trueValue = true, format = six_indicator_formats[["consumer_sentiment"]])
                }
            ),
            color = col_cs, 
            lineWidth = 3.5,
            zIndex = 10,
            showInLegend = TRUE
        )
        
        i <- 0
        for (nm in overview_vars) {
            i <- i + 1
            nm_pretty <- if (exists("pretty_map") && nm %in% names(pretty_map)) pretty_map[[nm]] else nm
            
            current_format <- six_indicator_formats[[nm]]
            
            hc <- hc %>% hc_add_series(
                type = "line", 
                name = nm_pretty,
                data = purrr::pmap(
                    list(x = x_keep, y = z_all[[nm]], true = df_all[[nm]]),
                    function(x, y, true) {
                        list(x = x, y = y, trueValue = true, format = current_format)
                    }
                ),
                color = cols_ind[(i - 1) %% length(cols_ind) + 1],
                lineWidth = 1.8,
                opacity = 0.75,
                showInLegend = TRUE
            )
        }
        
        hc %>%
            hc_tooltip(
                shared = TRUE,
                crosshairs = TRUE,
                useHTML = TRUE,
                backgroundColor = "rgba(255, 255, 255, 0.95)",
                borderColor = "#B0B0B0",
                borderRadius = 8,
                borderWidth = 1,
                shadow = TRUE,
                style = list(color = "#333333", fontSize = "12px"),
                formatter = JS("
              function () {
                var s = '<b>' + Highcharts.dateFormat('%Y', this.x) + '</b><br/>';
                this.points.forEach(function(p){
                  
                  var v = p.point.trueValue;
                  var fmt = p.point.format || '';
                  var txt;
                  
                  if (fmt === '$') {
                    var sign = (v < 0 ? '-' : '');
                    var absv = Highcharts.numberFormat(Math.abs(v), 2);
                    txt = sign + '$' + absv;
                  } else if (fmt === '%') {
                    txt = Highcharts.numberFormat(v, 2) + '%';
                  } else {
                    txt = Highcharts.numberFormat(v, 2);
                  }
                  
                  s += '<span style=\"color:' + p.series.color + '\">' + p.series.name +
                       '</span>: <b>' + txt + '</b><br/>';
                });
                return s;
              }
            ")
            )
    })
    
    # Map button clicks -> index 1..5 based on *current* plot_df ordering
    model_idx <- reactiveVal(1L)  # default = first button
    
    observeEvent(input$mod_one,   ignoreInit = TRUE, { model_idx(1L) })
    observeEvent(input$mod_two,   ignoreInit = TRUE, { model_idx(2L) })
    observeEvent(input$mod_three, ignoreInit = TRUE, { model_idx(3L) })
    observeEvent(input$mod_four,  ignoreInit = TRUE, { model_idx(4L) })
    observeEvent(input$mod_five,  ignoreInit = TRUE, { model_idx(5L) })
    
    ### Model Selection Bar Chart ----
    
    # the bar chart displaying the mean LL for the 5 best models
    output$model_selection_plot <- renderHighchart({
        sel_idx <- 1L
        sel_pos <- sel_idx - 1
        
        cats  <- plot_df$label
        ybar  <- plot_df$mean_delta_ll
        ci_lo <- plot_df$ci_lo
        ci_hi <- plot_df$ci_hi
        
        # Best model stays green (#28a745), others aquamarine
        best_idx <- if ("rank" %in% names(plot_df)) which(plot_df$rank == 1)[1] else which.max(ybar)
        col_best <- "#28a745"
        col_oth <- "#00aaff"
        band_col <- "rgba(0,191,166,0.12)"
        
        #col_oth <- "#54D7CA"
        
        data_list <- lapply(seq_along(ybar), function(i){
            list(y = ybar[i], color = if (i == best_idx) col_best else col_oth)
        })
        err_js <- sprintf("[%s]", paste(sprintf("[%.6f,%.6f]", ci_lo, ci_hi), collapse = ","))
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(type = "bar", id = session$ns("model_selection_plot_chart")) %>%
            hc_title(text = "Mean Test Log-Likelihood vs Baseline",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_subtitle(text = "Bars: Δ Mean Test LL <br> Whiskers: 95% CI") %>%
            hc_xAxis(
                categories = cats,
                plotBands  = list(list(id = "sel", from = sel_pos - 0.5, to = sel_pos + 0.5,
                                       color = band_col, zIndex = -1))
            ) %>%
            hc_yAxis(title = list(text = "Δ mean LL per obs"),
                     plotLines = list(list(value = 0, color = "#888", width = 1))) %>%
            hc_plotOptions(series = list(
                dataLabels = list(enabled = TRUE, format = "{point.y:.3f}",
                                  style = list(color = "#000", textOutline = "none"))
            )) %>%
            hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:.3f}") %>%
            hc_add_series(name = "Δ Mean LL", data = data_list, showInLegend = FALSE) %>%
            hc_add_series(type = "errorbar", name = "95% CI", data = JS(err_js), showInLegend = FALSE) %>%
            # Custom handler to move only the band
            htmlwidgets::onRender("
                              function(el, x) {
                                function getChart() {
                                  if (window.Highcharts && Highcharts.charts) {
                                    var idx = +el.getAttribute('data-highcharts-chart');
                                    if (!isNaN(idx) && Highcharts.charts[idx]) return Highcharts.charts[idx];
                                    // fallback: scan by DOM containment
                                    for (var i = 0; i < Highcharts.charts.length; i++) {
                                      var c = Highcharts.charts[i];
                                      if (c && c.renderTo && (c.renderTo === el || el.contains(c.renderTo))) return c;
                                    }
                                  }
                                  return null;
                                }
                            
                                var chart = getChart();
                                if (!chart) { console.warn('No Highcharts chart found for', el.id); return; }
                            
                                var channel = 'hc-bar-select-' + el.id;
                            
                                Shiny.addCustomMessageHandler(channel, function(msg){
                                  var idx = (msg.index || 1) - 1;       // 0-based bar index
                                  var xa  = chart.xAxis && chart.xAxis[0];
                                  if (!xa) { console.warn('No xAxis on chart'); return; }
                            
                                  try { xa.removePlotBand('sel'); } catch(e) {}
                                  xa.addPlotBand({
                                    id: 'sel',
                                    from: idx - 0.5, to: idx + 0.5,
                                    color: msg.bandColor || 'rgba(0,191,166,0.12)',
                                    zIndex: -1
                                  });
                                  chart.redraw(false);
                                });
                              }
                            ")
    })
    
    # Updates top 5 model plot highlight
    observeEvent(model_idx(), {
        session$sendCustomMessage(
            "hc-bar-select-model_selection_plot",
            list(index = model_idx(), bandColor = "rgba(0,191,166,0.12)")
        )
    }, ignoreInit = FALSE)
    
    # Updates the action button
    observeEvent(model_idx(), {
        ids <- c('mod_one','mod_two','mod_three','mod_four','mod_five')
        sel_id <- session$ns(ids[model_idx()])
        
        session$sendCustomMessage('segmented-set-active', list(id = sel_id))
    }, ignoreInit = FALSE)
    
    ### Regime Time Series Plot ----
    
    selected_model_id <- reactive({
        idx <- model_idx()
        validate(need(idx %in% 1:5, "Select a model"))
        plot_df$model_id[idx]
    })
    
    # Build plotBands from contiguous runs of state_hi (0/1)
    make_plotbands <- function(df_yearly) {
        # df_yearly: year (numeric), state_hi (0/1), ordered
        # Use Jan 1 for yearly stamps
        df_yearly <- df_yearly %>% mutate(date = as.Date(paste0(year, "-01-01")))
        ts <- datetime_to_timestamp(df_yearly$date) # ms
        
        runs <- rle(df_yearly$state_hi)
        ends <- cumsum(runs$lengths)
        starts <- c(1, head(ends, -1) + 1)
        
        # last band extends ~1 year
        to_ts <- purrr::map_dbl(seq_along(starts), function(i) {
            if (i < length(starts)) ts[starts[i + 1]] else ts[ends[i]] + 365*24*3600*1000
        })
        
        purrr::map2(seq_along(starts), runs$values, function(i, v) {
            list(
                from  = ts[starts[i]],
                to    = to_ts[i],
                color = if (v == 1)
                    "rgba(83, 212, 74, 0.4)"  # High regime
                else
                    "rgba(231, 76, 60, 0.5)",  # Low regime
                label = list(
                    text  = if (v == 1) "State 1" else "State 2",
                    style = list(fontSize = "10px", color = "#555")
                ),
                zIndex = -1
            )
        })
    }
    
    df_model <- eventReactive(selected_model_id(), {
        mid <- selected_model_id()
        df <- posterior_all_df %>%
            filter(model_id == mid) %>%
            arrange(year)
        
        validate(need(nrow(df) > 0, "No rows for selected model."))
        df
    }, ignoreInit = FALSE)
    
    ### Time Series Plot with Regime Bands ----
    output$cs_regime_chart <- renderHighchart({
        df <- df_model()
        bands <- make_plotbands(df)
        
        series_df <- df %>%
            transmute(
                x = datetime_to_timestamp(as.Date(paste0(year, "-01-01"))),
                y = cs
            )
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(zoomType = "x") %>%
            hc_title(text = "Predicted Consumer Sentiment Regimes",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_subtitle(text = paste0("Threshold: P(High) ≥ 0.5")) %>%
            hc_xAxis(type = "datetime", title = list(text = "Year"), plotBands = bands) %>%
            hc_yAxis(title = list(text = "Consumer Sentiment")) %>%
            hc_add_series(
                data = list_parse2(series_df),
                name = "Consumer Sentiment",
                type = "line",
                lineWidth = 3,
                color = "black",
                marker = list(enabled = FALSE)
            ) %>%
            hc_tooltip(valueDecimals = 1, pointFormat = "<b>{point.y}</b>") %>%
            hc_legend(enabled = FALSE) %>%
            hc_exporting(enabled = TRUE)
    })
    
    norm_id <- function(x) {
        x <- iconv(x, "", "UTF-8")
        x <- gsub("[\u200B-\u200D\uFEFF]", "", x)
        x <- trimws(x)
        x <- gsub("[^A-Za-z0-9_]", "_", x)
        x <- gsub("_+", "_", x)
        x <- gsub("^_|_$", "", x)
        tolower(x)
    }
    
    ### Average TPM Heatmap ----
    output$tpm_avg <- renderHighchart({
        b <- current()
        P <- b$P_avg
        K <- nrow(P)
        
        y_cats <- as.vector(outer(paste0("S",1:K), paste0("S",1:K), function(a,b) paste0(a,"to",b)))
        hm_df <- data.frame(
            x = rep(0:(K-1), times = K),
            y = rep(0:(K-1), each  = K),
            value = as.vector(P),
            pair = y_cats
        )
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_add_dependency("modules/heatmap") %>%
            hc_chart(type = "heatmap") %>%
            hc_title(text = "Average Transition Matrix",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_xAxis(categories = paste0("S",1:K), title = list(text = "To")) %>%
            hc_yAxis(categories = paste0("S",1:K), title = list(text = "From"), reversed = TRUE) %>%
            hc_colorAxis(min = 0, max = 1, minColor = "#d9ffd9", maxColor = "#30c953") %>%
            hc_add_series(data = list_parse2(hm_df[,c("x","y","value")]), keys=c("x","y","value")) %>%
            hc_legend(
                layout        = "horizontal",
                align         = "center",
                verticalAlign = "bottom",
                floating      = FALSE,
                alignColumns  = FALSE,
                x             = 0,
                y             = 0
            ) %>%
            hc_tooltip(
                useHTML = TRUE,
                formatter = JS("
                    function () {
                      const from = this.series.yAxis.categories[this.point.y];
                      const to   = this.series.xAxis.categories[this.point.x];
                      const val  = Highcharts.numberFormat(this.point.value, 2);
                      return 'Average ' + from + 'to' + to + ' Probability: <b>' + val + '</b>';
                    }
                  ")
            )
    })
    
    ### Transition Heatmap Based on Current State ----
    output$tpm_time <- renderHighchart({
        b <- current()
        xi <- b$P_time
        K <- dim(xi)[2]
        Tm1 <- dim(xi)[1]
        years_eff <- b$years_P
        
        #labs   <- state_labels(b)
        #y_cats <- as.vector(outer(labs, labs, function(a,b) paste0(a,"to",b)))
        y_cats <- as.vector(outer(paste0("S",1:K), paste0("S",1:K), function(a,b) paste0(a,"to",b)))
        
        # rebuild long df (masked): active from-state at t is viterbi[t]
        df <- lapply(seq_len(Tm1), function(t){
            expand.grid(i=1:K, j=1:K) %>%
                mutate(x = t-1L,
                       pair = paste0("S",i,"toS",j),
                       value = as.vector(xi[t,,]),
                       from_idx = i)
        }) %>% bind_rows() %>%
            mutate(y = match(pair, y_cats)-1L,
                   value = ifelse(from_idx == b$viterbi[x+1L], value, NA_real_)) %>%
            filter(is.finite(value)) %>%
            dplyr::select(x,y,value)
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_add_dependency("modules/heatmap") %>%
            hc_chart(type = "heatmap") %>%
            hc_title(text = "Transition Probabilities Over Time",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            #hc_subtitle(text = "Only the Viterbi from-state row is shown per column") %>%
            hc_xAxis(categories = b$years_P, title = list(text = "Year")) %>%
            hc_yAxis(categories = y_cats, title = list(text = "From to To"), reversed = TRUE) %>%
            hc_colorAxis(min = 0, max = 1, minColor = "#d9ffd9", maxColor = "#30c953") %>%
            hc_add_series(data = list_parse2(df), keys=c("x","y","value"), turboThreshold=0) %>%
            hc_legend(
                layout        = "horizontal",
                align         = "center",
                verticalAlign = "bottom",
                floating      = FALSE,
                alignColumns  = FALSE,
                x             = 0,
                y             = 0
            ) %>%
            hc_tooltip(
                useHTML = TRUE,
                formatter = JS("
                            function () {
                              const pair = this.series.yAxis.categories[this.point.y];   // e.g., 'HightoLow'
                              const val  = Highcharts.numberFormat(this.point.value, 2);
                              return pair + ': <b>' + val + '</b>';
                            }
                  ")
            )
    })
    
    ### Emissions Bar Chart ----
    output$state_means_scaled <- renderHighchart({
        b   <- current()
        dbs <- dplyr::arrange(scaled_data, Year)
        
        vars <- norm_id(b$vars)
        lags <- as.integer(b$lags)
        stopifnot(length(vars) == length(lags))
        
        X <- sapply(seq_along(vars), function(i) {
            v <- dbs[[ vars[i] ]]
            L <- as.integer(lags[i])
            if (L > 0L) v <- c(rep(NA_real_, L), v)[1:length(v)]
            v
        })
        X <- as.matrix(X)
        colnames(X) <- paste0(vars, "_L", lags)   # internal lagged names
        
        idx <- match(b$posterior$year, dbs$Year)
        X   <- X[idx, , drop = FALSE]

        P1 <- b$posterior$S1
        P2 <- b$posterior$S2
        P_high <- if (b$hi_state == 1L) P1 else P2
        P_low  <- if (b$hi_state == 1L) P2 else P1
        
        wmean <- function(x, w) {
            ok <- is.finite(x) & is.finite(w)
            w <- w[ok]
            x <- x[ok]; if (!length(x)) return(NA_real_)
            sum(w * x) / sum(w)
        }
        wsd <- function(x, w, mu) {
            ok <- is.finite(x) & is.finite(w)
            w <- w[ok]
            x <- x[ok]
            if (!length(x)) return(NA_real_)
            sqrt(sum(w * (x - mu)^2) / sum(w))
        }
        
        mu_H <- apply(X, 2, function(col) wmean(col, P_high))
        mu_L <- apply(X, 2, function(col) wmean(col, P_low))
        sd_H <- mapply(function(col, m) wsd(col, P_high, m), as.data.frame(X), mu_H)
        sd_L <- mapply(function(col, m) wsd(col, P_low,  m), as.data.frame(X), mu_L)
        
        base_names   <- vars
        pretty_final <- unname(pretty_map[base_names])
        miss <- is.na(pretty_final)
        if (any(miss)) pretty_final[miss] <- base_names[miss]  # fallback
        
        cats <- as.character(pretty_final)  # categories in model order

        err_high <- Map(function(m, s) c(m - s, m + s), as.numeric(mu_H), as.numeric(sd_H))
        err_low  <- Map(function(m, s) c(m - s, m + s), as.numeric(mu_L), as.numeric(sd_L))
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_add_dependency("highcharts-more") %>%
            hc_chart(type = "column") %>%
            hc_title(text = "Indicator Means and Standard Deviation (scaled)",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_subtitle(text = "Posterior-weighted by state") %>%
            hc_xAxis(categories = cats, type = "category", tickPositions = 0:(length(cats) - 1)) %>%
            hc_yAxis(title = list(text = "Scaled units (z)"),
                     plotLines = list(list(value = 0, color = "#666", width = 1))) %>%
            hc_plotOptions(column = list(pointPadding = 0.1, groupPadding = 0.08)) %>%
            hc_legend(
                layout        = "horizontal",
                align         = "center",
                verticalAlign = "bottom",
                floating      = FALSE,
                alignColumns  = FALSE,
                x             = 0,
                y             = 0
            ) %>%
            
            # Means
            hc_add_series(name = "High", data = round(as.numeric(mu_H), 3), color = "#28a745", zIndex = 2) %>%
            hc_add_series(name = "Low",  data = round(as.numeric(mu_L),  3), color = "#e74c3c", zIndex = 2) %>%
            
            # Error bars (±1 SD)
            hc_add_series(type = "errorbar", name = "High ±1 SD", data = err_high,
                          linkedTo = ":previous", color = "#1b5e20", whiskerLength = "30%", zIndex = 1,
                          tooltip = list(pointFormat = "High ±1 SD: <b>{point.low:.3f}</b> – <b>{point.high:.3f}</b>")) %>%
            hc_add_series(type = "errorbar", name = "Low ±1 SD",  data = err_low,
                          color = "#7f1d1d", whiskerLength = "30%", zIndex = 1,
                          tooltip = list(pointFormat = "<br>Low ±1 SD: <b>{point.low:.3f}</b> – <b>{point.high:.3f}</b>")) %>%
            hc_tooltip(shared = TRUE, valueDecimals = 3)
    })
    
    ### Emissions Correlation Matrix ----
    
    build_X_all <- function(df, vars, lags) {
        stopifnot(length(vars) == length(lags))
        n <- nrow(df)
        X <- Map(function(v, L) {
            x <- df[[v]]
            if (L > 0L) x <- c(rep(NA_real_, L), x)[1:n]
            x
        }, vars, lags) |> as.data.frame()
        colnames(X) <- paste0(vars, "_L", lags)
        X
    }
    
    lagged_X_for_model <- function(dat_base, b) {
        vars <- norm_id(b$vars)
        lags <- as.integer(b$lags)
        
        X <- sapply(seq_along(vars), function(i) {
            v <- dat_base[[ vars[i] ]]
            L <- lags[i]
            if (L > 0L) v <- c(rep(NA_real_, L), head(v, -L))
            v
        })
        X <- as.matrix(X)
        colnames(X) <- paste0(vars, "_L", lags)
        
        idx  <- match(b$posterior$year, dat_base$Year)
        keep <- !is.na(idx)
        
        as.matrix(X[idx[keep], , drop = FALSE])
    }
    
    output$state_prob_heatmap <- renderHighchart({
        b  <- current()
        db <- dplyr::arrange(full_dataset, Year)
        
        X <- lagged_X_for_model(db, b)
        orig_cols <- colnames(X)
        p <- ncol(X)
        
        yrs <- db$Year[match(b$posterior$year, db$Year)]
        X_det <- matrix(NA_real_, nrow = nrow(X), ncol = p)
        for (j in seq_len(p)) {
            v <- X[, j]
            rr <- try(resid(lm(v ~ yrs)), silent = TRUE)
            X_det[, j] <- if (inherits(rr, "try-error") || anyNA(rr)) v else rr
        }
        colnames(X_det) <- orig_cols
        X <- X_det
        
        P1 <- b$posterior$S1; P2 <- b$posterior$S2
        P_high <- if (b$hi_state == 1L) P1 else P2
        P_low  <- if (b$hi_state == 1L) P2 else P1

        safe_cor <- function(x, y) suppressWarnings(cor(x, y, use = "pair", method = "spearman"))
        c_high <- vapply(seq_len(p), function(j) safe_cor(X[, j], P_high), numeric(1))
        c_low  <- vapply(seq_len(p), function(j) safe_cor(X[, j], P_low ), numeric(1))
        names(c_high) <- orig_cols
        names(c_low)  <- orig_cols

        M <- rbind(c_high, c_low)
        rownames(M) <- c("State 1", "State 2")
        colnames(M) <- orig_cols  # <- ensure names exist
        
        orig_cols  <- colnames(M)
        base_names <- sub("_L[0-9]+$", "", orig_cols)
        
        pretty_final <- unname(pretty_map[base_names])
        
        missing <- is.na(pretty_final)
        if (any(missing)) pretty_final[missing] <- base_names[missing]
        
        colnames(M) <- pretty_final
        vars <- as.character(pretty_final)

        rows <- rownames(M)
        p    <- ncol(M)
        
        df_plot <- data.frame(
            x     = rep(0:(p-1), each = 2L), 
            y     = rep(c(0L, 1L), times = p),
            value = as.vector(t(M)),
            stringsAsFactors = FALSE
    )
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_add_dependency("modules/heatmap") %>%
            hc_chart(type = "heatmap") %>%
            hc_title(text = "Correlation to Indicators per State",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_subtitle(text = "Spearman correlation; detrended by Year") %>%
            hc_xAxis(categories = vars, title = list(text = NULL)) %>%
            hc_legend(
                layout        = "horizontal",
                align         = "center",
                verticalAlign = "bottom",
                floating      = FALSE,
                alignColumns  = FALSE,
                x             = 0,
                y             = 0
            ) %>%
            hc_yAxis(
                categories = c("State 1", "State 2"),
                type = "category",
                tickPositions = 0:(length(rows)-1),
                reversed = TRUE,
                title = list(text = NULL)
            ) %>%
            hc_colorAxis(min = -1, max = 1,
                         stops = list(list(0, "#b2182b"), list(0.5, "#f7f7f7"), list(1, "#30c953"))) %>%
            hc_add_series(
                data = highcharter::list_parse2(df_plot),
                keys = c("x","y","value"),
                borderWidth = 0.5,
                tooltip = list(
                    pointFormat = "{point.series.yAxis.categories[this.point.y]} × {point.series.xAxis.categories[this.point.x]}: <b>{point.value:.2f}</b>"
                )
            )
    })
    
    
    current <- reactive({ bundle[[ selected_model_id() ]] })
    
    ### Emissions Density Chart ----
    output$emis_dens <- renderHighchart({
        b  <- current()                 
        ep <- b$emis                    

        labs <- if (b$hi_state == 1L) c("High","Low") else c("Low","High")
        
        x_min <- min(ep$mu - 4*ep$sd, na.rm = TRUE)
        x_max <- max(ep$mu + 4*ep$sd, na.rm = TRUE)

        pad   <- 0.03 * (x_max - x_min)
        x_min <- x_min - pad; x_max <- x_max + pad
        x_grid <- seq(x_min, x_max, length.out = 300)
        
        # Build density series per state (keep S1 row as series 1, S2 as series 2; rename via labs)
        dens_list <- lapply(seq_len(nrow(ep)), function(i){
            data.frame(x = x_grid, y = dnorm(x_grid, mean = ep$mu[i], sd = ep$sd[i]))
        })

        is_high1 <- (b$hi_state == 1L)
        
        lab1 <- if (is_high1) "High" else "Low"
        lab2 <- if (is_high1) "Low"  else "High"
        col1 <- if (is_high1) "#28a745" else "#e74c3c"
        col2 <- if (is_high1) "#e74c3c" else "#28a745"
        
        peak_lines <- list(
            list(
                value = ep$mu[1], color = col1, width = 2, dashStyle = "ShortDash", zIndex = 5,
                label = list(
                    text = sprintf("%s peak: μ=%.2f", lab1, ep$mu[1]),
                    rotation = 0, align = "left", y = -5, x = 5,
                    style = list(color = col1, fontWeight = "bold")
                )
            ),
            list(
                value = ep$mu[2], color = col2, width = 2, dashStyle = "ShortDash", zIndex = 5,
                label = list(
                    text = sprintf("%s peak: μ=%.2f", lab2, ep$mu[2]),
                    rotation = 0, align = "left", y = -5, x = 5,
                    style = list(color = col2, fontWeight = "bold")
                )
            )
        )

        hc <- highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(type = "areaspline") %>%
            hc_title(text = "Emission Densities by State",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_subtitle(text = "Theoretical Densities Assuming Normal Distribution <br>ㅤ") %>%
            hc_xAxis(title = list(text = "Average Observed Index"),
                     min = x_min, max = x_max, plotLines = peak_lines) %>%
            hc_yAxis(title = list(text = "Density")) %>%
            hc_legend(
                layout        = "horizontal",
                align         = "center",
                verticalAlign = "bottom",
                floating      = FALSE,
                alignColumns  = FALSE,
                x             = 0,
                y             = 0
            ) %>%
            hc_plotOptions(series = list(marker = list(enabled = FALSE), 
                                         animation = TRUE))
        
        if (is_high1) {
            hc <- hc %>%
                hc_add_series(name = "High", type = "areaspline",
                              data = list_parse2(dens_list[[1]][, c("x","y")]), keys = c("x","y"),
                              color = "#28a745", tooltip = list(valueDecimals = 3)) %>%
                hc_add_series(name = "Low",  type = "areaspline",
                              data = list_parse2(dens_list[[2]][, c("x","y")]), keys = c("x","y"),
                              color = "#e74c3c", tooltip = list(valueDecimals = 3))
        } else {
            hc <- hc %>%
                hc_add_series(name = "High", type = "areaspline",
                              data = list_parse2(dens_list[[2]][, c("x","y")]), keys = c("x","y"),
                              color = "#28a745", tooltip = list(valueDecimals = 3)) %>%
                hc_add_series(name = "Low",  type = "areaspline",
                              data = list_parse2(dens_list[[1]][, c("x","y")]), keys = c("x","y"),
                              color = "#e74c3c", tooltip = list(valueDecimals = 3))
        }
        
        hc
    })
    
    ### Buttons to Switch Between Transition and Emissions Plots ----
    observeEvent(input$emiss_btn, {
        shinyjs::hide("transition_probs")
        shinyjs::show("emission_probs")

        session$sendCustomMessage(
            "segmented-set-active",
            list(id = session$ns("emiss_btn"))
        )
    })
    
    observeEvent(input$trans_btn, {
        shinyjs::hide("emission_probs")
        shinyjs::show("transition_probs")
        
        session$sendCustomMessage(
            "segmented-set-active",
            list(id = session$ns("trans_btn"))
        )
    })
    
    ## Conclusion Code ----
    
    ### Time series Conclusion ----
    
    output$consumer_sent_monthly <- renderHighchart({
        b <- bundle[["M4"]]
        
        yr_map <- tibble(
            year  = as.integer(b$years),
            state = as.integer(b$viterbi)
        ) %>%
            mutate(regime = ifelse(state == b$hi_state, "High", "Low")) %>%
            dplyr::select(year, regime)
        
        dfm <- cons_sent_monthly %>%
            transmute(
                date      = as.Date(observation_date),
                year      = as.integer(format(observation_date, "%Y")),
                sentiment = as.numeric(UMCSENT)
            ) %>%
            left_join(yr_map, by = "year") %>%
            mutate(regime = ifelse(is.na(regime), "Low", regime)) %>%
            arrange(date)
        
        
        runs   <- rle(dfm$regime)
        ends   <- cumsum(runs$lengths)
        starts <- c(1, head(ends, -1) + 1)
        col_reg <- c(High = "rgba(40,167,69,0.18)", Low = "rgba(231,76,60,0.18)")
        state_bands <- Map(function(i, j, lab) {
            list(
                from  = datetime_to_timestamp(dfm$date[i]),
                to    = datetime_to_timestamp(dfm$date[j]) + 24*3600*1000 - 1,
                color = col_reg[[lab]],
                zIndex = 1
            )
        }, starts, ends, runs$values)
        
        line_pts <- dfm %>%
            transmute(
                x = datetime_to_timestamp(date),
                y = sentiment,
                regime = regime
            ) %>%
            purrr::pmap(function(x, y, regime) list(x = x, y = y, regime = regime))
        
        zone_pieces <- unlist(lapply(seq_len(nrow(recessions)), function(k){
            s <- datetime_to_timestamp(as.Date(recessions$start[k]))
            e <- datetime_to_timestamp(as.Date(recessions$end[k]))
            c(
                sprintf("{value:%s,color:'#111'}", s),
                sprintf("{value:%s,color:'#d62728'}", e)
            )
        }), use.names = FALSE)
        zones_js <- paste0("[", paste(zone_pieces, collapse = ","), "]")
        
        js_quote <- function(s) paste0("'", gsub("(['\\\\])", "\\\\\\1", s), "'")
        macro_items <- vapply(seq_len(nrow(recessions)), function(k){
            from_ts <- datetime_to_timestamp(as.Date(recessions$start[k]))
            to_ts   <- datetime_to_timestamp(as.Date(recessions$end[k]))
            nm      <- js_quote(recessions$name[k])
            sprintf("{from:%s,to:%s,name:%s}", from_ts, to_ts, nm)
        }, character(1))
        bands_js <- paste0("[", paste(macro_items, collapse = ","), "]")
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(zoomType = "x") %>%
            hc_title(text = ("<b> Consumer Sentiment with Regime Bands </b>"),
                     style = list(
                         fontSize = "28px")) %>%
            hc_xAxis(type = "datetime",
                     gridLineWidth = 0,
                     crosshair = list(
                         color = "darkgrey",
                         width = 1,
                         dashStyle = "Solid"
                     ),
                     plotBands = state_bands) %>%
            hc_yAxis(
                title = list(text = "Index Value"),
                gridLineWidth = 0,
                crosshair = list(
                    color = "darkgrey",
                    width = 1,
                    dashStyle = "Solid"
                ),
                plotLines = list(list(
                    value = 100, color = "#888", width = 2, dashStyle = "Dash",
                    label = list(text = "<b> Index Baseline: 1966 = 100 </b>", style = list(color = "red", fontSize = "16px"))
                ))
            ) %>%
            hc_add_series(
                data = line_pts,
                type = "line",
                name = "Sentiment",
                color = "#111",
                lineWidth = 3,
                marker = list(enabled = FALSE),
                zIndex = 5,
                showInLegend = FALSE,
                zoneAxis = "x",
                zones = JS(zones_js)
            ) %>%
            hc_tooltip(
                useHTML = TRUE,
                formatter = JS(sprintf("
                                        function () {
                                          var x=this.x, y=this.y, rg=(this.point&&this.point.regime)?this.point.regime:'-';
                                          var rgColor = (rg==='High') ? '#28a745' : '#e74c3c';
                                          var tag='', bands=%s;
                                          for (var i=0;i<bands.length;i++){
                                            if(x>=bands[i].from && x<=bands[i].to){ tag='<br><span style=\"color:#d62728\"><b>'+bands[i].name+'</b></span>'; break; }
                                          }
                                          return Highcharts.dateFormat('%%b %%e, %%Y', x) +
                                                 '<br>Index: <b>'+Highcharts.numberFormat(y,1)+'</b>' +
                                                 '<br>State: <b><span style=\"color:'+rgColor+'\">'+rg+'</span></b>' + tag;
                                        }
      ", bands_js))
            ) %>%
            hc_legend(enabled = FALSE) %>%
            hc_credits(enabled = FALSE)
    })
    
    
    # Helper function to create lagged covariates
    build_X_all <- function(df, vars, lags) {
        stopifnot(length(vars) == length(lags))
        n <- nrow(df)
        X <- Map(function(v, L) {
            x <- df[[v]]
            if (L > 0L) x <- c(rep(NA_real_, L), x)[1:n]
            x
        }, vars, lags) |> as.data.frame()
        colnames(X) <- paste0(vars, "_L", lags)
        X
    }
    
    ### 3D TSNE PLOT ----
    output$state_plot <- renderHighchart({
        
        tsne_df   <- tsne_data$tsne
        centers   <- tsne_data$centers
        nm_high   <- sprintf("State %d", tsne_data$meta$hi_state)
        nm_low    <- sprintf("State %d", 3 - tsne_data$meta$hi_state)
        
        df_low_cent  <- subset(centers, state == "Low")[, c("cx","cy","cz")]
        names(df_low_cent)  <- c("x","y","z")
        df_high_cent <- subset(centers, state == "High")[, c("cx","cy","cz")]
        names(df_high_cent) <- c("x","y","z")
        
        
        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_add_dependency("highcharts-3d") %>%
            hc_chart(type = "scatter3d",
                     options3d = list(enabled = TRUE, 
                                      alpha = 10, 
                                      beta = 30, 
                                      depth = 250, 
                                      viewDistance = 40),
                     
                     events = list(load = JS("
                                            function () {
                                              var chart = this, H = Highcharts;
                                              var startX, startY, alpha0, beta0, dragging = false, sens = 5;
                                        
                                              function onMove(e){
                                                if (!dragging) return;
                                                e = chart.pointer.normalize(e);
                                                var a = alpha0 + (e.chartY - startY) / sens;
                                                var b = beta0  + (startX - e.chartX) / sens;
                                                chart.update({ chart: { options3d: { alpha: a, beta: b } } }, true, false, false);
                                              }
                                        
                                              function endDrag(){
                                                if (!dragging) return;
                                                dragging = false;
                                                H.removeEvent(document, 'mousemove', onMove);
                                                H.removeEvent(document, 'touchmove', onMove);
                                              }
                                        
                                              H.addEvent(chart.container, 'mousedown', function (e) {
                                                e = chart.pointer.normalize(e);
                                                startX = e.chartX; startY = e.chartY;
                                                alpha0 = chart.options.chart.options3d.alpha;
                                                beta0  = chart.options.chart.options3d.beta;
                                                dragging = true;
                                                H.addEvent(document, 'mousemove', onMove);
                                                H.addEvent(document, 'touchmove', onMove);
                                                H.addEvent(document, 'mouseup',   endDrag);
                                                H.addEvent(document, 'touchend',  endDrag);
                                              });
                                        
                                              H.addEvent(chart.container, 'touchstart', function (e) {
                                                e = chart.pointer.normalize(e.touches[0] || e.changedTouches[0]);
                                                startX = e.chartX; startY = e.chartY;
                                                alpha0 = chart.options.chart.options3d.alpha;
                                                beta0  = chart.options.chart.options3d.beta;
                                                dragging = true;
                                                H.addEvent(document, 'mousemove', onMove);
                                                H.addEvent(document, 'touchmove', onMove);
                                                H.addEvent(document, 'mouseup',   endDrag);
                                                H.addEvent(document, 'touchend',  endDrag);
                                              });
                                            }
                                        "))) %>%
            hc_title(text = "t-SNE of Best Model", style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_subtitle(text = "Colored by HMM State") %>%
            hc_plotOptions(scatter = list(marker = list(radius = 5))) %>%
            
            # Low / High series
            hc_add_series(tsne_df |> dplyr::filter(group == "Low"),
                          type = "scatter3d", name = nm_low,  color = "#e74c3c",
                          hcaes(x = x, y = y, z = z)) %>%
            hc_add_series(tsne_df |> dplyr::filter(group == "High"),
                          type = "scatter3d", name = nm_high, color = "#28a745",
                          hcaes(x = x, y = y, z = z)) %>%
            hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom") %>%
            
            # halos
            hc_add_series(type = "scatter3d", showInLegend = FALSE,
                          data = highcharter::list_parse2(df_low_cent),  keys = c("x","y","z"),
                          name = paste(nm_low, "Cluster"), 
                          color = "rgba(231,76,60,0.16)",  marker = list(symbol = "circle", radius = 26),
                          tooltip = list(pointFormat = "<b>{series.name}</b><br>x: {point.x:.3f}<br>y: {point.y:.3f}<br>z: {point.z:.3f}")) %>%
            
            hc_add_series(type = "scatter3d", showInLegend = FALSE,
                          data = highcharter::list_parse2(df_low_cent),  keys = c("x","y","z"),
                          name = paste(nm_low, "Cluster"),
                          color = "rgba(231,76,60,0.10)",  marker = list(symbol = "circle", radius = 76),
                          tooltip = list(pointFormat = "<b>{series.name}</b><br>x: {point.x:.3f}<br>y: {point.y:.3f}<br>z: {point.z:.3f}")) %>%
            
            
            hc_add_series(type = "scatter3d", showInLegend = FALSE,
                          data = highcharter::list_parse2(df_high_cent), keys = c("x","y","z"),
                          name = paste(nm_high, "Cluster"),
                          color = "rgba(40,167,69,0.16)", marker = list(symbol = "circle", radius = 26),
                          tooltip = list(pointFormat = "<b>{series.name}</b><br>x: {point.x:.3f}<br>y: {point.y:.3f}<br>z: {point.z:.3f}")) %>%
            
            hc_add_series(type = "scatter3d", showInLegend = FALSE,
                          data = highcharter::list_parse2(df_high_cent), keys = c("x","y","z"),
                          name = paste(nm_high, "Cluster"),
                          color = "rgba(40,167,69,0.10)", marker = list(symbol = "circle", radius = 76),
                          tooltip = list(pointFormat = "<b>{series.name}</b><br>x: {point.x:.3f}<br>y: {point.y:.3f}<br>z: {point.z:.3f}"))
    })
    
    ### Means Bar Chart for Best Model ----
    
    output$state_means_real_M4 <- renderHighchart({
        b <- bundle[["M4"]]

        dbs <- dplyr::arrange(scaled_data, Year)
        dbr <- dplyr::arrange(full_dataset, Year)
        
        vars <- norm_id(b$vars)
        lags <- as.integer(b$lags)
        stopifnot(length(vars) == length(lags))
        
        mk_lag <- function(df) {
            M <- sapply(seq_along(vars), function(i){
                v <- df[[vars[i]]]
                L <- as.integer(lags[i])
                if (L > 0L) v <- c(rep(NA_real_, L), v)[1:length(v)]
                v
            })
            colnames(M) <- paste0(vars, "_L", lags)
            as.matrix(M)
        }
        
        Xz <- mk_lag(dbs)
        Xr <- mk_lag(dbr)
        
        idx <- match(b$posterior$year, dbs$Year)
        Xz  <- Xz[idx, , drop = FALSE]
        Xr  <- Xr[idx, , drop = FALSE]
        
        P1 <- b$posterior$S1; P2 <- b$posterior$S2
        P_high <- if (b$hi_state == 1L) P1 else P2
        P_low  <- if (b$hi_state == 1L) P2 else P1
        
        wmean <- function(x, w){ ok <- is.finite(x)&is.finite(w); if(!any(ok)) return(NA_real_); sum(w[ok]*x[ok])/sum(w[ok]) }
        wsd   <- function(x, w, m){ ok <- is.finite(x)&is.finite(w); if(!any(ok)) return(NA_real_); sqrt(sum(w[ok]*(x[ok]-m)^2)/sum(w[ok])) }
        
        mu_H <- apply(Xz, 2, function(col) wmean(col, P_high))
        mu_L <- apply(Xz, 2, function(col) wmean(col, P_low))
        sd_H <- mapply(function(col, m) wsd(col, P_high, m), as.data.frame(Xz), mu_H)
        sd_L <- mapply(function(col, m) wsd(col, P_low,  m), as.data.frame(Xz), mu_L)
        
        r_H  <- apply(Xr, 2, function(col) wmean(col, P_high))
        r_L  <- apply(Xr, 2, function(col) wmean(col, P_low))
        
        r_sd_H <- mapply(function(col, m) wsd(col, P_high, m), as.data.frame(Xr), r_H)
        r_sd_L <- mapply(function(col, m) wsd(col, P_low,  m), as.data.frame(Xr), r_L)
        
        base_names   <- vars
        pretty_final <- unname(pretty_map[base_names])
        miss <- is.na(pretty_final)
        if (any(miss)) pretty_final[miss] <- base_names[miss]
        cats <- as.character(pretty_final)
        
        hi_pts <- lapply(seq_along(mu_H), function(i) list(
            y = as.numeric(mu_H[i]), real = as.numeric(r_H[i])
        ))
        lo_pts <- lapply(seq_along(mu_L), function(i) list(
            y = as.numeric(mu_L[i]), real = as.numeric(r_L[i])
        ))
        
        err_high <- Map(function(mz, sz, rsd) {
            list(
                low    = mz - sz,   # scaled low
                high   = mz + sz,   # scaled high
                realSD = rsd        # real ±1 SD (we'll use this in tooltip)
            )
        }, as.numeric(mu_H), as.numeric(sd_H), as.numeric(r_sd_H))
        
        err_low <- Map(function(mz, sz, rsd) {
            list(
                low    = mz - sz,
                high   = mz + sz,
                realSD = rsd
            )
        }, as.numeric(mu_L), as.numeric(sd_L), as.numeric(r_sd_L))

        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_add_dependency("highcharts-more") %>%
            hc_chart(type = "column") %>%
            hc_title(text = "Indicator Means and Standard Deviation for Each State",
                     style = list(fontWeight = "bold", fontSize = "16px")) %>%
            hc_subtitle(text = "Bars are Scaled for Visual Cohesiveness <br> (Z-Score: Mean of 0, SD of 1)") %>%
            hc_xAxis(categories = cats, type = "category", tickPositions = 0:(length(cats) - 1)) %>%
            hc_yAxis(title = list(text = NULL), labels = list(enabled = FALSE),
                     plotLines = list(list(value = 0, color = "#666", width = 1))) %>%
            hc_plotOptions(column = list(pointPadding = 0.1, groupPadding = 0.08)) %>%
            hc_add_series(name = "High", id = "s_high", data = hi_pts, color = "#28a745", zIndex = 2) %>%
            hc_add_series(name = "Low",  id = "s_low",  data = lo_pts, color = "#e74c3c", zIndex = 2) %>%
            hc_add_series(type = "errorbar", name = "High ±1 SD", data = err_high,
                          linkedTo = "s_high", color = "#1b5e20", whiskerLength = "30%", zIndex = 1) %>%
            hc_add_series(type = "errorbar", name = "Low ±1 SD",  data = err_low,
                          linkedTo = "s_low",  color = "#7f1d1d",  whiskerLength = "30%", zIndex = 1) %>%
            hc_tooltip(
                shared = TRUE, useHTML = TRUE,
                formatter = JS("
                                function () {
                                  var cat = this.x;
                            
                                  function fmtDollarFront(v){
                                    return '$' + Highcharts.numberFormat(v, 2);
                                  }
                                  function fmtDollarNegAfter(v){
                                    var neg = v < 0;
                                    var s = Highcharts.numberFormat(Math.abs(v), 2);
                                    return (neg ? '-' : '') + '$' + s;   // -$1,234.56
                                  }
                                  function fmtPct(v){
                                    return Highcharts.numberFormat(v, 2) + '%';
                                  }
                            
                                  var s = '<b>' + cat + '</b><br/>';
                                    (this.points || [this.point]).forEach(function(pt){
                                      var isErrorbar = (pt.series.type === 'errorbar');
                                      var real;
                                    
                                      if (isErrorbar && pt.point && typeof pt.point.realSD !== 'undefined') {
                                        // use the *real* standard deviation for the errorbar series
                                        real = pt.point.realSD;
                                      } else if (pt.point && pt.point.real != null) {
                                        // means: use real-scale mean when available
                                        real = pt.point.real;
                                      } else {
                                        // fallback (scaled)
                                        real = pt.y;
                                      }
                                    
                                      var val;
                                      if (/Real\\s*GDP/i.test(cat)) {
                                        val = fmtDollarFront(real);
                                      } else if (/(Federal\\s*Surplus\\/?Deficit|FYFSD)/i.test(cat)) {
                                        val = fmtDollarNegAfter(real);
                                      } else if (/PCE\\s*Price|PCEPI/i.test(cat)) {
                                        val = fmtPct(real);
                                      } else {
                                        val = Highcharts.numberFormat(real, 2);
                                      }
                                      s += pt.series.name + ': <b>' + val + '</b><br/>';
                                  });
                                  
                                  
                                  return s;
                                }
                        ")
            )
    })
    
    
    ### Best Model Tested Post 2007 ----
    
    b_full  <- bundle[["M4"]]
    b_short <- b_frozen_m4
    
    start_oos <- as.Date("2007-01-01")
    end_oos   <- as.Date("2024-12-31")
    
    cs <- cons_sent_monthly |>
        dplyr::transmute(
            date = as.Date(observation_date),
            year = as.integer(format(as.Date(observation_date), "%Y")),
            y    = suppressWarnings(as.numeric(UMCSENT))
        ) |>
        dplyr::filter(date >= start_oos, date <= end_oos, is.finite(y)) |>
        dplyr::arrange(date)
    
    yr_map <- tibble::tibble(
        year  = as.integer(b_short$years),
        state = as.integer(b_short$viterbi)
    ) |>
        dplyr::mutate(regime = ifelse(state == b_short$hi_state, "High", "Low")) |>
        dplyr::select(year, regime)
    
    dfm <- dplyr::left_join(
        cs[, c("date","year","y")],
        yr_map, by = "year"
    ) |>
        dplyr::mutate(regime = ifelse(is.na(regime), "Low", regime))
    
    r   <- rle(dfm$regime)
    end <- cumsum(r$lengths)
    beg <- c(1, head(end, -1) + 1)
    
    bands_short <- Map(function(i,j,lab){
        list(
            from   = datetime_to_timestamp(dfm$date[i]),
            to     = datetime_to_timestamp(dfm$date[j]) + 24*3600*1000 - 1,
            color  = if (lab == "High") "rgba(40,167,69,0.18)" else "rgba(231,76,60,0.18)",
            zIndex = 1,
            regime = lab
        )
    }, beg, end, r$values)
    
    # JS array from regime bands for tooltip state inference
    bands_short_js <- paste0(
        "[",
        paste(vapply(bands_short, function(b)
            sprintf("{from:%s,to:%s,regime:'%s'}", b$from, b$to, b$regime),
            character(1)), collapse = ","),
        "]"
    )
    
    # Recessions -> red segments (zones) + JS array for tooltip labeling
    rec_oos <- recessions |>
        dplyr::mutate(
            from_ts = datetime_to_timestamp(as.Date(start)),
            to_ts   = datetime_to_timestamp(as.Date(end))
        ) |>
        dplyr::filter(as.Date(end) >= start_oos, as.Date(start) <= end_oos) |>
        dplyr::arrange(from_ts)
    
    zones <- list()
    if (nrow(rec_oos) == 0) {
        zones[[1]] <- list(color = "#000000")
    } else {
        for (i in seq_len(nrow(rec_oos))) {
            zones[[length(zones) + 1]] <- list(value = rec_oos$from_ts[i], color = "#000000")
            zones[[length(zones) + 1]] <- list(value = rec_oos$to_ts[i],   color = "#e74c3c")
        }
        zones[[length(zones) + 1]] <- list(color = "#000000")
    }
    
    rec_js <- paste0(
        "[",
        paste(sprintf("{from:%s,to:%s,name:'%s'}",
                      rec_oos$from_ts, rec_oos$to_ts, rec_oos$name),
              collapse = ","),
        "]"
    )
    
    series_line <- cs |>
        dplyr::transmute(x = datetime_to_timestamp(date), y = y) |>
        highcharter::list_parse2()
    
    ### Forecasting ----
    # Constants & Model Load
    TRAIN_END <- as.Date("2024-12-01")
    
    # Load the pre-trained model
    m4_fit <- readRDS("data/m4_monthly_fit.rds")
    
    # Helper: Identify High Sentiment State
    get_hi_state <- function(fit) {
        # Dynamically check parameters for all states
        mus <- vapply(seq_len(depmixS4::nstates(fit)), function(i) {
            # param 1 is usually the intercept/mean for gaussian response
            depmixS4::getpars(fit@response[[i]][[1]])[1] 
        }, numeric(1))
        which.max(mus)
    }
    
    HI_STATE <- get_hi_state(m4_fit)
    
    fred_wide <- reactive({
        
        fw <- readr::read_csv("https://raw.githubusercontent.com/warisp897/ConsumerSentimentHMM/refs/heads/main/data/fred_raw_long.csv", show_col_types = FALSE)
        
        if ("date" %in% names(fw) && !"Date" %in% names(fw)) fw <- dplyr::rename(fw, Date = date)
        
        fw <- fw %>% dplyr::mutate(Date = as.Date(Date))
        validate(need(sum(!is.na(fw$Date)) > 0, "Error: Dates parsed as all NA."))
        
        fw %>% dplyr::arrange(Date)
    })
    
    model_data <- reactive({
        fw <- fred_wide()
        
        # Identify Target Column
        y_col <- "cons_sent"
        validate(need(y_col %in% names(fw), "Error: Column 'cons_sent' not found in CSV."))
        
        # Select & Impute
        df_prep <- fw %>%
            dplyr::select(
                Date, 
                y = dplyr::all_of(y_col),
                gdp_real, 
                pcepi, 
                FYFSD
            ) %>%
            dplyr::arrange(Date) %>%
            # Carry forward quarterly data to months
            tidyr::fill(gdp_real, pcepi, FYFSD, .direction = "down") %>%
            
            # Filter to monthly frequency (only where we have Sentiment)
            dplyr::filter(!is.na(y)) %>%
            
            # Date Range
            dplyr::filter(Date >= as.Date("1987-01-01")) %>%
            
            # Create Lags
            dplyr::mutate(
                real_GDP_L1 = dplyr::lag(gdp_real, 1),
                FYFSD_L1    = dplyr::lag(FYFSD, 1),
                PCEPI_L0    = pcepi
            ) %>%
            tidyr::drop_na()
        
        validate(need(nrow(df_prep) > 10, "Error: Dataset is empty after processing."))
        df_prep
    })
    
    # Forecasting Logic
    forecast_results <- reactive({
        df <- model_data()
        req(nrow(df) > 10)
        
        cov_vars <- c("real_GDP_L1", "PCEPI_L0", "FYFSD_L1")
        
        # Z-Score Scaling
        train_mask <- df$Date <= TRAIN_END
        
        stats_list <- lapply(cov_vars, function(var) {
            list(
                mu = mean(df[[var]][train_mask], na.rm = TRUE),
                sd = sd(df[[var]][train_mask], na.rm = TRUE)
            )
        })
        names(stats_list) <- cov_vars
        
        for (var in cov_vars) {
            mu <- stats_list[[var]]$mu
            s  <- stats_list[[var]]$sd
            if (is.na(s) || s == 0) s <- 1 
            df[[var]] <- (df[[var]] - mu) / s
        }
        
        # Model Application
        
        n_states_loaded <- depmixS4::nstates(m4_fit)
        trans_formula <- as.formula(paste("~", paste(cov_vars, collapse = " + ")))
        
        # Initialize model
        mod_new_2 <- depmixS4::depmix(
            response = y ~ 1,
            data = df,
            nstates = n_states_loaded,
            family = gaussian(),
            transition = trans_formula
        )
        
        # Inject parameters
        mod_applied <- depmixS4::setpars(mod_new_2, depmixS4::getpars(m4_fit))
        
        # Calculate Posterior
        post_probs <- depmixS4::posterior(mod_applied, type = "smoothing")
        target_col_idx <- HI_STATE + 1
        
        # Validate structure to catch any odd failures
        validate(need(ncol(post_probs) >= target_col_idx, 
                      paste("Error: Posterior has", ncol(post_probs), 
                            "columns, but we need index", target_col_idx)))
        
        # Extract probability by position
        p_high_vals <- post_probs[, target_col_idx]
        
        # Bind result
        df %>%
            dplyr::mutate(
                p_high   = p_high_vals,
                # SWAP: Logic inverted so 0 becomes 1 and 1 becomes 0
                state_hi = 1 - as.integer(p_high >= 0.5),
                # Labels will now align correctly (High = High)
                regime   = ifelse(state_hi == 1, "High", "Low"),
                is_fcst  = Date > TRAIN_END
            )
    })
    
    #### Forecast Line Chart ----
    
    output$regime_forecast_monthly <- renderHighchart({
        dfp <- forecast_results()
        validate(need(nrow(dfp) > 0, "Error: No data available for plotting"))

        make_plotbands <- function(d) {
            runs <- rle(d$state_hi)
            ends <- cumsum(runs$lengths)
            starts <- c(1, head(ends, -1) + 1)
            ts <- datetime_to_timestamp(d$Date)
            step_ms <- 30 * 24 * 3600 * 1000

            purrr::map(seq_along(starts), function(i) {
                color <- if(runs$values[i] == 1) "rgba(40,167,69,0.18)" else "rgba(231,76,60,0.18)"
                to_t  <- if(i < length(starts)) ts[starts[i+1]] else (ts[ends[i]] + step_ms)
                list(from = ts[starts[i]], to = to_t, color = color, zIndex = 1)
            })
        }

        bands <- make_plotbands(dfp)
        fcst_ts <- datetime_to_timestamp(TRAIN_END)

        pts_train <- dfp %>%
            dplyr::filter(!is_fcst) %>%
            dplyr::transmute(x = datetime_to_timestamp(Date), y = y, regime = regime) %>%
            highcharter::list_parse()

        pts_fcst <- dfp %>%
            dplyr::filter(Date >= TRAIN_END) %>%
            dplyr::transmute(x = datetime_to_timestamp(Date), y = y, regime = regime) %>%
            highcharter::list_parse()

        highchart() %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_chart(zoomType = "x") %>%
            hc_title(text = "<b> Consumer Sentiment with Forecasted Regime Bands </b>",
                     style = list(fontSize = "26px")) %>%
            hc_subtitle(text = "Forecasting Starts January 2025 with a 4 Week Delay due to FRED<br> Drag to Zoom in") %>%
            hc_xAxis(
                type = "datetime",
                gridLineWidth = 0,
                plotBands = bands,
                plotLines = list(list(
                    value = fcst_ts, color = "rgba(120,120,120,0.8)", width = 2,
                    dashStyle = "ShortDash", zIndex = 6,
                    label = list(text = "<b>Forecast start</b>", style = list(color = "#787878", fontSize = "12px"))
                ))
            ) %>%
            hc_yAxis(
                title = list(text = "Consumer Sentiment Index"),
                gridLineWidth = 0,
                plotLines = list(list(
                    value = 100, color = "#888", width = 2, dashStyle = "Dash",
                    label = list(text = "<b> Index Baseline: 1966 = 100 </b>", style = list(color = "red", fontSize = "16px"))
                ))
            ) %>%
            hc_add_series(data = pts_train, type = "line", name = "History", color = "#111",
                          lineWidth = 3, marker = list(enabled = FALSE), showInLegend = FALSE, zIndex = 5) %>%
            hc_add_series(data = pts_fcst, type = "line", name = "Forecast", color = "#111",
                          lineWidth = 3, dashStyle = "Dash", marker = list(enabled = FALSE), showInLegend = FALSE, zIndex = 5) %>%
            hc_tooltip(useHTML = TRUE, formatter = JS("
                                                    function () {
                                                      var rg = (this.point.regime) ? this.point.regime : '-';
                                                      var rgColor = (rg === 'High') ? '#28a745' : '#e74c3c';
                                                      return Highcharts.dateFormat('%b %Y', this.x) +
                                                             '<br>CSI: <b>' + Highcharts.numberFormat(this.y, 1) + '</b>' +
                                                             '<br>Regime: <b><span style=\"color:' + rgColor + '\">' + rg + '</span></b>';
                                                    }")) %>%
            hc_legend(enabled = FALSE) %>%
            hc_credits(enabled = FALSE)
    })

    #### Status Cards ----
    output$status_box <- renderUI({
        df <- forecast_results()
        req(nrow(df) > 0)
        
        last_row <- tail(df, 1)
        
        prob <- max(last_row$p_high, 1 - last_row$p_high)
        state_label <- last_row$regime
        
        runs <- rle(df$state_hi)
        
        current_streak <- tail(runs$lengths, 1)
        prev_streak <- if(length(runs$lengths) > 1) tail(runs$lengths, 2)[1] else 0
        
        # Colors
        is_positive_state <- last_row$p_high > 0.5
        text_color <- if(is_positive_state) "#721c24" else "#155724"
        bg_color   <- if(is_positive_state) "#f8d7da" else "#d4edda"
        
        div(
            style = paste0("background-color:", bg_color, "; color:", text_color, ";
                padding: 15px; border-radius: 5px; border: 1px solid ", text_color, ";
                display: flex; justify-content: space-between; align-items: center;"),
            
            # Left Side: Regime & Probability
            div(
                h4(strong(state_label), style = "margin: 0;"),
                div(paste0("Prob. of ", state_label, " Regime: ", round(prob * 100, 1), "%"),
                    style = "font-size: 1.1em; margin-top: 4px;")
            ),
            
            # Divider Line
            div(style = paste0("border-left: 1px solid ", text_color, "; height: 50px; margin: 0 20px; opacity: 0.4;")),
            
            # Right Side: Duration Stats
            div(style = "text-align: right;",
                # Current
                h4(strong(paste(current_streak, "Months")), style = "margin: 0; line-height: 1;"),
                div("Current State", style = "font-size: 0.8em; opacity: 0.8; margin-bottom: 6px;"),
                
                # Previous
                div(strong(paste(prev_streak, "Months")), style = "font-size: 1.0em; opacity: 0.65; line-height: 1;"),
                div("Previous State", style = "font-size: 0.75em; opacity: 0.5;")
            )
        )
    })
    
    pretty_names <- list(
        "gdp_nominal"    = "Nominal GDP",
        "inflation_rate" = "Inflation Rate",
        "unemployment"   = "Unemployment Rate",
        "trade_balance"  = "Trade Balance",
        "T10Y2Y"         = "Yield Curve (10Y-2Y)",
        "UNRATE"         = "Unemployment Rate",
        "CPIAUCSL"       = "CPI Inflation",
        "VIXCLS"         = "VIX Volatility",
        "DTWEXBGS"       = "Dollar Index",
        "PCEPI"          = "PCE Inflation",
        "fed_receipts"   = "Federal Receipts",
        "case_schiller"  = "Case-Shiller Home Price",
        "fed_debt"       = "Federal Debt",
        "fed_outlays"    = "Federal Outlays",
        "pcepi_robust"   = "PCE Inflation (Robust)",
        "m2_val"         = "M2 Money Supply",
        "cpi"            = "CPI Inflation",
        "cons_sent"      = "Consumer Sentiment",
        "gdp_real"       = "Real GDP",
        "FYFSD"          = "Federal Surplus/Deficit",
        "ppi"            = "PPI (Producer Prices)",
        "exports"        = "Exports",
        "imports"        = "Imports",
        "acc_balance"    = "Current Account Bal.",
        "initial_claims" = "Initial Jobless Claims",
        "urate"          = "Unemployment Rate",
        "new_house_sales" = "New Home Sales",
        "part_rate"      = "Labor Force Part.",
        "short_treasury" = "Short-Term Yield",
        "ffr"            = "Fed Funds Rate",
        "new_private_houses" = "Housing Starts",
        "new_permits"    = "Building Permits",
        "long_treasury"  = "Long-Term Yield"
    )
    
    ### Data Table ----
    output$table_drivers <- DT::renderDT({
        fcst <- forecast_results()
        req(nrow(fcst) > 0)
        fw <- fred_wide()
        
        full_df <- fw %>%
            dplyr::inner_join(fcst %>% dplyr::select(Date, regime), by = "Date") %>%
            dplyr::arrange(Date) %>%
            tidyr::fill(where(is.numeric), .direction = "down") %>%
            dplyr::select(-Date)
        
        scaled_df <- full_df %>%
            dplyr::mutate(across(where(is.numeric), ~scale(.)[,1]))
        
        last_row <- tail(scaled_df, 1)
        current_regime <- last_row$regime
        
        regime_means <- scaled_df %>%
            dplyr::filter(regime == current_regime) %>%
            dplyr::summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
        
        indicators <- names(scaled_df)[sapply(scaled_df, is.numeric)]
        
        rows <- lapply(indicators, function(var_name) {
            if(var_name %in% c("p_high", "state_hi", "is_fcst", "day_num")) return(NULL)
            
            val      <- last_row[[var_name]]
            mean_val <- regime_means[[var_name]]
            dist     <- val - mean_val
            dist_abs <- abs(dist) # Calculate absolute difference
            is_divergent <- dist_abs > 1.0
            
            status_txt <- if(is_divergent) "Inconsistent" else "Consistent"
            
            # Inconsistent = Red (Bad match), Consistent = Green (Good match)
            fill_color   <- if(is_divergent) "#ffe6e6" else "#e6fffa"
            line_color   <- if(is_divergent) "#cc0000" else "#007a5e"
            
            trend_vals <- round(tail(scaled_df[[var_name]], 12), 3)
            
 
            spk_html <- sparkline::spk_chr(
                trend_vals, 
                type = "line", 
                lineColor = line_color, 
                fillColor = fill_color,
                width = 120, 
                height = 25,
                spotColor = FALSE,
                minSpotColor = FALSE,
                maxSpotColor = FALSE,
                lineWidth = 1.5
            )
            
            clean_name <- if(!is.null(pretty_names[[var_name]])) pretty_names[[var_name]] else gsub("_", " ", var_name)
            
            # Formatting Numbers
            val_fmt  <- paste0("<span class='mono-num'>", sprintf("%.2f", val), "</span>")
            mean_fmt <- paste0("<span class='mono-num'>", sprintf("%.2f", mean_val), "</span>")
            diff_fmt <- paste0("<span class='mono-num'>", sprintf("%.2f", dist_abs), "</span>")
            
            data.frame(
                Indicator = clean_name,
                `Current (Z)` = val_fmt,
                `Baseline (Z)` = mean_fmt,
                `Difference (Threshold < 1.0)` = diff_fmt,
                Status    = status_txt,
                Trend     = spk_html,
                dist_abs  = dist_abs,
                stringsAsFactors = FALSE,
                check.names = FALSE
            )
        })
        
        final_df <- do.call(rbind, rows)
        
        final_df <- final_df %>% 
            dplyr::arrange(desc(dist_abs)) %>%
            dplyr::select(-dist_abs)
        
        d <- DT::datatable(
            final_df,
            rownames = FALSE, 
            escape = FALSE, 
            selection = "none",
            class = 'table clean-table hover', 
            options = list(
                dom = 't', 
                paging = FALSE,
                ordering = FALSE,
                columnDefs = list(
                    list(className = 'dt-center', targets = "_all"),
                    list(width = '120px', targets = 5)
                ),
                fnDrawCallback = htmlwidgets::JS("function(){ HTMLWidgets.staticRender(); }")
            )
        ) %>% 
            DT::formatStyle(
                'Status',
                target = 'cell',
                backgroundColor = DT::styleEqual(
                    c("Inconsistent", "Consistent"), 
                    c("#ffe6e6", "#e6fffa")
                ),
                color = DT::styleEqual(
                    c("Inconsistent", "Consistent"), 
                    c("#cc0000", "#007a5e")
                ),
                fontWeight = 'bold'
            )
        
        d$dependencies <- append(d$dependencies, htmlwidgets:::getDependency("sparkline", "sparkline"))
        
        d
    })
    
    ### Summary Metrics ----
    output$summary_metrics <- renderUI({
        fcst <- forecast_results()
        req(nrow(fcst) > 0)
        fw <- fred_wide()
        
        full_df <- fw %>%
            dplyr::inner_join(fcst %>% dplyr::select(Date, regime, state_hi), by = "Date") %>%
            dplyr::arrange(Date) %>%
            tidyr::fill(where(is.numeric), .direction = "down") %>%
            dplyr::select(-Date)
        
        scaled_df <- full_df %>%
            dplyr::mutate(across(where(is.numeric) & !c(state_hi), ~scale(.)[,1]))
        
        last_row <- tail(scaled_df, 1)
        current_regime <- last_row$regime
        is_bad_state   <- last_row$state_hi == 1 
        
        indicators <- names(scaled_df)[sapply(scaled_df, is.numeric)]
        indicators <- setdiff(indicators, c("p_high", "state_hi", "is_fcst", "day_num"))
        
        vals_current <- as.numeric(last_row[indicators])
        
        means_by_regime <- scaled_df %>%
            dplyr::group_by(regime) %>%
            dplyr::summarise(across(where(is.numeric) & !c(state_hi), \(x) mean(x, na.rm = TRUE)))
        
        mean_curr_vec <- means_by_regime %>% 
            dplyr::filter(regime == current_regime) %>% 
            dplyr::select(all_of(indicators)) %>% 
            as.numeric()
        
        mean_other_vec <- means_by_regime %>% 
            dplyr::filter(regime != current_regime) %>% 
            dplyr::select(all_of(indicators)) %>% 
            as.numeric()
        
        dist_to_curr <- abs(vals_current - mean_curr_vec)
        n_aligned <- sum(dist_to_curr <= 1.0)
        pct_aligned <- (n_aligned / length(indicators)) * 100
        
        avg_deviation <- mean(dist_to_curr)
        
        dist_to_other <- abs(vals_current - mean_other_vec)
        n_leaning_other <- sum(dist_to_other < dist_to_curr)
        pct_leaning_other <- (n_leaning_other / length(indicators)) * 100
        
        m1_color <- if(pct_aligned > 70) "success" else if(pct_aligned > 40) "warning" else "danger"
        m1_desc  <- "Historical Match"
        m1_foot  <- "Percent of drivers behaving normally for this regime"
        
        m2_color <- if(avg_deviation < 0.8) "success" else if(avg_deviation < 1.2) "warning" else "danger"
        m2_desc  <- "Anomaly Score (Z)"
        m2_foot  <- "Average standard deviation from the baseline"
        
        if (is_bad_state) {
            if(pct_leaning_other > 50) {
                m3_color <- "success"
                m3_desc  <- "Recovery Detected"
                m3_foot  <- "Drivers are shifting toward growth patterns"
                m3_icon  <- "arrow-up"
            } else {
                m3_color <- "danger"
                m3_desc  <- "Deep Contraction"
                m3_foot  <- "Drivers are reinforcing the negative trend"
                m3_icon  <- "anchor"
            }
        } else {
            if(pct_leaning_other > 50) {
                m3_color <- "danger"
                m3_desc  <- "Warning Signal"
                m3_foot  <- "Drivers are shifting toward volatility"
                m3_icon  <- "exclamation-triangle"
            } else {
                m3_color <- "success"
                m3_desc  <- "Strong Expansion"
                m3_foot  <- "Drivers are reinforcing the stable trend"
                m3_icon  <- "shield-alt"
            }
        }
        
        fluidRow(
            bs4ValueBox(
                value = paste0(round(pct_aligned, 0), "%"),
                subtitle = m1_desc,
                footer = m1_foot,
                icon = icon("check-circle"),
                color = m1_color,
                width = 4
            ),
            bs4ValueBox(
                value = round(avg_deviation, 2),
                subtitle = m2_desc,
                footer = m2_foot,
                icon = icon("chart-bar"),
                color = m2_color,
                width = 4
            ),
            bs4ValueBox(
                value = paste0(round(pct_leaning_other, 0), "%"),
                subtitle = m3_desc,
                footer = m3_foot, 
                icon = icon(m3_icon),
                color = m3_color,
                width = 4
            )
        )
    })
}

options(shiny.host = "0.0.0.0", shiny.port = 3838)

# Run App
shinyApp(ui, server)