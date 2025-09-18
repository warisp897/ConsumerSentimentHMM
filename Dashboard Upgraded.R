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
library(shinyjs)
library(reactable)

library(ggplot2)
library(depmixS4)

# SHOULD NOT BE NEEDED

# mint_theme <- bs_theme(
#     bootswatch = "flatly",  # clean base
#     primary = "#98FF98",    # mint green base
#     secondary = "#2E8B57",  # optional darker green accent
#     base_font = font_google("Nunito Sans"),
#     heading_font = font_google("Quicksand")
# )


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
    
    <p>For this analysis, a series of 20 macroeconomic indicators were collected from 1987 to 2024 from the 
    <strong>Federal Reserve of St. Louis (FRED)</strong>, a widely respected source for accurate, up-to-date economic data. 
    This period provides a robust dataset for the Hidden Markov Model (HMM), offering a balance between the length of the time 
    series and the number of economic sectors represented. The selection of indicators is critical as they serve as the 
    <strong>observed variables</strong> in the HMM, providing the data to train the model and identify the complex, 
    non-linear relationships and their effect on sentiment.</p>
    
    
     <p>The indicators are categorized as the following:</p>
     <ul>
     <li><strong>Output</strong>: Measures of economic activity which reflect the overall health of the economy.</li>
     
     <li> <strong>Labor Market</strong>: Indicators measuring employment, which directly impact people's financial 
     security and future outlook.
     
     </li>
     
     <li><strong>Price Levels</strong>: Inflation metrics which greatly affect purchasing power and consumer confidence.</li>
     
     <li><strong>Monetary and Fiscal</strong>: Data on interest rates and government spending, which 
     shape the financial environment and economic policy.</li>
     
     <li><strong>Housing and Construction</strong>: Indicators such as housing starts and building permits, which serve as leading 
     indicators of economic cycles and household confidence.</li>
     
     <li><strong>Trade</strong>: Metrics like the trade balance, which reflect the global economic position and can influence domestic 
     economic conditions.</li>
     
     </ul>
     <p>Detailed information about each category and their relationship to consumer sentiment is in 
     <a href='javascript:void(0);' id='prelim_analysis'>Preliminary Analysis</a>.</p>
     "
)

## Preliminary Analysis Text ----

output_analysis <- HTML('
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
    ')

labor_analysis <- HTML('
      <p><strong>Labor market indicators:</strong> measures of how easily people find and keep jobs.</p>
      <ul>
        <li><strong>Unemployment rate:</strong> percent of the labor force without a job. 
          Rising unemployment often reflects businesses cutting back. This tends to dent consumer sentiment as job security erodes. 
          Falling unemployment generally lifts confidence and spending.</li>
        <li><strong>Participation rate:</strong> share of working‑age people in the labor force. 
          A rising participation rate can signal optimism that jobs are available, bolstering sentiment; 
          a falling rate may reflect discouraged workers leaving the market, dampening overall confidence.</li>
        <li><strong>Unemployment claims:</strong> weekly filings for jobless benefits. 
          Spikes in initial claims often presage layoffs and can spook consumers, while declines point to a stable labor market and support higher sentiment.</li>
      </ul>
      <p>Together, these indicators tell you how tight or slack the jobs market is, a key driver of household incomes and therefore consumer attitudes toward spending and saving.</p>
    ')

price_analysis <- HTML('
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
    ')

monetary_analysis <- HTML('
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
    ')

housing_analysis <- HTML('
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
    ')

trade_analysis <- HTML('
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

## Hidden Markov Model Text ----

intro_hmm_text <- HTML(
    "
    <p> </p>
    <p>
    <b> A Hidden Markov Model (HMM) </b> is a statistical tool used to understand systems where you can't directly 
    observe the state of the system, but you can observe evidence or signals that are influenced by that state.
    </p>
    
    <p>
    Think of it like trying to guess the weather on another planet. You can't see if it's sunny 
    or rainy from where you are, but if you are told the humidity is low, there is a high-pressure system, and wind speeds are low, 
    so you can make a good guess that it is likely calm weather right now.
    </p>
    
    <p>
    There is two layers to this: the unobservable hidden states (the weather type) and the observations we can see 
    (the meteorological indicators). The model assumes that the sequence of hidden states follows a specific type of random process 
    called a <b> Markov chain </b>, where the next state only depends on the current state.
    </p>
    
    <p>
    The primary goal of a Hidden Markov Model is to infer the most likely sequence of hidden states given a sequence of observations. 
    For example, if you observe a pattern of <b> (Arid, Arid, Humid) </b> over three days, the HMM can calculate 
    the most probable weather sequence, such as <b> (Sunny, Sunny, Rainy) </b>.
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

model_result_info <- HTML(
    "<div style='font-size:18px; line-height:1.6;'>
  <p>
    To optimize the Hidden Markov Model algorithm, the model is reduced to use a select number of the predictors for analysis. 
    A set of 7 predictors were determined to be the most accurate in detecting hidden states using an algorithm of 
    maximizing the log-likelihood of each possible set. The predictors most effective for predicting consumer sentiment are:
    
    <ol>
      <li>Nominal GDP</li>
      <li>Real GDP</li>
      <li>Unemployment Rate</li>
      <li>Robust PCEPI</li>
      <li>Federal Funds Rate</li>
      <li>Case-Schiller Index</li>
     </ol>
    
  </p>
  
  By this, we can identify that the variables that affect consumer sentiment the most are the ones that hit consumers
  the hardest. <b> Nominal GDP </b> and <b> Real GDP </b> are a result of consumers (something about what they see on the news and how
  it affects their view of the economy? Find an article for this). The <b> Unemployment Rate </b> is a direct response
  to their ability to find work. The <b> Robust PCEIPI </b> is representative of their cost of living, as it represents
  how much they are spending each year, properly accounting for the (hyperlink blue) Substitution Effect. And finally, the <b> Federal Funds
  Rate </b> affects their ability to make large purchases, such as cars and homes. The presence of the <b> Case-Schiller Index </b> 
  further emphasizes this, as home ownership is parallel to economic propserity and the American Dream.
  
</div>"
)
  

# Matrices for HMM tab

A0  <- matrix(c(0.75,0.25,
                0.20,0.80), 2, byrow = TRUE)
B0  <- matrix(c(0.70,0.30,
                0.40,0.60), 2, byrow = TRUE)


# Dashboard Construction ----

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

ui <- bs4DashPage(
    freshTheme = mint_dark_theme,
    #title = "An Analysis of Consumer Sentiment States with a Hidden Markov Model",
    controlbar = NULL,
    
    # Header/navbar
    header = bs4DashNavbar(
        #title = bs4DashBrand(
            #title = "An Analysis of Consumer Sentiment States with a Hidden Markov Model",
        #),
        status = "success",
        #brandColor = "success",
        border = TRUE
    ),
    
    ## Sidebar with vertical menu ----
    sidebar = bs4DashSidebar(
        id   = "sidebar", # ID of the sidebar object (not the tabs)
        skin = "light",
        #status = "success",
        #brandColor = "success",
        #brandColor = "teal",
        collapsed = TRUE,
        bs4SidebarMenu(
            id = "dashboard_tabs", # ID of Sidebar menu
            bs4SidebarMenuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            bs4SidebarMenuItem("Preliminary Analysis", tabName = "preliminary_analysis", icon = icon("chart-line")),
            bs4SidebarMenuItem("Hidden Markov Model", tabName = "model_intro", icon = icon("project-diagram")),
            bs4SidebarMenuItem("Analysis", tabName = "model_analysis", icon = icon("chart-line")),
            bs4SidebarMenuItem("Conclusion", tabName = "model_conclusion", icon = icon("chart-line"))
        )
    ),
    
    ## Custom CSS Adjustments ----
    body = bs4DashBody(
        useShinyjs(),
        tags$head(
            
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
            
            #hmmwrap { position: relative; width: 100%; }
            #hmmwrap .base-svg { width: 100%; height: 60vh; display: block; }
            
            /* Hotspot container: positioned by % and centered on that point */
            .hs { position: absolute; transform: translate(-50%, -50%); pointer-events: auto; }
            
            /* Visible tag (the label on the arrow) */
            .hs .tag {
              font-weight: 600;
              padding: .15rem .35rem;
              border-radius: .5rem;
              background: rgba(0,0,0,.55);
              color: #fff;
              line-height: 1;
              font-size: clamp(10px, 1.2vw, 14px);
              backdrop-filter: blur(2px);
              -webkit-backdrop-filter: blur(2px);
            }
            
            /* The hover card */
            .hs .callout {
              position: absolute;
              left: 50%; top: -8px;
              transform: translate(-50%, -12px) scale(.98);
              opacity: 0; visibility: hidden;
              transition: opacity .18s ease, transform .18s ease, visibility 0s linear .18s;
              background: #fff; color: #222;
              border: 1px solid rgba(0,0,0,.12);
              box-shadow: 0 6px 22px rgba(0,0,0,.12);
              border-radius: .6rem;
              padding: .5rem .6rem;
              min-width: 180px; max-width: min(360px, 60vw);
              z-index: 2;
              white-space: normal;
            }
            
            #hmmwrap_all{
              position: relative;        /* base for absolute children */
              isolation: isolate;        /* own stacking context so z-index is predictable */
            }
            
            /* The base SVG just fills the box */
            #hmmwrap_all .base-svg{
              width: 100%; height: 60vh; display: block;
            }
            
            /* Each hotspot */
            #hmmwrap_all .hs{
              position: absolute;
              transform: translate(-50%, -50%);  /* center on left/top percent */
              z-index: 1;                        /* base layer */
            }
            
            /* Raise hovered/focused hotspot above siblings */
            #hmmwrap_all .hs:hover,
            #hmmwrap_all .hs:focus-within{
              z-index: 9999;
            }
            
            /* The popup */
            #hmmwrap_all .hs .callout{
              position: absolute;
              left: 50%; top: -10px;              /* above the pill */
              transform: translate(-50%, -100%);
              min-width: 220px; max-width: 360px;
              padding: 10px 12px;
              border-radius: 10px;
              background: #fff; color: #333;
              box-shadow: 0 12px 30px rgba(0,0,0,.25);
              opacity: 0; visibility: hidden;
              pointer-events: none;               /* pointer stays on parent */
              transition: opacity .12s ease, transform .12s ease;
              z-index: 2147483647;                /* above everything in this context */
            }
            
            /* Show popup on hover/focus */
            #hmmwrap_all .hs:hover .callout,
            #hmmwrap_all .hs:focus-within .callout{
              opacity: 1; visibility: visible;
              transform: translate(-50%, -110%);
            }
            
            /* Palette for overlay pills */
            #hmmwrap_all{
              --pi:    #2e86de;   /* initial probs (π) */
              --trans: #e74c3c;   /* transitions (A)  */
              --emit:  #27ae60;   /* emissions (B)    */
              --tagText: #fff;
            }
            
            /* Default pill (fallback) */
            #hmmwrap_all .hs .tag{
              color: var(--tagText);
              font-weight: 600;
              padding: .18rem .50rem;
              border-radius: .55rem;
              background: rgba(0,0,0,.35);          /* fallback neutral */
              backdrop-filter: blur(2px);
              -webkit-backdrop-filter: blur(2px);
            }
            
            /* Group tints (override the neutral background) */
            #hmmwrap_all .hs.pi   .tag{ background: var(--pi); }
            #hmmwrap_all .hs.trans.tag,                 /* in case .tag sits on the same node */
            #hmmwrap_all .hs.trans .tag{ background: var(--trans); }
            #hmmwrap_all .hs.emit .tag{ background: var(--emit); }
            
            /* Optional: slightly brighter on hover/focus */
            #hmmwrap_all .hs:hover .tag,
            #hmmwrap_all .hs:focus-within .tag{
              filter: brightness(1.05);
            }
            
            /* Frame draws [  ] around the table */
            .mx-frame{
              position:relative; display:inline-block; padding:12px 18px; margin:8px 0;
            }
            .mx-frame::before, .mx-frame::after{
              content:''; position:absolute; top:4px; bottom:4px; width:12px;
              border-top:3px solid #222; border-bottom:3px solid #222;
            }
            .mx-frame::before{ left:0;  border-left:3px solid #222;  border-right:none;  border-radius:4px 0 0 4px; }
            .mx-frame::after { right:0; border-right:3px solid #222; border-left:none;  border-radius:0 4px 4px 0; }
            
            /* Handsontable → matrix vibe */
            .mx-table .htCore td, .mx-table .htCore th{ 
              border-color: rgba(0,0,0,.12);
            }
            .mx-table .htCore th{ 
              background: transparent; 
              font-weight:600; 
              font-family: 'Times New Roman', Georgia, serif; 
              font-style: italic;
            }
            /* center numbers and make them serif/italic like math */
            .mx-table .htCore td{ 
              text-align:center; 
              vertical-align:middle; 
              font-family:'Times New Roman', Georgia, serif; 
              font-style:italic; 
              font-size:1.05rem;
            }
            /* strong separators (like the line after headers and before row labels) */
            .mx-table .ht_clone_top .htCore th{ border-bottom:2px solid #222; }
            .mx-table .ht_clone_left .htCore th{ border-right:2px solid #222; }
            
            .mx-row { display:flex; align-items:center; justify-content:center; gap:.75rem; width:100%; }
            .mx-lead { text-align:center; font-weight:600; }
            
                /* Center card titles */
              .card-header { position: relative; }
              .card-header .card-title{
                float: none !important;   /* undo AdminLTE's float */
                width: 100%;
                text-align: center;
                margin: 0;
                font-size: 36px;
              }
              /* Keep collapse/close tools on the right */
              .card-header .card-tools{
                position: absolute;
                right: .75rem;
                top: .5rem;
              }
            
              /* Optional: center tab labels in tabbed headers/navs */
              .nav-tabs{ justify-content: center; }
              
            .irs-bar,
             .irs-bar-edge{
              background: #28a745 !important;
              border-color: #28a745 !important;
            }
            .irs-single,
            .irs-from,
            .irs-to{
              background: #28a745 !important;
            }
            .irs-handle > i:first-child{
              background: #28a745 !important;
            }
            
            <style>
              /* Center a label + the matrix side by side */
              .mx-row { display:flex; align-items:center; justify-content:center; gap:.75rem; width:100%; }
            
              /* The bracketed matrix container */
              .mx-mat {
                position: relative; display:inline-grid; grid-template-columns: repeat(2, 90px);
                gap: 10px; padding: 10px 14px; margin: 6px 0;
              }
              /* Draw [   ] brackets with pseudo elements */
              .mx-mat::before, .mx-mat::after {
                content:''; position:absolute; top:0; bottom:0; width:10px;
                border-top:3px solid #222; border-bottom:3px solid #222;
              }
              .mx-mat::before { left:-12px;  border-left:3px solid #222;  border-radius:6px 0 0 6px; }
              .mx-mat::after  { right:-12px; border-right:3px solid #222; border-radius:0 6px 6px 0; }
            
              /* Tighten Shiny form-group spacing inside the grid */
              .mx-mat .form-group { margin:0; }
            
              /* Inputs look mathy, centered */
              .mx-mat input.form-control {
                width: 90px; height: 2.2rem; text-align:center;
                font-family: 'Times New Roman', Georgia, serif; font-style: italic; font-size: 1.1rem;
                padding: .25rem .5rem;
              }
              /* Remove number spinners */
              .mx-mat input[type=number]::-webkit-outer-spin-button,
              .mx-mat input[type=number]::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }
              .mx-mat input[type=number] { -moz-appearance: textfield; }
            
              .mx-lead { text-align:center; font-weight:600; }
            </style>
            
          "))
        ),

        tags$script(src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=TeX-AMS-MML_HTMLorMML"),
        
        ## All Tab Content ----
        
        ### Introduction ----
        bs4TabItems(
            bs4TabItem(
                tabName = "overview",
                fluidRow(
                    # Box 2: Consumer Sentiment Over Time
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        title = HTML("<b>Analysis of Consumer Sentiment Using Hidden Markov Model Regimes</b>"),
                        width = 12,
                        status = "success",
                        #solidHeader = TRUE,
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
                tabName = "preliminary_analysis",
                #id = "analysis",
                fluidRow(
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        width = 5,
                        status = "success",
                        title = HTML('<b> Economic Indicator Preliminary Analysis </b>'),

                            div(
                                style = "min-height: 86vh; font-size:18px; line-height:1.5; display:flex; flex-direction:column;",
                                
                                # selector stays on top
                                selectInput("select1", "Indicator Category", choices = c(
                                    "Output", "Labor Market", "Price Levels",
                                    "Monetary and Fiscal", "Housing and Construction", "Trade"
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
                        highchartOutput("pearsons_plot_hc", width = "100%", height = "50vh")
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
                                                class = "d-flex align-items-center",      # vertical center (cross-axis)
                                                style = "min-height:70vh;",               # match the left chart height
                                                div(style = "width:100%;", uiOutput("em_step_details"))
                                            )
                                        )                                    )
                                ),
                                
                                # B) TWO-COLUMN LAYOUT for other tabs
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
                                                    
                                                    # Reserve the space BEFORE the chart renders
                                                    div(id = "hmm_chart_shell",
                                                        style = "height:40vh; min-height:420px;",
                                                        highcharter::highchartOutput("hmm_demo_timeline", height = "100%")
                                                    ),
                                                    
                                                    uiOutput("more_btn")
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            
                            # DETAILS VIEW (full width)
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
                
                # right side (text, sig factors, analysis)
                fluidRow(
                    bs4Card(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        title = "State Plot",
                        width = 7,
                        status = "success",
                        solidHeader = TRUE,
                        bs4Dash::tabsetPanel(
                            id = "model_analysis_tabs",
                            type = "pills",
                            tabPanel("Top Economic Indicators",
                                     div(style = "padding:10px; font-size:18px; line-height:1.5; overflow-y:auto;",
                                         model_result_info)
                            ),
                            tabPanel("Transition Matrix",
                                     div(style = "padding:10px; font-size:16px; line-height:1.5; height:398.5px; overflow-y:auto;",
                                         "ok",
                                         uiOutput("transition_mat", height = "600px", width = "100%"))
                            ),
                            tabPanel("t-SNE Plot",
                                     div(style = "padding:10px; font-size:16px; line-height:1.5; height:398.5px; overflow-y:auto;",
                                         "cs_interpretation")
                            ),
                            tabPanel("Data Collection",
                                     div(style = "padding:10px; font-size:16px; line-height:1.5; height:398.5px; overflow-y:auto;",
                                         cs_data_collection)
                            )
                        )
                    ),
                    
                    column(
                        collapsible = FALSE,   # removes collapse toggle
                        closable = FALSE,      # removes close icon
                        width = 5,
                        
                        # State plot, aligned to bottom
                        div(
                            style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: flex-end; height: calc(100vh - 200px);",  # 200px = height of header/navbar/etc
                            highchartOutput("state_plot", height = "650px", width = "100%")
                        )
                    )
                ),
            ),
            
            ### Model Conclusion ----
            bs4TabItem(
                tabName = "model_conclusion",
                bs4Card(
                    title = HTML('<span style="margin-left: 0%; font-size:36px
                                     "> <b> Model Conclusion </b> </span>'),
                    width = 12,
                    fluidRow(
                    )
                )
            )
        )
    )
        )


# Server function ----

server <- function(input, output, session) {
    
    ## Overview Code ----
    
    # link hyperlink blue prelim analysis text to tab
    onclick("prelim_analysis", {
        updateTabsetPanel(session, "dashboard_tabs", selected = "preliminary_analysis")
    })
    
    
    recessions <- data.frame(
        start = c("2000-03-01", "2007-12-01", "2020-03-01"),
        end   = c("2001-11-30", "2009-06-30", "2023-05-31"),
        name  = c("Dot-com Bubble", "Global Financial Crisis", "COVID-19 Pandemic"),
        color = c("rgba(255,0,0,0.3)", "rgba(255,0,0,0.3)", "rgba(255, 174, 66,0.3)"),
        stringsAsFactors = FALSE
    )
    
    
    ### Consumer Sentiment TS Plot ----
    scaled_data = readRDS(here("scaled_dataset.rds"))
    full_dataset = readRDS(here("full_dataset.rds"))
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
            hc_yAxis(title = list(text = "Index Value"),
                     plotLines = list(
                         list(
                             value = 100,
                             color = "grey",
                             dashStyle = "Dash",
                             width = 3,
                             zIndex = 5,
                             label = list(
                                 text = "Baseline (1966)",
                                 align = "left",
                                 style = list(color = "red",
                                              fontSize = "16px")
                             )
                         )
                     )
                ) %>%
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
            hc_title(text = "Consumer Sentiment Over Time",
                     style = list(
                         fontSize = "28px")
                     )%>%
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
    
    ## Preliminary Analysis Code ----
    
    ### Preliminary Analysis Switching Functionality ----
    output$category_summary <- renderUI({
        summary_text <- switch(input$select1,
                               
                               # Text displayed based on selected predictor type in combo box
                               
                               "Output" = output_analysis,
                               "Labor Market" = labor_analysis,
                               "Price Levels" = price_analysis,
                               "Monetary and Fiscal" = monetary_analysis,
                               "Housing and Construction" = housing_analysis,
                               "Trade" = trade_analysis
        )
        
        div(
            style = "font-size:18px; font-family:Helvetica; line-height:1.6;",
            summary_text
        )
    })
    
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
        switch(fmt,
               "$" = {
                   s <- formatC(abs(val), format = "f", digits = 2, big.mark = ",")
                   paste0(ifelse(val < 0, "-", ""), "$", s)
               },
               "%" = paste0(formatC(val, format = "f", digits = 2), "%"),
               round(val, 3)
        )
    }
    
    format_true <- function(val, ind) {
        if (!ind %in% names(indicator_formats)) return(round(val, 2))
        fmt <- indicator_formats[[ind]]
        if (is.na(val)) return(NA)
        if (fmt == "$") {
            s <- formatC(abs(val), format = "f", digits = 2, big.mark = ",")
            paste0(ifelse(val < 0, "-", ""), "$", s)
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
    
    # map from raw names to pretty labels:
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
    
    # Distinct color palette per category
    pal_out    <- brewer.pal(length(category_map$`Output`), "Blues")
    pal_lab    <- brewer.pal(length(category_map$`Labor Market`), "Greens")
    pal_price  <- brewer.pal(length(category_map$`Price Levels`), "Purples")
    pal_mon    <- brewer.pal(length(category_map$`Monetary and Fiscal`), "Reds")
    pal_house  <- brewer.pal(length(category_map$`Housing and Construction`), "BuGn")
    pal_trade  <- brewer.pal(length(category_map$`Trade`), "PuRd")
    
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
    
    scaled <- readRDS("scaled_dataset.rds") %>%
        pivot_longer(-Year, names_to = "Indicator", values_to = "Scaled")
    full   <- readRDS("full_dataset.rds") %>%
        pivot_longer(-Year, names_to = "Indicator", values_to = "True")
    
    
    #### Selected Indicators over Time Plot ----
    output$category_plot_hc <- renderHighchart({
        
        session$ns("category_plot_hc_loaded")
        
        
        plot_df <- scaled %>%
            left_join(full, by = c("Year","Indicator")) %>%
            mutate(
                Label    = nice_names[Indicator],
                fmt      = indicator_formats[Indicator],
            )
        
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
                formatter = JS("function () {
                      var fmt = this.point.format || '';
                      var v   = this.point.trueValue;
                      var txt;
                      if (fmt === '$') {
                        var sign = (v < 0 ? '-' : '');
                        var absv = Highcharts.numberFormat(Math.abs(v), 2);
                        txt = sign + '$' + absv;  // -$ before digits
                      } else if (fmt === '%') {
                        txt = Highcharts.numberFormat(v, 2) + '%';
                      } else {
                        txt = Highcharts.numberFormat(v, 2);
                      }
                      return '<b>' + this.series.name + '</b><br>' +
                             'Year: ' + this.x + '<br>' +
                             'Value: ' + txt;
                    }"
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
        req(isTRUE(input$category_plot_hc_loaded))
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
        req(isTRUE(input$category_plot_hc_loaded))
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
            df, rownames = FALSE, class = "compact stripe hover",
            options = list(dom='t', paging=FALSE, searching=FALSE, ordering=TRUE, autoWidth=TRUE)
        ) %>%
            DT::formatStyle(colnames(df)[-1], `min-width` = '120px', `padding` = '6px 12px') %>%
            DT::formatStyle(colnames(df)[1],  `min-width` = '180px')
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
        `Housing & Construction` = "#2D7C66",
        Trade                    = "#E40078"
    )
    
    # function to lower opacity of unselected bars
    with_alpha <- function(hex, a = 0.30){
        rgb <- grDevices::col2rgb(hex)
        sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], a)
    }
    
    output$pearsons_plot_hc <- renderHighchart({
        session$ns("pearsons_plot_hc_loaded")
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
            hc_chart(type = "bar") %>%
            hc_xAxis(categories = unname(cor_df$Label), reversed = TRUE) %>%
            hc_yAxis(title = list(text = "Pearson's r")) %>%
            hc_title(text = "Correlation to Consumer Sentiment") %>%
            hc_subtitle(text = "The greater the value, the more correlated the indicator is with Consumer Sentiment") %>%
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
                    "<span style='font-weight:bold;color:#5F1DF9'>&#9632;</span> Trade",
                    "</span>"
                ),
                useHTML = TRUE
            ) %>%
            hc_plotOptions(bar = list(pointWidth = 15))
    })
    
    #update bar chart when new combo box item is selected
    observeEvent(input$select1, {
        req(isTRUE(input$pearsons_plot_hc_loaded))
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
      The algorithm begins by making an initial guess for the model\'s parameters, collectively known as \\(\\theta\\). These parameters define the model\'s starting beliefs about the system.
      $$ \\theta = \\{\\pi, A, B \\} $$
      This includes \\(\\pi\\), the probability of starting in each hidden state; <b>A</b>, the probability of transitioning between hidden states; and <b>B</b>, the probability of seeing an observation from a given hidden state. For our weather example, we might guess there\'s a 50% chance the first day is <em>Sunny</em> (\\(\\pi\\)), a 10% chance a <em>Sunny</em> day is followed by a <em>Rainy</em> one (in matrix A), and an 80% chance a <em>Sunny</em> day results in an <em>Arid</em> observation (in matrix B).
    </p>
        </div>',
        
        estep = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 2: The E-Step (Expectation)</h4>
    <p>
      Using its current parameters, the model calculates the probability of each hidden state being the true state for every point in our observed data sequence. It does this using a formula for \\(\\gamma_t(i)\\):
      $$ \\gamma_t(i) = P(z_t=i | X, \\theta) $$
      This equation calculates the probability that the hidden state \\(z\\) at time \\(t\\) was state \\(i\\) (e.g., <em>Sunny</em>), given the entire sequence of observations \\(X\\) (e.g., Arid, Humid, Humid...). Rather than making a hard decision, it creates a soft, probabilistic map. For a day we observed <em>Arid</em> conditions, this step might conclude there\'s an 85% probability the underlying state was <em>Sunny</em> and a 15% probability it was <em>Rainy</em>.
    </p>
        </div>',
        
        mstep = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 3: The M-Step (Maximization)</h4>
    <p>
      With the probabilistic map from the E-step, the model updates its parameters to better explain the data. For example, the transition probabilities in matrix <b>A</b> are re-calculated based on the expected number of transitions that occurred.
       $$ A_{ij} = \\frac{\\text{Expected # of transitions from }i \\text{ to } j}{\\text{Expected # of transitions from } i} $$
      If the E-step frequently found that a high-probability <em>Sunny</em> day was followed by a high-probability <em>Rainy</em> day, this M-step will increase the value for the <em>Sunny</em> → <em>Rainy</em> transition. The same logic is applied to update the initial state (\\(\\pi\\)) and emission (<b>B</b>) probabilities, ensuring the new parameters are a better fit for the data.
    </p>
        </div>',
        
        ll = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 4: Compute Log-Likelihood</h4>
    <p>
      After updating the parameters, the algorithm "scores" how well the new model explains the observed data by calculating the log-likelihood.
      $$ L(\\theta) = \\log P(X|\\theta) $$
      This value, \\(L(\\theta)\\), is the logarithm of the total probability of observing our specific sequence of data (e.g., Arid, Humid, Humid...) given the current model parameters. In a properly functioning EM algorithm, this score should increase with each iteration, signaling that the model is getting progressively better.
    </p>
        </div>',
        
        check = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 5: Check for Convergence</h4>
    <p>
      The algorithm must decide whether to stop or perform another iteration. It stops if the improvement in the log-likelihood score becomes negligible, or if a maximum number of cycles is reached.
      $$ \\Delta L < \\epsilon $$
      This condition checks if the change in log-likelihood (\\(\\Delta L\\)) is less than a tiny tolerance value (\\(\\epsilon\\)). When the probabilities for <em>Sunny/Rainy</em> transitions and <em>Arid/Humid</em> emissions barely change from one cycle to the next, we can be confident the model has converged on a stable, locally optimal solution.
    </p>
        </div>',
        
        rep_em = '
        <div style="font-size: 18px; font-family: Helvetica;">
        <h4>Repeat Cycle: Iterative Refinement</h4>
  <p>
    If the model has not yet converged, the algorithm loops back to the E-step to begin a new cycle. This iterative process is the heart of how the model learns. The refined parameters from the previous M-step are now used as the new "current" parameters for the E-step.
  </p>
  <p>
    $$ \\theta_{old} \\rightarrow \\text{E-Step} \\rightarrow \\text{M-Step} \\rightarrow \\theta_{new} $$
            Because the new parameters (e.g., the updated probability of a <i>Sunny</i> to <i>Rainy</i> transition) are guaranteed to be a better fit for the data, the subsequent E-step will produce a more accurate probabilistic map of the hidden states. This improved map, in turn, allows the next M-step to find an even better set of parameters, further increasing the log-likelihood score. This cycle of refinement continues, with each loop bringing the model closer to an optimal solution.
        </p>
        </div>
        ',
        
        done = '
        <div style="font-size: 18px; font-family: Helvetica;">
    <h4>Step 6: Algorithm Complete</h4>
    <p>
      Once convergence is reached, the algorithm terminates. The final set of parameters, \\(\\theta^*\\), represents the fully trained model. These optimized probabilities capture the underlying structure of the training data. For example, the final model might have learned that a <em>Sunny</em> day has a 95% chance of being followed by another <em>Sunny</em> day, and an 85% chance of producing an <em>Arid</em> observation. This trained model can now be used for analysis or to predict hidden states from new data.
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
                ☀  & P_{☀→☀} & P_{☀→🌧} \\\\
                🌧 & P_{🌧→☀} & P_{🌧→🌧}
              \\end{array}
              \\right]
            \\]
          </div>
          '))
    })

    output$emissions_mat <- renderUI({
        withMathJax(HTML('
          <div id="trans-mx" class="mx" style="font-size:160%; text-align:center;">
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
        
        pi <- R_pi()       # length 2 (0.5, 0.5)
        A  <- A_input()    # 2x2, rows sum to 1 (validated)
        B  <- B_input()    # 2x2, rows sum to 1 (validated)
        
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
        post <- depmixS4::posterior(fit)
        zh   <- head(post$state, nrow(df))  # drop dummy row if it was added
        
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
        list(id="pi_sun", cls="pi",   left=45, top=30, col="#ADD8E6",
             label="π(Sunny)", info="<b>π(Sunny)</b> = The likelihood the model will start at <b> Sunny </b>"),
        list(id="pi_rain", cls="pi",  left=55, top=30, col="#ADD8E6",
             label="π(Rainy)", info="<b>π(Rainy)</b> = The likelihood the model will start at <b> Rainy </b>"),
        
        # transitions A
        list(id="p11", cls="trans", left=20, top=57, col="#e74c3c",
             label="P(Sunny → Sunny)",
             info="<b>P(Sunny → Sunny)</b> = The probability it will be <b> Sunny </b> tomorrow
             given that it is <b> Sunny </b> today"),
        list(id="p12", cls="trans", left=50, top=46.5, col="#e74c3c",
             label="P(Sunny → Rainy)",
             info="<b>P(Sunny → Rainy)</b> = The probability it will be <b> Rainy </b> tomorrow
             given that it is <b> Sunny </b> today"),
        list(id="p21", cls="trans", left=50, top=70, col="#e74c3c",
             label="P(Rainy → Sunny)",
             info="<b>P(Rainy → Sunny)</b> = The probability it will be <b> Sunny </b> tomorrow
             given that it is <b> Rainy </b> today"),
        list(id="p22", cls="trans", left=80, top=57, col="#e74c3c",
             label="P(Rainy → Rainy)",
             info="<b>P(Rainy → Rainy)</b> = The probability it will be <b> Rainy </b> tomorrow
             given that it is <b> Rainy </b> today"),
        
        # emissions B
        list(id="b11", cls="emit", left=58, top=80, col="#27ae60",
             label="Pr(Humid | Sunny)",
             info="<b>Pr(Humid | Sunny)</b> = The likelihood the air is <b> Humid </b> given that it is  <b> Sunny </b>"),
        list(id="b12", cls="emit", left=32, top=75, col="#27ae60",
             label="Pr(Arid | Sunny)",
             info="<b>Pr(Arid | Sunny)</b> = The likelihood the air is <b> Arid </b> given that it is <b> Sunny </b>"),
        list(id="b21", cls="emit", left=70, top=75, col="#27ae60",
             label="Pr(Humid | Rainy)",
             info="<b>Pr(Humid | Rainy)</b> = The likelihood the air is <b> Humid </b> given that it is <b> Rainy </b>"),
        list(id="b22", cls="emit", left=43, top=80, col="#27ae60",
             label="Pr(Arid | Rainy)",
             info="<b>Pr(Arid | Rainy)</b> = The likelihood the air is <b> Arid </b> given that it is <b> Rainy </b>")
    )
    
    
    svg_file <- "HMM_Diagram.svg"
    output$hmm_overlay_pi <- renderUI({
        
        tags$div(
            id = "hmmwrap_all",
            # Base SVG (responsive)
            tags$img(
                src = svg_file, class = "base-svg",
                alt = "HMM diagram",
                style = "width:100%; height:60dvh; display:block;"
            ),
            
            # Labels
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
        
        # factor → strings/indices
        z_chr  <- as.character(dd$z)     # "S1" / "S2"
        x_chr  <- as.character(dd$x)     # "Humid" / "Arid"
        zh_chr <- as.character(dd$zh)
        
        z_i <- as.integer(dd$z)          # 1 / 2
        x_i <- as.integer(dd$x)          # 1 / 2
        
        # per-step probabilities actually used under A/B for the simulated path
        trans_prob <- c(NA_real_, vapply(2:n, function(t) A[z_i[t-1], z_i[t]], numeric(1)))
        emit_prob  <- vapply(1:n,  function(t) B[z_i[t],   x_i[t]],   numeric(1))
        
        # labels (adjust to your theme)
        state_sym  <- c(S1 = "☀",  S2 = "🌧")
        state_name <- c(S1 = "Sunny", S2 = "Rainy")
        obs_name   <- c(Humid = "Humid", Arid = "Arid")
        
        data.frame(
            Time        = dd$t,
            Transition  = c("—", paste0(state_sym[z_chr[-n]], " → ", state_sym[z_chr[-1]])),
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
        df <- detail_df(); req(df)
        
        reactable::reactable(
            df,
            pagination = FALSE,
            bordered   = TRUE,
            striped    = TRUE,
            highlight  = TRUE,
            defaultColDef = reactable::colDef(align = "center"),
            columns = list(
                Transition  = reactable::colDef(header = "z(t−1) → z(t)"),
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
        req(demo_data())  # eventReactive is NULL until Run is clicked
        div(
            actionButton("show_details", "More details", icon = icon("table")),
            style = "margin-top:.5rem; display:flex; justify-content:flex-end;"
        )
    })
    
    # Swap views WITHOUT rebuilding anything
    observeEvent(input$show_details, {
        shinyjs::hide("sim_shell")      # hides matrix + slider + run + chart + svg
        shinyjs::show("details_shell")  # shows full-width table + Back
    })
    
    observeEvent(input$back_to_chart, {
        shinyjs::show("sim_shell")
        shinyjs::hide("details_shell")
    })
    
    
    
    ## Analysis Code ----
    
    ### 3D TSNE PLOT ----
    output$state_plot <- renderHighchart({
        
        # reading in data
        sent_hmm = readRDS("sent_hmm.rds")
        tsne_result = readRDS("tnse_data.rds")
        
        
        fit_hmm = fit(sent_hmm)
        predicted_states <- posterior(fit_hmm)$state
        
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
    
    ### True Transition matrix ----
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