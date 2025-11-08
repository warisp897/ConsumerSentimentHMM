# **Analysis of Consumer Sentiment Using Hidden Markov Model Regimes**

This project successfully applied a **Gaussian Hidden Markov Model** with indicator-conditioned transitions to the University of Michigan Consumer Sentiment Index. The analysis moved beyond simple time-series diagnostics to identify and characterize two fundamental, latent economic states: a **High Sentiment Regime** and a **Low Sentiment Regime**.

## Economic Indicators
A set of 17 economic indicators were acquired from the Federal Reserve Economic Data of St. Louis  (**FRED**) from 1986-2024, providing a quality series to train from with many economic booms and falls. Each series is categorized into 5 economic categories:
- <b> Output </b>
- <b> Labor Market </b>
- <b> Price Levels </b>
- <b> Monetary and Fiscal </b>
- <b> Housing and Construction </b>

The dashboard goes into depth on each variable and its relationship to consumer sentiment, with economic analysis, correlation, and summary statistics.

## HMM Segment

The Dashboard includes a segment teaching the fundamentals of the **Hidden Markov Model**, and how it excels in certain forecasting use cases. Users can learn about each step of the model's algorithm, and adjust the transition and emissions matrix to observe how it affects the model's probabilities and ability to decode the hidden states.

## Key Findings

### <u> Robust Regime Identification </u>
The model was able to identify the existence of two statistically distinct sentiment regimes in the Consumer Sentiment.

* The **Emissions Density Plot** demonstrates the regimes were clearly separated by their observed CSI levels, with the High state having a significantly larger mean ($\mu_{High} \approx 95$) than the Low state ($\mu_{Low} \approx 76$).
*  The **t-SNE visualization** confirmed that data points corresponding to the two decoded HMM states clustered into two separate, non-arbitrary regions, validating the structural integrity of the inferred regimes. 

### <u> Economic Drivers </u>
The model was able to quantify the drivers of regime transition using a compact set of key economic indicators:

| Indicator | Mechanism | Relationship to Sentiment |
| :--- | :--- | :--- | :--- |
| **Real GDP** (12 month lag) | Real productivity/Income momentum | **Positive** |
| **PCE Price Index** | Cost-of-living pressure (Inflation) | **Negative** |
| **Federal Surplus/Deficit** (12 month lag)| Policy/Fiscal stability backdrop | **Negative** |

These indicators capture the primary economic factors influencing household confidence: **income, prices, and the policy environment.**

### <u> Dynamic Alignment with Economic Cycles </u>
The decoded HMM regimes provide an insightful historical narrative:

* The model successfully aligns the Low state (Red bands) with major US economic downturns (e.g., Dot-com, GFC, COVID-19) and their immediate aftermath.
* Crucially, the HMM captures the **transitions**, often signaling shifts in sentiment before official economic recessions were declared, highlighting its forward-looking utility.

---

## Utility and Future Outlook

The model can be turned into a powerful **Risk Flagging Tool**, offering a level of depth beyond traditional forecasting:

1.  **Conditional Regime Tracking:** The model can be used for conditional nowcasting, projecting the current or near-future regime state given the most recently realized indicator data.
2.  **Risk Assessment:** By analyzing the transition probability matrix in real-time, the model can quantify and flag elevated flip risk when the current indicator values enter a configuration that historically precedes a shift toward the Low Sentiment regime.
3.  **Extensibility:** The framework is robust, allowing for future extensions such as incorporating more complex emissions, exploring more states (Neutral/High/Low), and integration into a streaming pipeline for low-latency decision support.

The model's value is in it's ability to provide quantifiable evidence of when and why the underlying economic ground is shifting for the average consumer, offering far more than traditional time series regression methods.
