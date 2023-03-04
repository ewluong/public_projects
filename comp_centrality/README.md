Project Update 03/02/2023
In this project update, we explored the relationships between the centrality measures of executives in the Dow Jones Industrial Average companies and the financial performance of those companies. We used data on executive directors, financial performance metrics such as Return on Equity (ROE) and Return on Assets (ROA), and various centrality measures from the network analysis library NetworkX.

We began by calculating the eigenvector centrality, betweenness centrality, and closeness centrality for each executive in the dataset. We then aggregated these measures for each company to obtain an overall measure of centrality for that company. We also removed certain companies (BA, HPQ, MCD) from our analysis.

We then ran Ordinary Least Squares (OLS) regression models to examine the relationship between the centrality measures and the financial performance metrics. Our results showed that both the eigenvector centrality and betweenness centrality measures were positively associated with ROA but negatively associated with ROE. Closeness centrality did not appear to have a significant relationship with either ROA or ROE.

To improve our analysis, we also tried scaling the ROA and ROE variables and imputing missing values for the ROE variable. However, these changes did not significantly affect our regression results.

Overall, our analysis suggests that the centrality of executive directors in Dow Jones Industrial Average companies may have a significant impact on the financial performance of those companies. These findings may have implications for how companies select and organize their executive leadership teams.