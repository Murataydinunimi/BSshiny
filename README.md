# BSshiny

The BSshiny calculates option prices through BS formula and shows the plots of Calls and Puts against their strikes.

As an example, the app extracts the option prices of SP index in real time and presents it to the user after doing some simple manipulations on the data. So, the user can use any option's values to price it simply doing copy-paste from the datatable presented on the main page with titles S&P calls and S&P puts to the interactive part where the arguments for BS formula are required. These datasets, S&P calls and S&P puts, have all the necesseary arguments for BS formula to work.

When the arguments correctly provided(For example maturity can not have a negative value because no options can start today and matures yesterday) by making up numbers, or using your own option from some index or by  simply using the data provided by the app, the app will output two prices given by the BS formula with titles Call price and Put price respectively, on the main page. Below the prices, there will be two graphs displayed within the titles Calls and Puts. In these graphs, the user will see the relationship between option prices and strike prices graphically.


Note that the Call and Put prices might be zero depending on the values given to the BS calculator.
