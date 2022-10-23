# Fitting robust M-estimators with a `shiny` interface

You can find the [compiled version of the app at shinyapps.io](https://martinasladek.shinyapps.io/shiny_robust/)

A proof-of-cencept `shiny` app that allows the user to fit an M-estimator with a point-and-click interface. The app uses `lmrob` from the `robustbase` package and applies the  "KS2014" adjustment. Output allows the user to compare the robust model to an ordinary least squares (OLS) model. 

You can either load the demo data or try using your own. I'm no longer actively developing the code, I mostly just wanted to see how feasible it would to make an app that someone who doesn't code could use. The app also outputs the underlying R code for reproducibility. 

Verdict: Just about feasible, though headache inducing once you get to interactions. 

![](https://www.martinasladek.co.uk/post/robust-statistics-in-jasp-and-jamovi/images/shiny_robust.gif)

Here's a [blogpost I've written a while back overviewing robust methods and stats software](https://www.martinasladek.co.uk/post/robust-statistics-in-jasp-and-jamovi/)
