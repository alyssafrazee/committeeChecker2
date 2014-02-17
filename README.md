## introducing committee checker 2.0!

unofficial name: "The Mary Joy"

Forming exam committees can be kind of a pain, and double checking whether your committee satistfies all the rules can be super time-consuming. Luckily, it's just a bunch of logic, which makes it a fun programming exercise! So here's [an app that validates your exam committee](https://alyssafrazee.shinyapps.io/committees/). I made it to learn about [Shiny](http://www.rstudio.com/shiny/), an R-based framework for building interactive web apps. 

This app validates committees based on the rules set out by the Johns Hopkins Bloomberg School of Public Health. I've copied the rules for the preliminary oral exam committee [here](http://gist.io/9012395) and the rules for the thesis committee [here](http://gist.io/9012377). They should be accurate as of June 2012, but are likely to change. Giant disclaimer: JHSPH is _not at all_ affiliated with this app (though _I_ am affiliated with JHSPH). Please use it to help you create your committee, or to learn about Shiny or R, but use your departmental academic coordinator for the official stuff, okay? 

I wrote a little about this app and my thoughts on making it with Shiny in [this blog post](http://alyssafrazee.com/committee-checker-2.html).

You can download this code (`ui.R` and `server.R`) and run it locally on your machine (by downloading `shiny` and running `runApp()` in R, where your working directory contains `ui.R` and `server.R`), OR you can just use the deployed web app at https://alyssafrazee.shinyapps.io/committees. 

[This gist](https://gist.github.com/alyssafrazee/7094055) has the code for committee checker 1.0, in case you were curious. It's just a script you can run, and it probably has a few bugs.
