README
================

# Making a Dashboard in R Using bs4Dash

Welcome! This repo contains the files I had used for my fist attempt at
making a dashboard in Shiny. If you are interested, you can find the
deployed app at Shinyapps.io by clicking
[here](https://patrickgreen93.shinyapps.io/MyApp/). If you are
interested in deploying the repo locally on your own device, note that
the only files necessary to include are: global.r, ui.r, server.r and
the Data folder.

The purpose of this dashboard was really just for me to get used to
reactivity, converting a project I worked on using Markdown into shiny.
In order to protect the client’s privacy, all data was altered,
including the values, variable names and removing several identifiers.
Even the state that used in the leaflet plot has been changed. Doing so
also helped familiarize myself a bit with insertion of CSS/HTML and JS
into R, as well as some of the more modern data visualization packages
out there which I highly recommend checking out, including:
[bs4Dash](https://github.com/RinteRface/bs4Dash),
[Leaflet](https://github.com/rstudio/leaflet),
[Highcharter](https://github.com/jbkunst/highcharter), and
[billboarder](https://github.com/dreamRs/billboarder).

Also, something not included in the actual project, I did explore the
inclusion of statistic elements (e.g., simple/multipl regression, RWA,
Kruskall-Wallis, and Dunns test). The method of running these
calculations is included in the Fake_Datasets.r file. I also found
several interesting (for me at least) interesting packages related to
plotting the results of these statistical analyses, including:
[performance](https://github.com/easystats/performance) and
[ggstatsplot](https://github.com/IndrajeetPatil/ggstatsplot).

## Brief gif of content

<img src="misc/MyApp.gif" title="A caption" alt="A caption" width="100%" />

## Disclaimer

The dashboard is nowhere near optimized at this point. Working with such
a large dataset while using more graphically intense packages (e.g.,
performance, leaflet, etc.) was certainly a challenge. Not too mention
that incorporating the light/dark mode button involved re-rendering
various elements, which likely isn’t the best way of going about it. If
anyone viewing this notices such issues (or others) and wants to leave a
comment on ways to improve the dashboard, please feel free to do so in
whatever form is most convenient to you (e.g., private message, issue,
etc.). This project was an extension of a static dashboard made in
markdown, so that I could learn shiny. Therefore, I’m more than happy to
receive any form of feedback to further grow. Thanks!
