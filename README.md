# Shiny Serving Demo

This is a demo project that serves a Shiny app from a Renku V2 project.

To use it, you can create a Renku V2 launcher that references the image URL in the project package. You need to also set the following settings:

- user id: 999 (the `shiny` user in the Rocker image)
- port: 3838



## References

This project builds on The Rocker Shiny which builds a Docker image containing the Shiny Server.

- https://rocker-project.org/images/versioned/shiny.html
- https://posit.co/download/shiny-server/
- https://github.com/rstudio/shiny-server/
