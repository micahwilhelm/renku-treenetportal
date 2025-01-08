FROM rocker/shiny:4

# set permissions of the R library directory to be editable by shiny (997:997)
# see https://github.com/SwissDataScienceCenter/renkulab-docker/blob/main/docker/r/Dockerfile
ENV NB_UID=997
ENV NB_GID=997
COPY fix-permissions.sh /usr/local/bin
RUN fix-permissions.sh /usr/local/lib/R

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

USER shiny
