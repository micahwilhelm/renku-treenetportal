FROM rocker/shiny:4

# set permissions of the R library directory to be editable by shiny (997:997)
# see https://github.com/SwissDataScienceCenter/renkulab-docker/blob/main/docker/r/Dockerfile
ENV NB_UID=997
ENV NB_GID=997

# Fix R library permissions
COPY fix-permissions.sh /usr/local/bin
RUN fix-permissions.sh /usr/local/lib/R

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gettext-base \
    libpq-dev \
    libmagick++-dev \
    python3 \
    python3-pip \
 && pip3 install --break-system-packages requests urllib3 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
 
COPY renv.lock /home/shiny/app/renv.lock
COPY renv /home/shiny/app/renv

# Install R dependencies via renv
RUN R -e "install.packages('renv', version = "1.1.4", repos='https://cloud.r-project.org/')"
RUN R -e "renv::restore(lockfile = '/home/shiny/app/renv.lock')"

# Copy Shiny server configuration and entrypoint scripts
COPY shiny-server.conf.tpl /shiny-server.conf.tpl
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chown shiny:shiny /etc/shiny-server/shiny-server.conf

# Copy the Shiny app source code
COPY src/r/app /home/shiny/app

# Fix permissions after copying everything
RUN fix-permissions.sh /home/shiny/app /usr/local/lib/R /var/log/shiny-server

# Set the user to shiny
USER shiny

# Set the entrypoint and default command
ENTRYPOINT ["/bin/sh", "/docker-entrypoint.sh"]
CMD ["/init"]
