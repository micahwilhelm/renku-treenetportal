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
    python3-pip && \
    pip3 install --break-system-packages requests urllib3

# Copy Shiny server config and entrypoint logic
COPY shiny-server.conf.tpl /shiny-server.conf.tpl
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN touch /etc/shiny-server/shiny-server.conf && \
    chown shiny:shiny /etc/shiny-server/shiny-server.conf

# Copy app and R environment
COPY src/r/app /home/shiny/app
COPY renv.lock /home/shiny/app/renv.lock
COPY renv /home/shiny/app/renv

# Install R dependencies via renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/')"
RUN R -e "renv::restore(lockfile = '/home/shiny/app/renv.lock')"

# Copy config-setup script
COPY copy_config.sh /usr/local/bin/copy_config.sh
RUN chmod +x /usr/local/bin/copy_config.sh

# Use shiny user
USER shiny

# Entrypoint: copy config, then start container
ENTRYPOINT ["/usr/local/bin/copy_config.sh"]
CMD ["/bin/sh", "/docker-entrypoint.sh"]
