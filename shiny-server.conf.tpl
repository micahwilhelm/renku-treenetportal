# Instruct Shiny Server to run applications as the user "shiny"
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Renku will pass in the base URL path to the container
  location $RENKU_BASE_URL_PATH/ {

    # Host the directory of example Shiny Apps
    # site_dir /srv/shiny-server;
    # If you have assets that also need to be served, use site_dir
    app_dir /home/shiny/app;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
}
