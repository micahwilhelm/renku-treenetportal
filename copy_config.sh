#!/bin/bash
set -e

mkdir -p ~/.filesender

# Copy from mounted Renku S3 path
cp ~/work/forestcast/.config/filesender.py.ini ~/.filesender/filesender.py.ini

# Set proper permissions
chown -R shiny:shiny ~/.filesender

exec "$@"
