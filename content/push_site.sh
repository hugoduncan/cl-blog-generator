#!/usr/bin/env bash

cd ~/projects/blog/content/site
rsync -avz -e ssh ~/projects/blog/content/site/ bluehost:\~/public_html/blog