#!/bin/bash
#exec shiny-server > /dev/null 2>&1
source /home/shiny/miniconda3/etc/profile.d/conda.sh
conda activate v_monocle3

# Print environment info for debugging
echo "R libraries available:" >> /var/log/nasqar/r-libs.log
R -e "installed.packages()[,1]" >> /var/log/nasqar/r-libs.log 2>&1

# Run with verbose logging
exec shiny-server --verbose >> /var/log/shiny-server/shiny-server.log 2>&1
