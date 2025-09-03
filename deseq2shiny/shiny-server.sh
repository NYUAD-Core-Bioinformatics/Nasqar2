#!/bin/bash
#exec shiny-server > /dev/null 2>&1
source /home/shiny/miniconda3/etc/profile.d/conda.sh
conda activate v_monocle3
exec shiny-server >> /var/log/shiny-server/shiny-server.log 2>&1
