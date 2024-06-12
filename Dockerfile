FROM rocker/shiny:latest

# Install Miniconda
RUN apt-get update && apt-get install -y wget bzip2 nginx supervisor && \
    wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh && \
    /bin/bash ~/miniconda.sh -b -p /opt/conda && \
    rm ~/miniconda.sh && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \ 
    adduser --system --no-create-home --shell /bin/false --group --disabled-login nginx


RUN /opt/conda/bin/conda install -c bioconda -c conda-forge mamba -y

# Create multiple conda environments and install RShiny


COPY deseq2shiny /srv/shiny-server/deseq2shiny
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/deseq2shiny/environment.yaml



#RUN /opt/conda/bin/conda create -n shiny_env1 -c r r-shiny && \
#    /opt/conda/bin/conda create -n shiny_env2 -c r r-shiny

# Copy your Shiny app files to the container

# Add Nginx configuration
COPY nginx.conf /etc/nginx/nginx.conf
COPY uiApp/src/out/ /usr/share/nginx/html

# Add the supervisor configuration file
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# Expose the port for the Nginx server
EXPOSE 80

# Command to start supervisord
CMD ["/usr/bin/supervisord"]
