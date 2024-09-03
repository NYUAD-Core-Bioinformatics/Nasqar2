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


#deseq2shiny
COPY deseq2shiny /srv/shiny-server/deseq2shiny
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/deseq2shiny/environment.yaml
RUN /opt/conda/bin/conda run -n v_deseq2 R -e "devtools::install_github('smin95/smplot2')"


# animalcules
COPY animalcules  /srv/shiny-server/animalcules
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/animalcules/environment.yaml

# ATACseqQCShniy
COPY ATACseqQCShniy /srv/shiny-server/ATACseqQCShniy
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/ATACseqQCShniy/environment.yaml


# ClusterProfShinyGSEA
COPY ClusterProfShinyGSEA /srv/shiny-server/ClusterProfShinyGSEA
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/ClusterProfShinyGSEA/environment.yaml
RUN /opt/conda/bin/conda run -n v_enrichGSEA  R -e "install.packages('GOplot', repos='http://cran.rstudio.com/')"


#ClusterProfShinyORA
COPY ClusterProfShinyORA /srv/shiny-server/ClusterProfShinyORA
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/ClusterProfShinyORA/environment.yaml
RUN /opt/conda/bin/conda run -n v_enrichORA  R -e "install.packages('GOplot', repos='http://cran.rstudio.com/')"

#dada2Shiny
COPY dada2Shiny  /srv/shiny-server/dada2Shiny
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/dada2Shiny/environment.yaml

#debrowser
COPY DEBrowser /srv/shiny-server/DEBrowser
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/DEBrowser/environment.yaml


# GeneCountMerger
COPY GeneCountMerger /srv/shiny-server/GeneCountMerger
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/GeneCountMerger/environment.yaml


#monocle3
COPY monocle3 /srv/shiny-server/monocle3
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/monocle3/environment.yaml
RUN /opt/conda/bin/conda run -n v_monocle3 R -e "install.packages(c('Seurat'), repos ='https://cran.nyuad.nyu.edu/')"

#SeuratV5Shiny
COPY SeuratV5Shiny /srv/shiny-server/SeuratV5Shiny
RUN /opt/conda/bin/mamba env create -f /srv/shiny-server/SeuratV5Shiny/environment.yaml
RUN /opt/conda/bin/conda run -n v_seuratv5 R -e "install.packages(c('Seurat'), repos ='https://cran.nyuad.nyu.edu/')"



# Clean stale data
RUN /opt/conda/bin/conda clean -a -y



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
