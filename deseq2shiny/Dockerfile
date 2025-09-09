FROM rocker/shiny:4.3.3

# --- System dependencies ---
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
     vim libjpeg9 libtiff5-dev cmake libmagick++-dev wget curl \
  && apt-get autoremove -yqq --purge \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# --- Timezone ---
ENV TZ=Asia/Dubai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# --- Logging directory ---
RUN mkdir /var/log/nasqar && chmod 777 /var/log/nasqar

# Switch to shiny user
USER shiny
WORKDIR /home/shiny

# --- Install Mambaforge (replaces Miniconda + manual mamba install) ---
RUN wget https://github.com/conda-forge/miniforge/releases/download/25.3.1-0/Miniforge3-25.3.1-0-Linux-x86_64.sh \
    && mkdir -p /home/shiny/.conda \
    && bash Miniforge3-25.3.1-0-Linux-x86_64.sh -b \
    && rm -f Miniforge3-25.3.1-0-Linux-x86_64.sh

ENV PATH="/home/shiny/mambaforge/bin:$PATH"

# Use bash as default shell for conda/mamba commands
SHELL ["/bin/bash", "-lc"]

# --- Copy environment file early to leverage Docker cache ---
ADD environment.yaml /home/shiny/environment.yaml

# --- Create conda environment using mamba ---
RUN mamba env create -f /home/shiny/environment.yaml

# Add alias for user convenience
RUN echo "alias l='ls -lah'" >> /home/shiny/.bashrc

# Initialize conda for bash shells
RUN conda init bash
RUN echo "conda activate v_deseq2" >> /home/shiny/.bashrc

# Clean up unused packages and caches
RUN conda clean --all -y

# Install R package smplot2 inside v_deseq2 environment
RUN conda run -n v_deseq2 R -e "devtools::install_github('smin95/smplot2')"

# Switch to root for final steps
USER root
RUN mkdir /opt/nasqar_build
WORKDIR /opt/nasqar_build

# Copy application files
COPY src /opt/nasqar_build/app
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /opt/nasqar_build/shiny-server.sh
RUN chown -R shiny:shiny /opt/nasqar_build/

USER shiny
RUN chmod a+x /opt/nasqar_build/shiny-server.sh

EXPOSE 3232

CMD ["/bin/bash", "/opt/nasqar_build/shiny-server.sh"]
