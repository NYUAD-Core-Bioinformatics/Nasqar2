FROM rocker/shiny:4.3.3

RUN apt-get update \
  && apt-get install vim libjpeg9  libtiff5-dev cmake  libmagick++-dev -y --no-install-recommends \
  && apt-get autoremove -yqq --purge \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ENV TZ=Asia/Dubai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN mkdir /var/log/nasqar
RUN chmod 777 /var/log/nasqar

USER shiny
WORKDIR /home/shiny
RUN wget \
    https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && mkdir /home/shiny/.conda \
    && bash Miniconda3-latest-Linux-x86_64.sh -b \
    && rm -f Miniconda3-latest-Linux-x86_64.sh
ENV PATH="/home/shiny/miniconda3/bin:$PATH"

# --- FIX: Configure conda to avoid Anaconda TOS error ---
RUN conda config --remove-key channels || true && \
    conda config --add channels conda-forge && \
    conda config --add channels bioconda && \
    conda config --set channel_priority strict && \
    conda config --show
    
RUN conda install -c bioconda -c conda-forge mamba -y
ADD environment.yaml environment.yaml
RUN mamba env create -f environment.yaml
RUN echo "alias l='ls -lah'" >> ~shiny/.bashrc
RUN conda init bash
SHELL ["/bin/bash", "-c"]


RUN echo "conda activate v_deseq2" >> ~shiny/.bashrc
RUN conda clean --all -y


RUN conda run -n v_deseq2 R -e "devtools::install_github('smin95/smplot2')"

USER root

RUN mkdir /opt/nasqar_build
WORKDIR /opt/nasqar_build

COPY src /opt/nasqar_build/app

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /opt/nasqar_build/shiny-server.sh
RUN chown -R shiny:shiny /opt/nasqar_build/

USER shiny
RUN chmod a+x /opt/nasqar_build/shiny-server.sh
EXPOSE 3232

CMD ["/bin/bash", "/opt/nasqar_build/shiny-server.sh"]
