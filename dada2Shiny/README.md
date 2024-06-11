## Docker build
docker build -t dada2 .


## Docker run


docker run  -p 8090:3232 dada2

docker run  -p 8090:3232 --name dada2container dada2 
docker ps
docker stop dada2container


#remove only stopped contatiners
for i in `docker ps -a -q`
do
   docker rm $i
done



#Access bash program of a container

 docker exec -it dada2container bash



#mamba insatll 
mamba install -c bioconda -c conda-forge  bioconductor-dada2
