# Nasqar2

### Install locally with Docker
Official Nasqar2 image is hosted in Dockerhub. Run Nasqar2 in a Docker container and access it at http://localhost:80.

Make sure Docker software is up and running.

**Pull the image:**
```
docker pull nyuadcorebio/nasqarall:latest
```

**Run on port 80:**
```
docker run -p 80:80 nyuadcorebio/nasqarall:latest
```

- If you run this service on a server, specify the (IP-address or hostname):80 on the browser.
- If you run this service on a standalone machine (e.g. laptop), specify localhost:80 on the browser.

To run Nasqar2 on another port, e.g. 8080:

```
docker run -p 8080:80 nyuadcorebio/nasqarall:latest
```
It can be accessed via http://localhost:8080

### Build a local image with Docker ( Optional )
If you want to customize the code and then build the docker image. Refer to below instructions.




Build the docker image as follows:-
Note:- Make sure you have sufficient space. It will be around 10GB and takes an hour to finish.
```
docker build  --progress=plain  -t <specify-image-name> .
```

Verify the build image
```
docker image ls
```

To start Nasqar2 using the build image.
```
docker run --name <specify-name-of-container> -p 80:80 -it <specify-image-name>
```
It can be accessed via http://localhost:80


