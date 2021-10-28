# sRedList-API
Plumber R application:
To run the server you must:
1)Install the following libraries in R:
library(plumber)
library(logger)
library(glue)
library(urltools)
library(Rook) ; library(ggplot2) ; library(raster) ; library(dplyr) ; library(sf) ; library(rgdal) ; library(plyr) ; library(ncdf4) ; library(ggalluvial) ; library(rredlist)
2)run the script inside the R env:
source(".server.R")

Run the plumber's API on http://127.0.0.1:8000
Running swagger Docs on http://127.0.0.1:8000/__docs__/



0) Upload Zip project in server or pull image directly
- unzip <file>


1) Change the path of the rasters contained in sRedList/data/ within the project's config.yml file.
  * alt_raster_path
  * cci1_raster_path
  * cci2_raster_path

  - Change in API.R : res$setHeader("Access-Control-Allow-Origin", "http://193.204.79.97")
  Change "*" with IP Client


Note: Remember to delete the previous docker image each time if not space left on the disk
  (sudo docker rmi sredlist-api)



2) Create Docker image (Go to the folder where the dockerfile is located and execute)

sudo docker build -t sredlist-api .

With Raster Data:
in path sredlist:

sudo docker build -t sredlist-api -f sredlist-server-develop/Dockerfile .


3) Run the container

sudo docker run --rm -p 8000:8000 sredlist-api

If you prefer to run the instance in foreground mode, you can use:
sudo docker run -d --name sredlist --restart=always --interactive --tty -p 8000:8000 sredlist-api

4) Stop container (*):
sudo docker stop c2d110be8ca3
sudo docker stop sredlist

5) Start container
If you prefer to run the instance in foreground mode, you can use:

sudo docker start -a sredlist



6) Inside the container:
sudo docker exec -it <container_id_or_name> echo "I'm inside the container!"
sudo docker exec -it sredlist /bin/bash

7) Delete Container:
    sudo docker rm <containerID>


8) View log sredlist API:

- sudo docker exec -it sredlist /bin/bash
- tail -f logs/plumber_122dc3c2.log

######################################################
(*) Note FIX: https://javahowtos.com/guides/124-docker/414-solved-cannot-kill-docker-container-permission-denied.html

It turned out that AppArmor service was messing up with Docker. AppArmor (or "Application Armor") is a Linux kernel security module that allows the system administrator to restrict programs' capabilities with per-program profiles. For this problem with containers, it helped me to remove the unknown from AppArmor using the following command:

sudo aa-remove-unknown