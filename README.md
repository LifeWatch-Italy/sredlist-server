# Repository content
This repository includes the code used in the server side of the sRedList platform (https://sredlist.eu/#/home). The server side runs analyses in R and works with APIs thanks to R plumber (every time a user clicks on a button on the platform, it calls an API in R that is executed and returns a result). 

server.R is the main script that starts the plumber application and makes the R session ready to receive API requests. It loads libraries used on the platform, loads some files, and prepares the session (with error management, asynchronous calls management…). It calls API.R to set up some parameters for R plumber and error-management.R to load the different errors that can be returned on the platform.

redlist.R and sredlist.R are the scripts containing the code of the APIs. The redlist.R script includes APIs that call the Red List API to retrieve data, the sredlist.R script includes all the other APIs. 

APIs include functions created for the platform that are located in independent scripts to make APIs easier to read. sRLfun_FirstPage.R includes the functions used on the welcome page of the platform. sRLfun_Mapping.R includes the functions used to create distributions and to extract countries of occurrence. sRLfun_AOH.R includes the functions used in Area of Habitat analyses. sRLfun_OptionalAnalyses.R includes the functions used in Optional Analyses (fragmentation and trends in remote-sensed products). sRLfun_Outputs.R includes the functions used to create outputs in SIS Connect format. sRL_markdown_scripts folder includes scripts to create the final sRedList_report in RmarkDown. The scripts called Shiny_EditPts, Shiny_EditPoly, Shiny_Countries, and Shiny_DD (and associated function files) are used to create Shiny Apps called on the sRedList platform to respectively edit occurrence records, edit distribution polygons, edit the list of countries of occurrence, and prioritise the reassessment of DD species.

Several files needed to run the platform (e.g., map of countries, habitat crosswalks, empty output files…) are present in Species repository. The big files (distributions, global land cover maps…) are stored in the resources repository but are not published on GitHub as they are too big. Get in touch with us if you want to know more.



# sRedList outdated platform deployment
## sRedList-API
Plumber R application:
To run the server you must:
1)Install the following libraries in R:
- library (plumber)
- library (logger)
- library (glue)
- library (urltools)
- library (Rook)
- library (ggplot2)
- library (raster)
- library (plyr)
- library (dplyr)
- library (sf) 
- library (rgdal) 
- library (ncdf4)
- library (ggalluvial)
- library (rredlist) 
- library (tools)
- library (rgbif);
- library (CoordinateCleaner)
- library (rCAT); 
- library (maps) 
- library (countrycode)
- library (rnaturalearthdata)
- library (exactextractr)
- library (rasterVis)
- library (readr)

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

## Run sRedList-Platform
1) Run the following command to run the docker-compose of the sRedList project: (Client and server images must have been created previously) 
sudo docker compose -p sredlist -f sredlist.yml up --scale server=5 -d

--scale server=5 -> create 5 server service containers
