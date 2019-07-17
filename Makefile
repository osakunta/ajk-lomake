NAME   := osakunta/ajk-lomake
TAG    := $$(git log -1 --pretty=%h)
IMAGE  := ${NAME}:${TAG}
LATEST := ${NAME}:latest
EXEC   := ajk-lomake-server

docker-build:
	sudo docker build --build-arg EXECUTABLE=${EXEC} -t ${IMAGE} .
	sudo docker tag ${IMAGE} ${LATEST}
 
docker-run:
	sudo docker run -it --name ${EXEC} --publish 8080:8080 ${LATEST}

docker-rm:
	sudo docker rm ${EXEC}

docker-push:
	sudo docker push ${NAME}

docker-bash:
	sudo docker exec -it ${EXEC} /bin/bash

ghcid :
	ghcid -c 'cabal new-repl'

run :
	cabal new-run ajk-lomake-server
