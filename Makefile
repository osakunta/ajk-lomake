NAME   := osakunta/ajk-lomake
TAG    := $$(git log -1 --pretty=%h)
IMAGE  := ${NAME}:${TAG}
LATEST := ${NAME}:latest
EXEC   := ajk-lomake-server

docker-build:
	sudo docker build --build-arg EXECUTABLE=${EXEC} -t ${IMAGE} .
	sudo docker tag ${IMAGE} ${LATEST}
 
docker-run:
	sudo docker rm ${EXEC} && sudo docker run -it --name ${EXEC} --publish 8080:8080 ${LATEST}

docker-push:
	sudo docker push ${NAME}

docker-login:
	gcloud auth print-access-token | sudo docker login -u oauth2accesstoken --password-stdin https://gcr.io

docker-bash:
	sudo docker exec -it ${EXEC} /bin/bash

deploy:
	gcloud beta run deploy ajk-lomake --image gcr.io/saation-palvelut/ajk-lomake --region us-central1

release: docker-build docker-login docker-push deploy

ghcid :
	ghcid -c 'cabal new-repl'

run :
	cabal new-run ajk-lomake-server
