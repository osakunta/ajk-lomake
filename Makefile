NAME   := osakunta/ajk-lomake
TAG    := $$(git log -1 --pretty=%h)
IMAGE  := ${NAME}:${TAG}
LATEST := ${NAME}:latest

docker-build:
	sudo docker build --build-arg EXECUTABLE=ajk-lomake-server -t ${IMAGE} .
	sudo docker tag ${IMAGE} ${LATEST}
 
docker-run:
	sudo docker run -it --publish 8080:8080 ${IMAGE}

docker-push:
	sudo docker push ${NAME}

ghcid :
	ghcid -c 'cabal new-repl'

run :
	cabal new-run ajk-lomake-server
