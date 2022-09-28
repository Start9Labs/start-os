.DELETE_ON_ERROR:

all: binfmt.tar

binfmt.tar: Dockerfile
	DOCKER_CLI_EXPERIMENTAL=enabled docker buildx build --tag start9/x_system/binfmt --platform=linux/arm64 -o type=docker,dest=binfmt.tar .
