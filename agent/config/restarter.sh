#!/bin/bash

for CONTAINER in $(docker ps -aq); do
	EXIT=`docker inspect -f "{{ .State.ExitCode }}" $CONTAINER`
	if [ $EXIT -eq 0 ]; then
		continue
	fi
	if [ $EXIT -eq 143 ]; then
		continue
	fi
	if [ $EXIT -eq 137 ]; then
		OOM=`docker inspect -f "{{ .State.OOMKilled }}" $CONTAINER`
		if [ "$OOM" == "false" ]; then
			continue
		fi
	fi
	docker start $CONTAINER
done