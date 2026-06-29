xfstests: xfstests.sh
	docker build -t backup-fs:xfstests -f xfstests.Dockerfile .
	# Additional permissions are needed to be able to mount FUSE
	docker run --rm -it --cap-add SYS_ADMIN --cap-add IPC_OWNER --device /dev/fuse --security-opt apparmor:unconfined \
	 --memory=2g --kernel-memory=200m \
	 -v "$(shell pwd)/logs:/code/logs" \
	 -v "$(shell pwd)/backup-fs:/code/backup-fs" backup-fs:xfstests bash -c "cd /code/backup-fs && ./xfstests.sh"