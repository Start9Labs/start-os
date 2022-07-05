# This folder contains GitHub Actions workflows for building the project

## backend-pr
Runs: when a pull request targets the master branch and changes the libs/ and/or backend/ folders

This workflow uses the actions docker/setup-qemu-action@v1 and docker/setup-buildx-action@v1 to prepare the environment for aarch64 cross complilation using docker buildx.
A matrix-strategy has been used for building the v8 snapshot instead of the makefile to allow parallel job execution.

## frontend-pr
Runs: when a pull request targets the master branch and changes the frontend/ folder

This workflow builds the frontends.

## product
Runs: when a change to the master branch is made

This workflow builds everything, re-using the backend-pr and frontend-pr workflows.
The download and extraction order of artifacts is relevant to `make`, as it checks the file timestamps to decide which targets need to be executed.

Result: eos.img

## a note on uploading artifacts

Artifacts are used to share data between jobs. File permissions are not maintained during artifact upload. Where file permissions are relevant, the workaround using tar has been used. See (here)[https://github.com/actions/upload-artifact#maintaining-file-permissions-and-case-sensitive-files].