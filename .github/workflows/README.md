# This folder contains GitHub Actions workflows for building the project

## backend
Runs: manually (on: workflow_dispatch) or called by product-pipeline (on: workflow_call)

This workflow uses the actions and docker/setup-buildx-action@v1 to prepare the environment for aarch64 cross complilation using docker buildx.
When execution of aarch64 containers is required the action docker/setup-qemu-action@v1 is added.
A matrix-strategy has been used to build for both x86_64 and aarch64 platforms in parallel.

### Running unittests

Unittests are run using [cargo-nextest]( https://nexte.st/). First the sources are (cross-)compiled and archived. The archive is then run on the correct platform.

## frontend
Runs: manually (on: workflow_dispatch) or called by product-pipeline (on: workflow_call)

This workflow builds the frontends.

## product
Runs: when a pull request targets the master or next branch and when a change to the master or next branch is made

This workflow builds everything, re-using the backend and frontend workflows.
The download and extraction order of artifacts is relevant to `make`, as it checks the file timestamps to decide which targets need to be executed.

Result: eos.img

## a note on uploading artifacts

Artifacts are used to share data between jobs. File permissions are not maintained during artifact upload. Where file permissions are relevant, the workaround using tar has been used. See (here)[https://github.com/actions/upload-artifact#maintaining-file-permissions-and-case-sensitive-files].