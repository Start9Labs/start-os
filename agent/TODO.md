* When adding ssh keys, don't add if identical one exists
* When adding ssh keys, check for newline at the end of the file. if not exists, add it.
* If `appmgr stop <ID>` throws no error, but completes without the app being stopped, we need to restart dockerd.
