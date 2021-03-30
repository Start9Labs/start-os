# appmgr

# Instructions

Install the portable version of appmgr

`cargo install --path=. --features=portable`

On Mac, also add 

`--no-default-features`

## Exit Codes
1. General Error
2. File System IO Error
3. Docker Error
4. Config Spec violation
5. Config Rules violation
6. Requested value does not exist
7. Invalid Backup Password