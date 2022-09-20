#
# Regular cron jobs for the embassy-os package
#
0 4	* * *	root	[ -x /usr/bin/embassy-os_maintenance ] && /usr/bin/embassy-os_maintenance
