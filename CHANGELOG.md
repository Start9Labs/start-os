# v0.3.3
## Highlights
    - x86_64 architecture compatibility
    - Kiosk mode - use your Embassy with monitor, keyboard, and mouse (available on x86 builds only, disabled on Raspberry Pi)
    - "Updates" tab - view all service updates from all registries in one place
    - Various UI/UX improvements
    - Various bugfixes and optimizations

## What's Changed
    - Minor typo fixes by @kn0wmad in #1887
    - Update build pipeline by @moerketh in #1896
    - Feature/setup migrate by @elvece in #1841
    - Feat/patch migration by @Blu-J in #1890
    - make js cancellable by @dr-bonez in #1901
    - wip: Making Injectable exec by @Blu-J in #1897
    - Fix/debug by @Blu-J in #1909
    - chore: Fix on the rsync not having stdout. by @Blu-J in #1911
    - install wizard project by @MattDHill in #1893
    - chore: Remove the duplicate loggging information that is making usele… by @Blu-J in #1912
    - Http proxy by @redragonx in #1772
    - fix(marketplace): loosen type in categories component by @waterplea in #1918
    - set custom meta title by @MattDHill in #1915
    - Feature/git hash by @dr-bonez in #1919
    - closes #1900 by @dr-bonez in #1920
    - feature/marketplace icons by @dr-bonez in #1921
    - Bugfix/0.3.3 migration by @dr-bonez in #1922
    - feat: Exposing the rsync that we have to the js by @Blu-J in #1907
    - Feature/install wizard disk info by @dr-bonez in #1923
    - bump shared and marketplace npm versions by @dr-bonez in #1924
    - fix error handling when store unreachable by @dr-bonez in #1925
    - wait for network online before launching init by @dr-bonez in #1930
    - silence service crash notifications by @dr-bonez in #1929
    - disable efi by @dr-bonez in #1931
    - Tor daemon fix by @redragonx in #1934
    - wait for url to be available before launching kiosk by @dr-bonez in #1933
    - fix migration to support portable fatties by @dr-bonez in #1935
    - Add guid to partition type by @MattDHill in #1932
    - add localhost support to the http server by @redragonx in #1939
    - refactor setup wizard by @dr-bonez in #1937
    - feat(shared): Ticker add new component and use it in marketplace by @waterplea in #1940
    - feat: For ota update using rsyncd by @Blu-J in #1938
    - Feat/update progress by @MattDHill in #1944
    - Fix/app show hidden by @MattDHill in #1948
    - create dpkg and iso workflows by @dr-bonez in #1941
    - changing ip addr type by @redragonx in #1950
    - Create mountpoints first by @k0gen in #1949
    - Hard code registry icons by @MattDHill in #1951
    - fix: Cleanup by sending a command and kill when dropped by @Blu-J in #1945
    - Update setup wizard styling by @elvece in #1954
    - Feature/homepage by @elvece in #1956
    - Fix millis by @Blu-J in #1960
    - fix accessing dev tools by @MattDHill in #1966
    - Update/misc UI fixes by @elvece in #1961
    - Embassy-init typo by @redragonx in #1959
    - feature: 0.3.2 -> 0.3.3 upgrade by @dr-bonez in #1958
    - Fix/migrate by @Blu-J in #1962
    - chore: Make validation reject containers by @Blu-J in #1970
    - get pubkey and encrypt password on login by @elvece in #1965
    - Multiple bugs and styling by @MattDHill in #1975
    - filter out usb stick during install by @dr-bonez in #1974
    - fix http upgrades by @dr-bonez in #1980
    - restore interfaces before creating manager by @dr-bonez in #1982
    - fuckit: no patch db locks by @dr-bonez in #1969
    - fix websocket hangup error by @dr-bonez in #1981
    - revert app show to use header and fix back button by @MattDHill in #1984
    - Update/marketplace info by @elvece in #1983
    - force docker image removal by @dr-bonez in #1985
    - do not error if cannot determine live usb device by @dr-bonez in #1986
    - remove community registry from FE defaults by @MattDHill in #1988
    - check environment by @dr-bonez in #1990
    - fix marketplace search and better category disabling by @MattDHill in #1991
    - better migration progress bar by @dr-bonez in #1993
    - bump cargo version by @dr-bonez in #1995
    - preload icons and pause on setup complete for kiosk mode by @MattDHill in #1997
    - use squashfs for rpi updates by @dr-bonez in #1998
    - do not start progress at 0 before diff complete by @dr-bonez in #1999
    - user must click continue in kiosk on success page by @MattDHill in #2001
    - fix regex in image rip script by @dr-bonez in #2002
    - fix bug with showing embassy drives and center error text by @MattDHill in #2006
    - fix partition type by @dr-bonez in #2007
    - lowercase service for alphabetic sorting by @MattDHill in #2008
    - dont add updates cat by @MattDHill in #2009
    - make downloaded page a full html doc by @MattDHill in #2011
    - wait for monitor to be attached before launching firefox by @chrisguida in #2005
    - UI fixes by @elvece in #2014
    - fix: Stop service before by @Blu-J in #2019
    - shield links update by @k0gen in #2018
    - fix: Undoing the breaking introduced by trying to stopp by @Blu-J in #2023
    - update link rename from embassy -> system by @elvece in #2027
    - initialize embassy before restoring packages by @dr-bonez in #2029
    - make procfs an optional dependency so sdk can build on macos by @elvece in #2028
    - take(1) for recover select by @MattDHill in #2030
    - take one from server info to prevent multiple reqs to registries by @MattDHill in #2032
    - remove write lock during backup by @MattDHill in #2033
    - fix: Ensure that during migration we make the urls have a trailing slash by @Blu-J in #2036
    - fix: Make the restores limited # restore at a time by @Blu-J in #2037
    - fix error and display of unknown font weight on success page by @elvece in #2038

## Checksums
```
8602e759d3ece7cf503b9ca43e8419109f14e424617c2703b3771c8801483d7e  embassyos_amd64.deb
b5c0d8d1af760881a1b5cf32bd7c5b1d1cf6468f6da594a1b4895a866d03a58c  embassyos_amd64.iso
fe518453a7e1a8d8c2be43223a1a12adff054468f8082df0560e1ec50df3dbfd  embassyos_raspberrypi.img
7b1ff0ada27b6714062aa991ec31c2d95ac4edf254cd464a4fa251905aa47ebd  embassyos_raspberrypi.tar.gz
```

# v0.3.2.1
## What's Changed
    - Update index.html copy and styling by @elvece in #1855
    - increase maximum avahi entry group size by @dr-bonez in #1869
    - bump version by @dr-bonez in #1871

### Linux and Mac

Download the `eos.tar.gz` file, then extract and flash the resulting eos.img to your SD Card
Windows

Download the `eos.zip` file, then extract and flash the resulting eos.img to your SD Card

## SHA-256 Checksums
```
c4b17658910dd10c37df134d5d5fdd6478f962ba1b803d24477d563d44430f96  eos.tar.gz
3a8b29878fe222a9d7cbf645c975b12805704b0f39c7daa46033d22380f9828c eos.zip
dedff3eb408ea411812b8f46e6c6ed32bfbd97f61ec2b85a6be40373c0528256  eos.img
```

# v0.3.2
## Highlights
    - Autoscrolling for logs
    - Improved connectivity between browser and Embassy
    - Switch to Postgres for EOS database for better performance
    - Multiple bug fixes and under-the-hood improvements
    - Various UI/UX enhancements
    - Removal of product keys

Update Hash (SHA256): `d8ce908b06baee6420b45be1119e5eb9341ba8df920d1e255f94d1ffb7cc4de9`

Image Hash (SHA256): `e035cd764e5ad9eb1c60e2f7bc3b9bd7248f42a91c69015c8a978a0f94b90bbb`

Note: This image was uploaded as a gzipped POSIX sparse TAR file. The recommended command for unpacking it on systems that support sparse files is `tar --format=posix --sparse -zxvf eos.tar.gz`

## What's Changed
    - formatting by @dr-bonez in #1698
    - Update README.md by @kn0wmad in #1705
    - Update README.md by @dr-bonez in #1703
    - feat: migrate to Angular 14 and RxJS 7 by @waterplea in #1681
    - 0312 multiple FE by @MattDHill in #1712
    - Fix http requests by @MattDHill in #1717
    - Add build-essential to README.md by @chrisguida in #1716
    - write image to sparse-aware archive format by @dr-bonez in #1709
    - fix: Add modification to the max_user_watches by @Blu-J in #1695
    - [Feat] follow logs by @chrisguida in #1714
    - Update README.md by @dr-bonez in #1728
    - fix build for patch-db client for consistency by @elvece in #1722
    - fix cli install by @chrisguida in #1720
    - highlight instructions if not viewed by @MattDHill in #1731
    - Feat: HttpReader by @redragonx in #1733
    - Bugfix/dns by @dr-bonez in #1741
    - add x86 build and run unittests to backend pipeline by @moerketh in #1682
    - [Fix] websocket connecting and patchDB connection monitoring by @MattDHill in #1738
    - Set pipeline job timeouts and add ca-certificates to test container by @moerketh in #1753
    - Disable bluetooth properly #862 by @redragonx in #1745
    - [feat]: resumable downloads by @dr-bonez in #1746
    - Fix/empty properties by @elvece in #1764
    - use hostname from patchDB as default server name by @MattDHill in #1758
    - switch to postgresql by @dr-bonez in #1763
    - remove product key from setup flow by @MattDHill in #1750
    - pinning cargo dep versions for CLI by @redragonx in #1775
    - fix: Js deep dir by @Blu-J in #1784
    - 0.3.2 final cleanup by @dr-bonez in #1782
    - expect ui marketplace to be undefined by @MattDHill in #1787
    - fix init to exit on failure by @dr-bonez in #1788
    - fix search to return more accurate results by @MattDHill in #1792
    - update backend dependencies by @dr-bonez in #1796
    - use base64 for HTTP headers by @dr-bonez in #1795
    - fix: Bad cert of *.local.local is now fixed to correct. by @Blu-J in #1798
    - fix duplicate patch updates, add scroll button to setup success by @MattDHill in #1800
    - level_slider reclaiming that precious RAM memory by @k0gen in #1799
    - stop leaking avahi clients by @dr-bonez in #1802
    - fix: Deep is_parent was wrong and could be escapped by @Blu-J in #1801
    - prevent cfg str generation from running forever by @dr-bonez in #1804
    - better RPC error message by @MattDHill in #1803
    - Bugfix/marketplace add by @elvece in #1805
    - fix mrketplace swtiching by @MattDHill in #1810
    - clean up code and logs by @MattDHill in #1809
    - fix: Minor fix that matt wanted by @Blu-J in #1808
    - onion replace instead of adding tor repository by @k0gen in #1813
    - bank Start as embassy hostname from the begining by @k0gen in #1814
    - add descriptions to marketplace list page by @elvece in #1812
    - Fix/encryption by @elvece in #1811
    - restructure initialization by @dr-bonez in #1816
    - update license by @MattDHill in #1819
    - perform system rebuild after updating by @dr-bonez in #1820
    - ignore file not found error for delete by @dr-bonez in #1822
    - Multiple by @MattDHill in #1823
    - Bugfix/correctly package backend job by @moerketh in #1826
    - update patch-db by @dr-bonez in #1831
    - give name to logs file by @MattDHill in #1833
    - play song during update by @dr-bonez in #1832
    - Seed patchdb UI data by @elvece in #1835
    - update patch db and enable logging by @dr-bonez in #1837
    - reduce patch-db log level to warn by @dr-bonez in #1840
    - update ts matches to fix properties ordering bug by @elvece in #1843
    - handle multiple image tags having the same hash and increase timeout by @dr-bonez in #1844
    - retry pgloader up to 5x by @dr-bonez in #1845
    - show connection bar right away by @MattDHill in #1849
    - dizzy Rebranding to embassyOS by @k0gen in #1851
    - update patch db by @MattDHill in #1852
    - camera_flash screenshots update by @k0gen in #1853
    - disable concurrency and delete tmpdir before retry by @dr-bonez in #1846

## New Contributors

	- @redragonx made their first contribution in #1733


# v0.3.1.1
## What's Changed

    - whale2 docker stats fix by @k0gen in #1630
    - update backend dependencies by @dr-bonez in #1637
    - Fix/receipts health by @Blu-J in #1616
    - return correct error on failed os download by @dr-bonez in #1636
    - fix build by @dr-bonez in #1639
    - Update product.yaml by @dr-bonez in #1638
    - handle case where selected union enum is invalid after migration by @MattDHill in #1658
    - fix: Resolve fighting with NM by @Blu-J in #1660
    - sdk: don't allow mounts in inject actions by @chrisguida in #1653
    - feat: Variable args by @Blu-J in #1667
    - add readme to system-images folder by @elvece in #1665
    - Mask chars beyond 16 by @MattDHill in #1666
    - chore: Update to have the new version 0.3.1.1 by @Blu-J in #1668
    - feat: Make the rename effect by @Blu-J in #1669
    - fix migration, add logging by @dr-bonez in #1674
    - run build checks only when relevant FE changes by @elvece in #1664
    - trust local ca by @dr-bonez in #1670
    - lower log level for docker deser fallback message by @dr-bonez in #1672
    - refactor build process by @dr-bonez in #1675
    - chore: enable strict mode by @waterplea in #1569
    - draft releases notes for 0311 by @MattDHill in #1677
    - add standby mode by @dr-bonez in #1671
    - feat: atomic writing by @Blu-J in #1673
    - allow server.update to update to current version by @dr-bonez in #1679
    - allow falsey rpc response by @dr-bonez in #1680
    - issue notification when individual package restore fails by @dr-bonez in #1685
    - replace bang with question mark in html by @MattDHill in #1683
    - only validate mounts for inject if eos >=0.3.1.1 by @dr-bonez in #1686
    - add marketplace_url to backup metadata for service by @dr-bonez in #1688
    - marketplace published at for service by @MattDHill in #1689
    - sync data to fs before shutdown by @dr-bonez in #1690
    - messaging for restart, shutdown, rebuild by @MattDHill in #1691
    - honor shutdown from diagnostic ui by @dr-bonez in #1692
    - ask for sudo password immediately during make by @dr-bonez in #1693
    - sync blockdev after update by @dr-bonez in #1694
    - set Matt as default assignee by @MattDHill in #1697
    - NO_KEY for CI images by @dr-bonez in #1700
    - fix typo by @dr-bonez in #1702

# v0.3.1
## What's Changed
    - Feat bulk locking by @Blu-J in #1422
    - Switching SSH keys to start9 user by @k0gen in #1321
    - chore: Convert from ajv to ts-matches by @Blu-J in #1415
    - Fix/id params by @elvece in #1414
    - make nicer update sound by @ProofOfKeags in #1438
    - adds product key to error message in setup flow when there is mismatch by @dr-bonez in #1436
    - Update README.md to include yq by @cryptodread in #1385
    - yin_yang For the peace of mind yin_yang by @k0gen in #1444
    - Feature/update sound by @ProofOfKeags in #1439
    - Feature/script packing by @ProofOfKeags in #1435
    - rename ActionImplementation to PackageProcedure by @dr-bonez in #1448
    - Chore/warning cleanse by @ProofOfKeags in #1447
    - refactor packing to async by @ProofOfKeags in #1453
    - Add nginx config for proxy redirect by @yzernik in #1421
    - Proxy local frontend to remote backend by @elvece in #1452
    - Feat/js action by @Blu-J in #1437
    - Fix/making js work by @Blu-J in #1456
    - fix: Dependency vs dependents by @Blu-J in #1462
    - refactor: isolate network toast and login redirect to separate services by @waterplea in #1412
    - Fix links in CONTRIBUTING.md, update ToC by @BBlackwo in #1463
    - Feature/require script consistency by @ProofOfKeags in #1451
    - Chore/version 0 3 1 0 by @Blu-J in #1475
    - remove interactive TTY requirement from scripts by @moerketh in #1469
    - Disable view in marketplace button when side-loaded by @BBlackwo in #1471
    - Link to tor address on LAN setup page (#1277) by @BBlackwo in #1466
    - UI version updates and welcome message for 0.3.1 by @elvece in #1479
    - Update contribution and frontend readme by @BBlackwo in #1467
    - Clean up config by @MattDHill in #1484
    - Enable Control Groups for Docker containers by @k0gen in #1468
    - Fix/patch db unwrap remove by @Blu-J in #1481
    - handles spaces in working dir in make-image.sh by @moerketh in #1487
    - UI cosmetic improvements by @MattDHill in #1486
    - chore: fix the master by @Blu-J in #1495
    - generate unique ca names based off of server id by @ProofOfKeags in #1500
    - allow embassy-cli not as root by @dr-bonez in #1501
    - fix: potential fix for the docker leaking the errors and such by @Blu-J in #1496
    - Fix/memory leak docker by @Blu-J in #1505
    - fixes serialization of regex pattern + description by @ProofOfKeags in #1509
    - allow interactive TTY if available by @dr-bonez in #1508
    - fix "missing proxy" error in embassy-cli by @dr-bonez in #1516
    - Feat/js known errors by @Blu-J in #1514
    - fixes a bug where nginx will crash if eos goes into diagnostic mode a… by @dr-bonez in #1506
    - fix: restart/ uninstall sometimes didn't work by @Blu-J in #1527
    - add "error_for_status" to static file downloads by @dr-bonez in #1532
    - fixes #1169 by @dr-bonez in #1533
    - disable unnecessary services by @dr-bonez in #1535
    - chore: Update types to match embassyd by @Blu-J in #1539
    - fix: found a unsaturaded args fix by @Blu-J in #1540
    - chore: Update the lite types to include the union and enum by @Blu-J in #1542
    - Feat: Make the js check for health by @Blu-J in #1543
    - fix incorrect error message for deserialization in ValueSpecString by @dr-bonez in #1547
    - fix dependency/dependent id issue by @dr-bonez in #1546
    - add textarea to ValueSpecString by @dr-bonez in #1534
    - Feat/js metadata by @Blu-J in #1548
    - feat: uid/gid/mode added to metadata by @Blu-J in #1551
    - Strict null checks by @waterplea in #1464
    - fix backend builds for safe git config by @elvece in #1549
    - update should send version not version spec by @elvece in #1559
    - chore: Add tracing for debuging the js procedure slowness by @Blu-J in #1552
    - Reset password through setup wizard by @MattDHill in #1490
    - feat: Make sdk by @Blu-J in #1564
    - fix: Missing a feature flat cfg by @Blu-J in #1563
    - fixed sentence that didn't make sense by @BitcoinMechanic in #1565
    - refactor(patch-db): use PatchDB class declaratively by @waterplea in #1562
    - fix bugs with config and clean up dev options by @MattDHill in #1558
    - fix: Make it so we only need the password on the backup by @Blu-J in #1566
    - kill all sessions and remove ripple effect by @MattDHill in #1567
    - adjust service marketplace button for installation source relevance by @elvece in #1571
    - fix connection failure display monitoring and other style changes by @MattDHill in #1573
    - add dns server to embassy-os by @dr-bonez in #1572
    - Fix/mask generic inputs by @elvece in #1570
    - Fix/sideload icon type by @elvece in #1577
    - add avahi conditional compilation flags to dns by @dr-bonez in #1579
    - selective backups and better drive selection interface by @MattDHill in #1576
    - Feat/use modern tor by @kn0wmad in #1575
    - update welcome notes for 031 by @MattDHill in #1580
    - fix: Properties had a null description by @Blu-J in #1581
    - fix backup lock ordering by @dr-bonez in #1582
    - Bugfix/backup lock order by @dr-bonez in #1583
    - preload redacted and visibility hidden by @MattDHill in #1584
    - turn chevron red in config if error by @MattDHill in #1586
    - switch to utc by @dr-bonez in #1587
    - update patchdb for array patch fix by @elvece in #1588
    - filter package ids when backing up by @dr-bonez in #1589
    - add select/deselect all to backups and enum lists by @elvece in #1590
    - fix: Stop the buffer from dropped pre-maturly by @Blu-J in #1591
    - chore: commit the snapshots by @Blu-J in #1592
    - nest new entries and message updates better by @MattDHill in #1595
    - fix html parsing in logs by @elvece in #1598
    - don't crash service if io-format is set for main by @dr-bonez in #1599
    - strip html from colors from logs by @elvece in #1604
    - feat: fetch effect by @Blu-J in #1605
    - Fix/UI misc by @elvece in #1606
    - display bottom item in backup list and refactor for cleanliness by @MattDHill in #1609

# v0.3.0.3
## What's Changed
	- refactor: decompose app component by @waterplea in #1359
	- Update Makefile by @kn0wmad in #1400
	- ⬐ smarter wget by @k0gen in #1401
	- prevent the kernel from OOMKilling embassyd by @dr-bonez in #1402
	- attempt to heal when health check passes by @dr-bonez in #1420
	- Feat new locking by @Blu-J in #1384
	- version bump by @dr-bonez in #1423
	- Update server-show.page.ts by @chrisguida in #1424
	- Bump async from 2.6.3 to 2.6.4 in /frontend by @dependabot in #1426
	- Update index.html by @mirkoRainer in #1419

## New Contributors
    - @dependabot made their first contribution in #1426
    - @mirkoRainer made their first contribution in #1419

# v0.3.0.2
- Minor compatibility fixes
  - #1392
  - #1390
  - #1388

# v0.3.0.1
Minor bugfixes and performance improvements

# v0.3.0
- Websockets
	- Real-time sync
- Patch DB
	- Closely mirror FE and BE state. Most operating systems are connected to their GUI. Here it is served over the web. Patch DB and websockets serve to close the perceptual gap of this inherent challenge.
- Switch kernel from Raspbian to Ubuntu
	- 64 bit
	- Possibility for alternative hardware
- Merging of lifeline, agent, and appmgr into embassyd
	- Elimination of Haskell in favor of pure Rust
	- Unified API for interacting with the OS
	- Easier to build from source
- OS (quarantined from OS and service data)
	- Kernel/boot
	- Persistent metadata (disk guid, product key)
	- Rootfs (the os)
	- Reserved (for updates) - swaps with rootfs
- Revamped OS updates
	- Progress indicators
	- Non-blocking
	- Simple swap on reboot
- Revamped setup flow
	- Elimination of Setup App (Apple/Google dependencies gone)
	- Setup Wizard on http://embassy.local
- Revamped service config
	- Dynamic, validated forms
- Diagnostic UI
	- Missing disk, wrong disk, corrupt disk
- Turing complete API for actions, backup/restore, config, properties, notifications, health checks, and dependency requirements
- Optional, arbitrary inputs for actions
- Install, update, recover progress for apps
- Multiple interfaces
	- E.g. rpc, p2p, ui
- Health checks
	- Developer defined
	- Internal, dependencies, and/or external
- Full Embassy backup (diff-based)
- External drive support/requirement
	- Single at first
	- Groundwork for extension and mirror drives
- Disk encryption
	- Random key encrypted with static value
	- Groundwork for swapping static value with chosen password
- Session Management
	- List all active sessions
	- Option to kill
- More robust and extensive logs
- Donations
