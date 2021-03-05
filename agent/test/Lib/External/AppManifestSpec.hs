{-# LANGUAGE QuasiQuotes #-}
module Lib.External.AppManifestSpec where

import           Startlude

import           Test.Hspec

import           Data.String.Interpolate.IsString
import           Data.Yaml

import           Lib.External.AppManifest

cups023Manifest :: ByteString
cups023Manifest = [i|
---
compat: v0
id: cups
version: 0.2.3
title: Cups
description:
  short: Peer-to-Peer Encrypted Messaging
  long: A peer-to-peer encrypted messaging platform that operates over tor.
release-notes: fix autofill for password field
ports:
  - internal: 59001
    tor: 59001
  - internal: 80
    tor: 80
image:
  type: tar
mount: /root
assets:
  - src: httpd.conf
    dst: "."
    overwrite: true
  - src: www
    dst: "."
    overwrite: true
hidden-service-version: v3
|]

cups023ManifestModNoUI :: ByteString
cups023ManifestModNoUI = [i|
---
compat: v0
id: cups
version: 0.2.3
title: Cups
description:
  short: Peer-to-Peer Encrypted Messaging
  long: A peer-to-peer encrypted messaging platform that operates over tor.
release-notes: fix autofill for password field
ports:
  - internal: 59001
    tor: 59001
image:
  type: tar
mount: /root
assets:
  - src: httpd.conf
    dst: "."
    overwrite: true
  - src: www
    dst: "."
    overwrite: true
hidden-service-version: v3
|]

mastodon330Manifest :: ByteString
mastodon330Manifest = [i|
---
id: mastodon
version: 3.3.0.1
title: Mastodon
description:
  short: "A free, open-source social network server."
  long: "Mastodon is a free, open-source social network server based on ActivityPub where users can follow friends and discover new ones. On Mastodon, users can publish anything they want: links, pictures, text, video. All Mastodon servers are interoperable as a federated network (users on one server can seamlessly communicate with users from another one, including non-Mastodon software that implements ActivityPub)!"
release-notes: Added an acation to reset the admin password
install-alert: "After starting mastodon for the first time, it can take a long time (several minutes) to be ready.\nPlease be patient. On future starts of the service, it will be faster, but still takes longer than other services.\nMake sure to sign up for a user before giving out your link. The first user to sign up is set as the admin user.\n"
uninstall-alert: ~
restore-alert: ~
start-alert: "It may take several minutes after startup for this service to be ready for use.\n"
has-instructions: true
os-version-required: ">=0.2.8"
os-version-recommended: ">=0.2.8"
ports:
  - internal: 80
    tor: 80
    lan: standard
  - internal: 443
    tor: 443
    lan:
      custom:
        port: 443
  - internal: 3000
    tor: 3000
    lan: ~
  - internal: 4000
    tor: 4000
    lan: ~
image:
  type: tar
shm-size-mb: ~
mount: /root/persistence
public: ~
shared: ~
assets: []
hidden-service-version: v3
dependencies: {}
actions:
  - id: reset-admin-password
    name: Reset Admin Password
    description: This action will reset your admin password to a random value
    allowed-statuses:
      - RUNNING
    command:
      - docker_entrypoint.sh
      - reset_admin_password.sh
|]


spec :: Spec
spec = do
  describe "parsing app manifest ports" $ do
    it "should parse mastodon 3.3.0" $ do
      res <- decodeThrow @IO @AppManifest mastodon330Manifest
      print res
      lanUiAvailable res `shouldBe` True
      torUiAvailable res `shouldBe` True

