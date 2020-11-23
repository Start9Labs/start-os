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

spec :: Spec
spec = do
    describe "parsing app manifest ports" $ do
        it "should yield true for cups 0.2.3" $ do
            res <- decodeThrow @IO @(AppManifest 0) cups023Manifest
            uiAvailable res `shouldBe` True
        it "should yield false for cups 0.2.3 Mod" $ do
            res <- decodeThrow @IO @(AppManifest 0) cups023ManifestModNoUI
            uiAvailable res `shouldBe` False
