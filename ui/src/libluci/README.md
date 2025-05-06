This folder contains a fork of Luci's JS libraries, for use in startwrt.

Forked at 3ded930e96f9a06eaed9297ce169220f2693b57f (openwrt-23.05)

Because Luci is Apache licensed, we should document our changes here. Key modifications:
- modern JS classes
- fetch API
- modern imports
- type annotations

We are mainly interested in the uci.js bindings. At some point this whole library should be discarded and reimplemented in-house with only the features we actually use (TODO).

As of writing the type annotations are a work in progress. They were written largely by AI so are not entirely trustworthy.
