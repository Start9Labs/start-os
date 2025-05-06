The entire point of this ui is to configure the router. That requires lots of
complex logic to dig into the guts of OpenWRT and translate start9's "nice"
networking concepts into all kings of insane configuration options. Additionally,
users can use the Luci UI to change those under-the-hood options
directly. As such, we need to use the router's live configuration as the source
of truth, translating it back into start9's concepts to render the UI.

This module implements all that bidirectional logic. Each file corresponds roughly
to a page of the UI (whenever some other organization is not more reasonable to implement).
Each file also has:
- a state object
- a function to get the state
- a function to updating the state
- other CRUD-y operations

At the moment we are still deciding whether to code this in pure TS or have an
additional Rust backend to handle the control logic.
