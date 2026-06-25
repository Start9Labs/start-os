# CLAUDE.md

Wire-protocol reference: [RPCSpec.md](RPCSpec.md).

## Operating rules

- **The runtime depends on the _built_ SDK at `../sdk/dist/`** (declared in `package.json` as `file:../sdk/dist`). Editing `sdk/` source alone has no effect on this package — rebuild the SDK (`cd ../sdk && make baseDist dist`) before running `npm run check` or `npm test` here.
- **Style: double quotes, no semicolons.** Prettier config in `package.json` differs from the SDK (`singleQuote: false` here vs `true` in `sdk/`). Don't "normalize" to single quotes.
