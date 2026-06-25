# Create Dynamic Daemons

Some services need a variable number of daemons based on user configuration — one per tunnel, one per website, one per connected node. The daemon chain is built at runtime from a config list.

## Solution

Read a variable-length list from a file model in `setupMain()`, then loop over entries to build the daemon chain with `.addDaemon()`. All dynamic daemons can share a single subcontainer image. The daemon ID must be unique per entry — derive it from the entry's data. An alternative approach generates dynamic config files (e.g., nginx server blocks) from the list and runs a single daemon serving all entries.

**Reference:** [Main](main.md) · [Actions](actions.md) · [File Models](file-models.md)

## Examples

See `startos/main.ts` in: [holesail](https://github.com/Start9Labs/holesail-startos) (one daemon per tunnel), [start9-pages](https://github.com/Start9Labs/start9-pages-startos) (dynamic nginx config per website)
