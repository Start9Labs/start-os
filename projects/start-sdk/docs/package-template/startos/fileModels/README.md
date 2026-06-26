Use the `/fileModels` directory to create separate `.ts` files to represent underlying files used by you package. The exported `FileModels` afford a convenient and type safe way to read amd write to the underlying files, as well as react to changes.

Supported file formats are `.yaml`, `.toml`, `.json`, `.env`, `.ini`, and `.txt`. For alternative file formats, you can use the `raw` method and provide custom serialization and parser functions.

It is common for packages to use a `store.json.ts` FileModel as a convenient place to persist arbitrary data that are needed by the package but are _not_ persisted by the upstream service. For example, you might use store.json to persist startup flags or login credentials.
