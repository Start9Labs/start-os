# `patch-db` Client

This project contains the client modules necessary for any front end utilizing `patch-db`.

## Build steps

First, install `node_modules` locally to project. From the project root, run:

```
npm --prefix . install
```

Next, build source from the project directory:

```
npm --prefix . run build
```

This client is consumed as a file dependency within the start-technologies monorepo
(`patch-db-client` in the root `package.json`, pointing at
`shared-libs/crates/patch-db/client`). To rebuild it and integrate it into the
Angular workspace rooted at the repo root, run the following from the repository
root:

```
npm run build:deps
```

This builds the patch-db client along with the other file dependencies (e.g.
`@start9labs/start-sdk`) used by the Angular project.
