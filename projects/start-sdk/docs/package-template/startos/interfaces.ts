import { sdk } from './sdk'

// This package exposes no network interfaces, so it returns an empty list. To
// add one, bind a port with a host (e.g. `sdk.MultiHost.of(...)`), create an
// interface with `sdk.createInterface`, export it, and return its receipt(s)
// here. See start-technologies/projects/start-sdk/docs/src/interfaces.md.
export const setInterfaces = sdk.setupInterfaces(async () => [])
