# Enforce Settings on a Dependency

Sometimes your service requires specific configuration on a dependency — Bitcoin Core must have `txindex=true`, or ZMQ must be enabled. A cross-service task fires on the dependency whenever its config drifts from the required values.

## Solution

In `setupDependencies()`, call `sdk.action.createTask()` targeting the dependency's autoconfig action (imported from the dependency's package). Pass `input: { kind: 'partial', value: { ... } }` with the required field values, and `when: { condition: 'input-not-matches', once: false }` so the task re-fires whenever the dependency's config drifts. The autoconfig action must be exported by the dependency and added to your `package.json` dependencies.

**Reference:** [Dependencies](dependencies.md) · [Tasks](tasks.md)

## Examples

See `startos/dependencies.ts` in: [fulcrum](https://github.com/Start9Labs/fulcrum-startos) (txindex + ZMQ on Bitcoin Core), [public-pool](https://github.com/Start9Labs/public-pool-startos) (ZMQ on Bitcoin Core)
