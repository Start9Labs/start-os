import { VersionRange } from "../exver"

export class Dependency {
  constructor(
    readonly data:
      | {
          /** Either "running" or "exists". Does the dependency need to be running, or does it only need to exist? */
          type: "running"
          /** The acceptable version range of the dependency. */
          versionRange: VersionRange
          /** A list of the dependency's health check IDs that must be passing for the service to be satisfied. */
          healthChecks: string[]
        }
      | {
          /** Either "running" or "exists". Does the dependency need to be running, or does it only need to exist? */
          type: "exists"
          /** The acceptable version range of the dependency. */
          versionRange: VersionRange
        },
  ) {}
}
