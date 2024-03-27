import { Checker } from "./emverLite/mod"

export class Dependency {
  constructor(
    readonly data:
      | {
          type: "running"
          versionSpec: Checker
          url: string
          healthChecks: string[]
        }
      | {
          type: "exists"
          versionSpec: Checker
          url: string
        },
  ) {}
}
