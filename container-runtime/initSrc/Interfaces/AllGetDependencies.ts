import { GetDependency } from "./GetDependency"
import { System } from "./System"
import { GetHostSystem, HostSystem } from "./HostSystem"

export type AllGetDependencies = GetDependency<"system", System> &
  GetDependency<"hostSystem", GetHostSystem>
