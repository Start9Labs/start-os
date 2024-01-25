import { GetDependency } from "./GetDependency"
import { System } from "./System"
import { GetHostSystem, HostSystem } from "./HostSystem"

export type AllGetDependencies = GetDependency<"system", Promise<System>> &
  GetDependency<"hostSystem", GetHostSystem>
