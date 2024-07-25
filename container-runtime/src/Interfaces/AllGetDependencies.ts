import { GetDependency } from "./GetDependency"
import { System } from "./System"
import { MakeMainEffects, MakeProcedureEffects } from "./MakeEffects"

export type AllGetDependencies = GetDependency<"system", Promise<System>> &
  GetDependency<"makeProcedureEffects", MakeProcedureEffects> &
  GetDependency<"makeMainEffects", MakeMainEffects>
