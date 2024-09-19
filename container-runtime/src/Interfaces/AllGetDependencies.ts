import { GetDependency } from "./GetDependency"
import { System } from "./System"
import { MakeMainEffects, MakeProcedureEffects } from "./MakeEffects"
import { CallbackHolder } from "../Models/CallbackHolder"

export type AllGetDependencies = GetDependency<"system", Promise<System>> &
  GetDependency<
    "makeProcedureEffects",
    (callbacks: CallbackHolder) => MakeProcedureEffects
  > &
  GetDependency<
    "makeMainEffects",
    (callbacks: CallbackHolder) => MakeMainEffects
  >
