import { Effects } from "../Models/Effects"
import { T } from "@start9labs/start-sdk"
export type MakeProcedureEffects = (procedureId: string) => Effects
export type MakeMainEffects = () => T.MainEffects
