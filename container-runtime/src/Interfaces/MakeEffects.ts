import { Effects } from "../Models/Effects"
import { MainEffects } from "@start9labs/start-sdk/cjs/lib/StartSdk"
export type MakeProcedureEffects = (procedureId: string) => Effects
export type MakeMainEffects = () => MainEffects
