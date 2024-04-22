import { TriggerInput } from "./TriggerInput"
export { changeOnFirstSuccess } from "./changeOnFirstSuccess"
export { cooldownTrigger } from "./cooldownTrigger"

export type Trigger = (
  getInput: () => TriggerInput,
) => AsyncIterator<unknown, unknown, never>
