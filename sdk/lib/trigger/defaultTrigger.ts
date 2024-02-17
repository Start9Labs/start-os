import { cooldownTrigger } from "./cooldownTrigger"
import { changeOnFirstSuccess } from "./changeOnFirstSuccess"
import { successFailure } from "./successFailure"

export const defaultTrigger = successFailure({
  duringSuccess: cooldownTrigger(0),
  duringError: cooldownTrigger(30000),
})
