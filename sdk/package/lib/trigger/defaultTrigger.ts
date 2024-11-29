import { cooldownTrigger } from "./cooldownTrigger"
import { changeOnFirstSuccess } from "./changeOnFirstSuccess"

export const defaultTrigger = changeOnFirstSuccess({
  beforeFirstSuccess: cooldownTrigger(1000),
  afterFirstSuccess: cooldownTrigger(30000),
})
