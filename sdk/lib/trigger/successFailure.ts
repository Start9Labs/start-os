import { Trigger } from "."
import { lastStatus } from "./lastStatus"

export const successFailure = (o: {
  duringSuccess: Trigger
  duringError: Trigger
}) => lastStatus({ success: o.duringSuccess, default: o.duringError })
