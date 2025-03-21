import { Trigger } from "./index"

export function changeOnFirstSuccess(o: {
  beforeFirstSuccess: Trigger
  afterFirstSuccess: Trigger
}): Trigger {
  return async function* (getInput) {
    let currentValue = getInput()
    while (!currentValue.lastResult) {
      yield
      currentValue = getInput()
    }
    const beforeFirstSuccess = o.beforeFirstSuccess(getInput)
    for (
      let res = await beforeFirstSuccess.next();
      currentValue?.lastResult !== "success" && !res.done;
      res = await beforeFirstSuccess.next()
    ) {
      yield
      currentValue = getInput()
    }
    const afterFirstSuccess = o.afterFirstSuccess(getInput)
    for (
      let res = await afterFirstSuccess.next();
      !res.done;
      res = await afterFirstSuccess.next()
    ) {
      yield
      currentValue = getInput()
    }
  }
}
