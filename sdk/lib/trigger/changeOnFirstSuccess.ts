import { Trigger } from "./index"

export function changeOnFirstSuccess(o: {
  beforeFirstSuccess: Trigger
  afterFirstSuccess: Trigger
}): Trigger {
  return async function* (getInput) {
    const beforeFirstSuccess = o.beforeFirstSuccess(getInput)
    yield
    let currentValue = getInput()
    beforeFirstSuccess.next()
    for (
      let res = await beforeFirstSuccess.next();
      currentValue?.lastResult !== "passing" && !res.done;
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
