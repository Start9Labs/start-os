import { Trigger } from "."

export function successFailure(o: {
  duringSuccess: Trigger
  duringError: Trigger
}): Trigger {
  return async function* (getInput) {
    while (true) {
      const beforeSuccess = o.duringSuccess(getInput)
      yield
      let currentValue = getInput()
      beforeSuccess.next()
      for (
        let res = await beforeSuccess.next();
        currentValue?.lastResult !== "passing" && !res.done;
        res = await beforeSuccess.next()
      ) {
        yield
        currentValue = getInput()
      }
      const duringError = o.duringError(getInput)
      for (
        let res = await duringError.next();
        currentValue?.lastResult === "passing" && !res.done;
        res = await duringError.next()
      ) {
        yield
        currentValue = getInput()
      }
    }
  }
}
