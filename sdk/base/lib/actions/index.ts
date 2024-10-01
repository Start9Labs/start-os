import * as T from "../types"
import * as IST from "../actions/input/inputSpecTypes"

export type RunActionInput<Input> =
  | Input
  | ((prev?: { spec: IST.InputSpec; value: Input | null }) => Input)

export const runAction = async <
  Input extends Record<string, unknown>,
>(options: {
  effects: T.Effects
  //   packageId?: T.PackageId
  actionId: T.ActionId
  input?: RunActionInput<Input>
}) => {
  if (options.input) {
    if (options.input instanceof Function) {
      const prev = await options.effects.action.getInput({
        // packageId: options.packageId,
        actionId: options.actionId,
      })
      const input = options.input(
        prev
          ? { spec: prev.spec as IST.InputSpec, value: prev.value as Input }
          : undefined,
      )
      return options.effects.action.run({
        // packageId: options.packageId,
        actionId: options.actionId,
        input,
      })
    } else {
      return options.effects.action.run({
        // packageId: options.packageId,
        actionId: options.actionId,
        input: options.input,
      })
    }
  } else {
    return options.effects.action.run({
      //   packageId: options.packageId,
      actionId: options.actionId,
    })
  }
}

// prettier-ignore
export type ActionRequest<T extends Omit<T.ActionRequest, "packageId">> =
  T extends { when: { condition: "input-not-matches" } }
    ? (T extends { input: T.ActionRequestInput } ? T : "input is required for condition 'input-not-matches'")
    : T

export const requestAction = <
  T extends Omit<T.ActionRequest, "packageId">,
>(options: {
  effects: T.Effects
  request: ActionRequest<T> & { replayId?: string; packageId: T.PackageId }
}) => {
  const request = options.request
  const req = {
    ...request,
    replayId: request.replayId || `${request.packageId}:${request.actionId}`,
  }
  return options.effects.action.request(req)
}
