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
        prev: prev || undefined,
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
export type ActionRequest<T extends T.ActionRequest & { replayId?: string }> =
  T extends { when: { condition: "input-not-matches" } }
    ? (T extends { input: T.ActionRequestInput } ? T : "input is required for condition 'input-not-matches'")
    : T

export const requestAction = <
  T extends T.ActionRequest & { replayId?: string },
>(options: {
  effects: T.Effects
  request: ActionRequest<T>
}) => {
  const request = options.request as T
  const req = {
    ...request,
    replayId: request.replayId || request.id,
  }
  return options.effects.action.request(req)
}
