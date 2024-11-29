import * as T from "../types"
import * as IST from "../actions/input/inputSpecTypes"
import { Action } from "./setupActions"
import { ExtractInputSpecType } from "./input/builder/inputSpec"

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
type GetActionInputType<A extends Action<T.ActionId, any, any>> =
  A extends Action<T.ActionId, any, infer I> ? ExtractInputSpecType<I> : never

type ActionRequestBase = {
  reason?: string
  replayId?: string
}
type ActionRequestInput<T extends Action<T.ActionId, any, any>> = {
  kind: "partial"
  value: Partial<GetActionInputType<T>>
}
export type ActionRequestOptions<T extends Action<T.ActionId, any, any>> =
  ActionRequestBase &
    (
      | {
          when?: Exclude<
            T.ActionRequestTrigger,
            { condition: "input-not-matches" }
          >
          input?: ActionRequestInput<T>
        }
      | {
          when: T.ActionRequestTrigger & { condition: "input-not-matches" }
          input: ActionRequestInput<T>
        }
    )

const _validate: T.ActionRequest = {} as ActionRequestOptions<any> & {
  actionId: string
  packageId: string
  severity: T.ActionSeverity
}

export const requestAction = <T extends Action<T.ActionId, any, any>>(options: {
  effects: T.Effects
  packageId: T.PackageId
  action: T
  severity: T.ActionSeverity
  options?: ActionRequestOptions<T>
}) => {
  const request = options.options || {}
  const actionId = options.action.id
  const req = {
    ...request,
    actionId,
    packageId: options.packageId,
    action: undefined,
    severity: options.severity,
    replayId: request.replayId || `${options.packageId}:${actionId}`,
  }
  delete req.action
  return options.effects.action.request(req)
}
