import * as T from "../types"
import * as IST from "../actions/input/inputSpecTypes"
import { Action, ActionInfo } from "./setupActions"
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
type GetActionInputType<A extends ActionInfo<T.ActionId, any>> =
  A extends Action<T.ActionId, infer I> ? ExtractInputSpecType<I> : never

type TaskBase = {
  reason?: string
  replayId?: string
}
type TaskInput<T extends ActionInfo<T.ActionId, any>> = {
  kind: "partial"
  value: T.DeepPartial<GetActionInputType<T>>
}
export type TaskOptions<T extends ActionInfo<T.ActionId, any>> = TaskBase &
  (
    | {
        when?: Exclude<T.TaskTrigger, { condition: "input-not-matches" }>
        input?: TaskInput<T>
      }
    | {
        when: T.TaskTrigger & { condition: "input-not-matches" }
        input: TaskInput<T>
      }
  )

const _validate: T.Task = {} as TaskOptions<any> & {
  actionId: string
  packageId: string
  severity: T.TaskSeverity
}

export const createTask = <T extends ActionInfo<T.ActionId, any>>(options: {
  effects: T.Effects
  packageId: T.PackageId
  action: T
  severity: T.TaskSeverity
  options?: TaskOptions<T>
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
  return options.effects.action.createTask(req)
}
