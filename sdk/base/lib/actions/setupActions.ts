import { InputSpec } from "./input/builder"
import { ExtractInputSpecType } from "./input/builder/inputSpec"
import * as T from "../types"
import { once } from "../util"
import { InitScript } from "../inits"
import { Parser } from "ts-matches"

type MaybeInputSpec<Type> = {} extends Type ? null : InputSpec<Type>
export type Run<A extends Record<string, any>> = (options: {
  effects: T.Effects
  input: A
  spec: T.inputSpecTypes.InputSpec
}) => Promise<(T.ActionResult & { version: "1" }) | null | void | undefined>
export type GetInput<A extends Record<string, any>> = (options: {
  effects: T.Effects
}) => Promise<null | void | undefined | T.DeepPartial<A>>

export type MaybeFn<T> = T | ((options: { effects: T.Effects }) => Promise<T>)
function callMaybeFn<T>(
  maybeFn: MaybeFn<T>,
  options: { effects: T.Effects },
): Promise<T> {
  if (maybeFn instanceof Function) {
    return maybeFn(options)
  } else {
    return Promise.resolve(maybeFn)
  }
}
function mapMaybeFn<T, U>(
  maybeFn: MaybeFn<T>,
  map: (value: T) => U,
): MaybeFn<U> {
  if (maybeFn instanceof Function) {
    return async (...args) => map(await maybeFn(...args))
  } else {
    return map(maybeFn)
  }
}

export interface ActionInfo<
  Id extends T.ActionId,
  Type extends Record<string, any>,
> {
  readonly id: Id
  readonly _INPUT: Type
}

export class Action<Id extends T.ActionId, Type extends Record<string, any>>
  implements ActionInfo<Id, Type>
{
  readonly _INPUT: Type = null as any as Type
  private prevInputSpec: Record<
    string,
    { spec: T.inputSpecTypes.InputSpec; validator: Parser<unknown, Type> }
  > = {}
  private constructor(
    readonly id: Id,
    private readonly metadataFn: MaybeFn<T.ActionMetadata>,
    private readonly inputSpec: MaybeInputSpec<Type>,
    private readonly getInputFn: GetInput<Type>,
    private readonly runFn: Run<Type>,
  ) {}
  static withInput<
    Id extends T.ActionId,
    InputSpecType extends InputSpec<Record<string, any>>,
  >(
    id: Id,
    metadata: MaybeFn<Omit<T.ActionMetadata, "hasInput">>,
    inputSpec: InputSpecType,
    getInput: GetInput<ExtractInputSpecType<InputSpecType>>,
    run: Run<ExtractInputSpecType<InputSpecType>>,
  ): Action<Id, ExtractInputSpecType<InputSpecType>> {
    return new Action<Id, ExtractInputSpecType<InputSpecType>>(
      id,
      mapMaybeFn(metadata, (m) => ({ ...m, hasInput: true })),
      inputSpec as any,
      getInput,
      run,
    )
  }
  static withoutInput<Id extends T.ActionId>(
    id: Id,
    metadata: MaybeFn<Omit<T.ActionMetadata, "hasInput">>,
    run: Run<{}>,
  ): Action<Id, {}> {
    return new Action(
      id,
      mapMaybeFn(metadata, (m) => ({ ...m, hasInput: false })),
      null,
      async () => null,
      run,
    )
  }
  async exportMetadata(options: {
    effects: T.Effects
  }): Promise<T.ActionMetadata> {
    const childEffects = options.effects.child(`setupActions/${this.id}`)
    childEffects.constRetry = once(() => {
      this.exportMetadata(options)
    })
    const metadata = await callMaybeFn(this.metadataFn, {
      effects: childEffects,
    })
    await options.effects.action.export({ id: this.id, metadata })
    return metadata
  }
  async getInput(options: { effects: T.Effects }): Promise<T.ActionInput> {
    let spec = {}
    if (this.inputSpec) {
      const built = await this.inputSpec.build(options)
      this.prevInputSpec[options.effects.eventId!] = built
      spec = built.spec
    }
    return {
      eventId: options.effects.eventId!,
      spec,
      value:
        ((await this.getInputFn(options)) as
          | Record<string, unknown>
          | null
          | undefined) || null,
    }
  }
  async run(options: {
    effects: T.Effects
    input: Type
  }): Promise<T.ActionResult | null> {
    let spec = {}
    if (this.inputSpec) {
      const prevInputSpec = this.prevInputSpec[options.effects.eventId!]
      if (!prevInputSpec) {
        throw new Error(
          `getActionInput has not been called for EventID ${options.effects.eventId}`,
        )
      }
      options.input = prevInputSpec.validator.unsafeCast(options.input)
      spec = prevInputSpec.spec
    }
    return (
      (await this.runFn({
        effects: options.effects,
        input: options.input,
        spec,
      })) ?? null
    )
  }
}

export class Actions<
  AllActions extends Record<T.ActionId, Action<T.ActionId, any>>,
> implements InitScript
{
  private constructor(private readonly actions: AllActions) {}
  static of(): Actions<{}> {
    return new Actions({})
  }
  addAction<A extends Action<T.ActionId, any>>(
    action: A, // TODO: prevent duplicates
  ): Actions<AllActions & { [id in A["id"]]: A }> {
    return new Actions({ ...this.actions, [action.id]: action })
  }
  async init(effects: T.Effects): Promise<void> {
    for (let action of Object.values(this.actions)) {
      const fn = async () => {
        let res: (value?: undefined) => void = () => {}
        const complete = new Promise((resolve) => {
          res = resolve
        })
        const e: T.Effects = effects.child(action.id)
        e.constRetry = once(() =>
          complete.then(() => fn()).catch(console.error),
        )
        try {
          await action.exportMetadata({ effects: e })
        } finally {
          res()
        }
      }
      await fn()
    }
    await effects.action.clear({ except: Object.keys(this.actions) })
  }
  get<Id extends T.ActionId>(actionId: Id): AllActions[Id] {
    return this.actions[actionId]
  }
}
