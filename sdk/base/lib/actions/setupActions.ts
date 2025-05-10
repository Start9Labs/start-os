import { InputSpec } from "./input/builder"
import {
  ExtractInputSpecType,
  ExtractPartialInputSpecType,
} from "./input/builder/inputSpec"
import * as T from "../types"
import { once } from "../util"

export type Run<
  A extends Record<string, any> | InputSpec<Record<string, any>>,
> = (options: {
  effects: T.Effects
  input: ExtractInputSpecType<A>
}) => Promise<(T.ActionResult & { version: "1" }) | null | void | undefined>
export type GetInput<
  A extends Record<string, any> | InputSpec<Record<string, any>>,
> = (options: {
  effects: T.Effects
}) => Promise<null | void | undefined | ExtractPartialInputSpecType<A>>

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

export class Action<
  Id extends T.ActionId,
  InputSpecType extends Record<string, any> | InputSpec<any>,
> {
  private constructor(
    readonly id: Id,
    private readonly metadataFn: MaybeFn<T.ActionMetadata>,
    private readonly inputSpec: InputSpecType,
    private readonly getInputFn: GetInput<InputSpecType>,
    private readonly runFn: Run<InputSpecType>,
  ) {}
  static withInput<
    Id extends T.ActionId,
    InputSpecType extends Record<string, any> | InputSpec<any>,
  >(
    id: Id,
    metadata: MaybeFn<Omit<T.ActionMetadata, "hasInput">>,
    inputSpec: InputSpecType,
    getInput: GetInput<InputSpecType>,
    run: Run<InputSpecType>,
  ): Action<Id, InputSpecType> {
    return new Action(
      id,
      mapMaybeFn(metadata, (m) => ({ ...m, hasInput: true })),
      inputSpec,
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
      {},
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
    return {
      spec: await this.inputSpec.build(options),
      value: (await this.getInputFn(options)) || null,
    }
  }
  async run(options: {
    effects: T.Effects
    input: ExtractInputSpecType<InputSpecType>
  }): Promise<T.ActionResult | null> {
    return (await this.runFn(options)) || null
  }
}

export class Actions<
  AllActions extends Record<T.ActionId, Action<T.ActionId, any>>,
> {
  private constructor(private readonly actions: AllActions) {}
  static of(): Actions<{}> {
    return new Actions({})
  }
  addAction<A extends Action<T.ActionId, any>>(
    action: A, // TODO: prevent duplicates
  ): Actions<AllActions & { [id in A["id"]]: A }> {
    return new Actions({ ...this.actions, [action.id]: action })
  }
  async update(options: { effects: T.Effects }): Promise<null> {
    for (let action of Object.values(this.actions)) {
      await action.exportMetadata(options)
    }
    await options.effects.action.clear({ except: Object.keys(this.actions) })

    return null
  }
  get<Id extends T.ActionId>(actionId: Id): AllActions[Id] {
    return this.actions[actionId]
  }
}
