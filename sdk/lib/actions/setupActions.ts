import { InputSpec } from "./input/builder"
import { ExtractInputSpecType } from "./input/builder/inputSpec"
import * as T from "../types"

export type Run<
  A extends
    | Record<string, any>
    | InputSpec<Record<string, any>, any>
    | InputSpec<Record<string, never>, never>,
> = (options: {
  effects: T.Effects
  input: ExtractInputSpecType<A> & Record<string, any>
  prev?: ExtractInputSpecType<A> & Record<string, any>
}) => Promise<T.ActionResult | null>
export type GetInput<
  A extends
    | Record<string, any>
    | InputSpec<Record<string, any>, any>
    | InputSpec<Record<string, any>, never>,
> = (options: {
  effects: T.Effects
}) => Promise<void | (ExtractInputSpecType<A> & Record<string, any>)>

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

export class Action<
  Id extends T.ActionId,
  Store,
  InputSpecType extends
    | Record<string, any>
    | InputSpec<any, Store>
    | InputSpec<any, never>,
  Type extends
    ExtractInputSpecType<InputSpecType> = ExtractInputSpecType<InputSpecType>,
> {
  private constructor(
    readonly id: Id,
    private readonly metadataFn: MaybeFn<T.ActionMetadata>,
    private readonly inputSpec: InputSpecType,
    private readonly getInputFn: GetInput<Type>,
    private readonly runFn: Run<Type>,
  ) {}
  static of<
    Id extends T.ActionId,
    Store,
    InputSpecType extends
      | Record<string, any>
      | InputSpec<any, Store>
      | InputSpec<any, never>,
    Type extends
      ExtractInputSpecType<InputSpecType> = ExtractInputSpecType<InputSpecType>,
  >(
    id: Id,
    metadata: MaybeFn<T.ActionMetadata>,
    inputSpec: InputSpecType,
    getInput: GetInput<Type>,
    run: Run<Type>,
  ): Action<Id, Store, InputSpecType, Type> {
    return new Action(id, metadata, inputSpec, getInput, run)
  }
  async exportMetadata(options: {
    effects: T.Effects
  }): Promise<T.ActionMetadata> {
    const metadata = await callMaybeFn(this.metadataFn, options)
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
    input: Type
    prev?: Type
  }): Promise<T.ActionResult | null> {
    return this.runFn(options)
  }
}

export class Actions<
  Store,
  AllActions extends Record<T.ActionId, Action<T.ActionId, Store, any, any>>,
> {
  private constructor(private readonly actions: AllActions) {}
  static of<Store>(): Actions<Store, {}> {
    return new Actions({})
  }
  addAction<Id extends T.ActionId, A extends Action<Id, Store, any, any>>(
    action: A,
  ): Actions<Store, AllActions & { [id in Id]: A }> {
    return new Actions({ ...this.actions, [action.id]: action })
  }
  async update(options: { effects: T.Effects }): Promise<void> {
    for (let action of Object.values(this.actions)) {
      await action.exportMetadata(options)
    }
    await options.effects.action.clear({ except: Object.keys(this.actions) })
  }
}

export const setupActions = <
  Store,
  AllActions extends Record<T.ActionId, Action<T.ActionId, Store, any, any>>,
>(
  actions: Actions<Store, AllActions>,
) => actions
