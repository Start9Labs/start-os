import { Config, ExtractConfigType } from "../config/builder/config"
import { SDKManifest } from "../manifest/ManifestTypes"
import { ActionMetadata, ActionResult, Effects, ExportedAction } from "../types"

export type MaybeFn<Manifest extends SDKManifest, Store, Value> =
  | Value
  | ((options: { effects: Effects }) => Promise<Value> | Value)
export class CreatedAction<
  Manifest extends SDKManifest,
  Store,
  ConfigType extends
    | Record<string, any>
    | Config<any, Store>
    | Config<any, never>,
  Type extends Record<string, any> = ExtractConfigType<ConfigType>,
> {
  private constructor(
    public readonly id: string,
    public readonly myMetadata: MaybeFn<
      Manifest,
      Store,
      Omit<ActionMetadata, "input">
    >,
    readonly fn: (options: {
      effects: Effects
      input: Type
    }) => Promise<ActionResult>,
    readonly input: Config<Type, Store>,
    public validator = input.validator,
  ) {}

  static of<
    Manifest extends SDKManifest,
    Store,
    ConfigType extends
      | Record<string, any>
      | Config<any, any>
      | Config<any, never>,
    Type extends Record<string, any> = ExtractConfigType<ConfigType>,
  >(
    id: string,
    metadata: MaybeFn<Manifest, Store, Omit<ActionMetadata, "input">>,
    fn: (options: { effects: Effects; input: Type }) => Promise<ActionResult>,
    inputConfig: Config<Type, Store> | Config<Type, never>,
  ) {
    return new CreatedAction<Manifest, Store, ConfigType, Type>(
      id,
      metadata,
      fn,
      inputConfig as Config<Type, Store>,
    )
  }

  exportedAction: ExportedAction = ({ effects, input }) => {
    return this.fn({
      effects,
      input: this.validator.unsafeCast(input),
    })
  }

  run = async ({ effects, input }: { effects: Effects; input?: Type }) => {
    return this.fn({
      effects,
      input: this.validator.unsafeCast(input),
    })
  }

  async metadata(options: { effects: Effects }) {
    if (this.myMetadata instanceof Function)
      return await this.myMetadata(options)
    return this.myMetadata
  }

  async ActionMetadata(options: { effects: Effects }): Promise<ActionMetadata> {
    return {
      ...(await this.metadata(options)),
      input: await this.input.build(options),
    }
  }

  async getConfig({ effects }: { effects: Effects }) {
    return this.input.build({
      effects,
    })
  }
}

export const createAction = CreatedAction.of
