import { DeepPartial } from "../../../types"
import { ValueSpec, ValueSpecUnion } from "../inputSpecTypes"
import { LazyBuild, InputSpec, ExtractInputSpecType } from "./inputSpec"
import { Parser, anyOf, literal, object } from "ts-matches"

export type UnionRes<
  Store,
  VariantValues extends {
    [K in string]: {
      name: string
      spec: InputSpec<any, Store> | InputSpec<any, never>
    }
  },
  K extends keyof VariantValues & string = keyof VariantValues & string,
> = {
  [key in keyof VariantValues]: {
    selection: key
    value: ExtractInputSpecType<VariantValues[key]["spec"]>
    other?: {
      [key2 in Exclude<keyof VariantValues & string, key>]?: DeepPartial<
        ExtractInputSpecType<VariantValues[key2]["spec"]>
      >
    }
  }
}[K]

/**
 * Used in the the Value.select { @link './value.ts' }
 * to indicate the type of select variants that are available. The key for the record passed in will be the
 * key to the tag.id in the Value.select
```ts
 
export const disabled = InputSpec.of({});
export const size = Value.number({
  name: "Max Chain Size",
  default: 550,
  description: "Limit of blockchain size on disk.",
  warning: "Increasing this value will require re-syncing your node.",
  required: true,
  range: "[550,1000000)",
  integral: true,
  units: "MiB",
  placeholder: null,
});
export const automatic = InputSpec.of({ size: size });
export const size1 = Value.number({
  name: "Failsafe Chain Size",
  default: 65536,
  description: "Prune blockchain if size expands beyond this.",
  warning: null,
  required: true,
  range: "[550,1000000)",
  integral: true,
  units: "MiB",
  placeholder: null,
});
export const manual = InputSpec.of({ size: size1 });
export const pruningSettingsVariants = Variants.of({
  disabled: { name: "Disabled", spec: disabled },
  automatic: { name: "Automatic", spec: automatic },
  manual: { name: "Manual", spec: manual },
});
export const pruning = Value.union(
  {
    name: "Pruning Settings",
    description:
      '- Disabled: Disable pruning\n- Automatic: Limit blockchain size on disk to a certain number of megabytes\n- Manual: Prune blockchain with the "pruneblockchain" RPC\n',
    warning: null,
    default: "disabled",
  },
  pruningSettingsVariants
);
```
 */
export class Variants<
  VariantValues extends {
    [K in string]: {
      name: string
      spec: InputSpec<any, Store> | InputSpec<any, never>
    }
  },
  Store,
> {
  private constructor(
    public build: LazyBuild<Store, ValueSpecUnion["variants"]>,
    public validator: Parser<unknown, UnionRes<Store, VariantValues>>,
  ) {}
  static of<
    VariantValues extends {
      [K in string]: {
        name: string
        spec: InputSpec<any, Store> | InputSpec<any, never>
      }
    },
    Store = never,
  >(a: VariantValues) {
    const validator = anyOf(
      ...Object.entries(a).map(([id, { spec }]) =>
        object({
          selection: literal(id),
          value: spec.validator,
        }),
      ),
    ) as Parser<unknown, any>

    return new Variants<VariantValues, Store>(async (options) => {
      const variants = {} as {
        [K in keyof VariantValues]: {
          name: string
          spec: Record<string, ValueSpec>
        }
      }
      for (const key in a) {
        const value = a[key]
        variants[key] = {
          name: value.name,
          spec: await value.spec.build(options as any),
        }
      }
      return variants
    }, validator)
  }
  /**
   * Use this during the times that the input needs a more specific type.
   * Used in types that the value/ variant/ list/ inputSpec is constructed somewhere else.
  ```ts
  const a = InputSpec.text({
    name: "a",
    required: false,
  })

  return InputSpec.of<Store>()({
    myValue: a.withStore(),
  })
  ```
   */
  withStore<NewStore extends Store extends never ? any : Store>() {
    return this as any as Variants<VariantValues, NewStore>
  }
}
