import { ValueSpec } from '../inputSpecTypes'
import { Value } from './value'
import { _ } from '../../../util'
import { Effects } from '../../../Effects'
import { z } from 'zod'
import { DeepPartial } from '../../../types'
import { InputSpecTools, createInputSpecTools } from './inputSpecTools'

/** Options passed to a lazy builder function when resolving dynamic form field values. */
export type LazyBuildOptions<Type> = {
  /** The effects interface for runtime operations (e.g. reading files, querying state). */
  effects: Effects
  /** Previously saved form data to pre-fill the form with, or `null` for fresh creation. */
  prefill: DeepPartial<Type> | null
}
/**
 * A function that lazily produces a value, potentially using effects and prefill data.
 * Used by `dynamic*` variants of {@link Value} to compute form field options at runtime.
 */
export type LazyBuild<ExpectedOut, Type> = (
  options: LazyBuildOptions<Type>,
) => Promise<ExpectedOut> | ExpectedOut

/** Extracts the runtime type from an {@link InputSpec}. */
// prettier-ignore
export type ExtractInputSpecType<A extends InputSpec<Record<string, any>, any>> =
  A extends InputSpec<infer B, any> ? B :
  never

/** Extracts the static validation type from an {@link InputSpec}. */
export type ExtractInputSpecStaticValidatedAs<
  A extends InputSpec<any, Record<string, any>>,
> = A extends InputSpec<any, infer B> ? B : never

// export type ExtractPartialInputSpecType<
//   A extends Record<string, any> | InputSpec<Record<string, any>>,
// > = A extends InputSpec<infer B> ? DeepPartial<B> : DeepPartial<A>

/** Maps an object type to a record of {@link Value} entries for use with `InputSpec.of`. */
export type InputSpecOf<A extends Record<string, any>> = {
  [K in keyof A]: Value<A[K]>
}

/** A value that is either directly provided or lazily computed via a {@link LazyBuild} function. */
export type MaybeLazyValues<A, T> = LazyBuild<A, T> | A
/**
 * InputSpecs are the specs that are used by the os input specification form for this service.
 * Here is an example of a simple input specification
  ```ts
    const smallInputSpec = InputSpec.of({
      test: Value.boolean({
        name: "Test",
        description: "This is the description for the test",
        warning: null,
        default: false,
      }),
    });
  ```

  The idea of an inputSpec is that now the form is going to ask for
  Test: [ ] and the value is going to be checked as a boolean.
  There are more complex values like selects, lists, and objects. See {@link Value}

  Also, there is the ability to get a validator/parser from this inputSpec spec.
  ```ts
  const matchSmallInputSpec = smallInputSpec.validator();
  type SmallInputSpec = typeof matchSmallInputSpec._TYPE;
  ```

  Here is an example of a more complex input specification which came from an input specification for a service
  that works with bitcoin, like c-lightning.
  ```ts

    export const hostname = Value.string({
  name: "Hostname",
  default: null,
  description: "Domain or IP address of bitcoin peer",
  warning: null,
  required: true,
  masked: false,
  placeholder: null,
  pattern:
    "(^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$)|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))",
  patternDescription:
    "Must be either a domain name, or an IPv4 or IPv6 address. Do not include protocol scheme (eg 'http://') or port.",
});
export const port = Value.number({
  name: "Port",
  default: null,
  description: "Port that peer is listening on for inbound p2p connections",
  warning: null,
  required: false,
  range: "[0,65535]",
  integral: true,
  units: null,
  placeholder: null,
});
export const addNodesSpec = InputSpec.of({ hostname: hostname, port: port });

  ```
 */
export class InputSpec<
  Type extends StaticValidatedAs,
  StaticValidatedAs extends Record<string, any> = Type,
> {
  private constructor(
    private readonly spec: {
      [K in keyof Type]: Value<Type[K]>
    },
    public readonly validator: z.ZodType<StaticValidatedAs>,
  ) {}
  public _TYPE: Type = null as any as Type
  public _PARTIAL: DeepPartial<Type> = null as any as DeepPartial<Type>
  /**
   * Builds the runtime form specification and combined Zod validator from this InputSpec's fields.
   *
   * @returns An object containing the resolved `spec` (field specs keyed by name) and a combined `validator`
   */
  async build<OuterType>(options: LazyBuildOptions<OuterType>): Promise<{
    spec: {
      [K in keyof Type]: ValueSpec
    }
    validator: z.ZodType<Type>
  }> {
    const answer = {} as {
      [K in keyof Type]: ValueSpec
    }
    const validator = {} as {
      [K in keyof Type]: z.ZodType<any>
    }
    for (const k in this.spec) {
      const built = await this.spec[k].build(options as any)
      answer[k] = built.spec
      validator[k] = built.validator
    }
    return {
      spec: answer,
      validator: z.object(validator) as any,
    }
  }

  /**
   * Adds a single named field to this spec, returning a new `InputSpec` with the extended type.
   *
   * @param key - The field key name
   * @param build - A {@link Value} instance, or a function receiving typed tools that returns one
   */
  addKey<Key extends string, V extends Value<any, any, any>>(
    key: Key,
    build: V | ((tools: InputSpecTools<Type>) => V),
  ): InputSpec<
    Type & { [K in Key]: V extends Value<infer T, any, any> ? T : never },
    StaticValidatedAs & {
      [K in Key]: V extends Value<any, infer S, any> ? S : never
    }
  > {
    const value =
      build instanceof Function ? build(createInputSpecTools<Type>()) : build
    const newSpec = { ...this.spec, [key]: value } as any
    const newValidator = z.object(
      Object.fromEntries(
        Object.entries(newSpec).map(([k, v]) => [
          k,
          (v as Value<any>).validator,
        ]),
      ),
    )
    return new InputSpec(newSpec, newValidator as any)
  }

  /**
   * Adds multiple fields to this spec at once, returning a new `InputSpec` with extended types.
   *
   * @param build - A record of {@link Value} entries, or a function receiving typed tools that returns one
   */
  add<AddSpec extends Record<string, Value<any, any, any>>>(
    build: AddSpec | ((tools: InputSpecTools<Type>) => AddSpec),
  ): InputSpec<
    Type & {
      [K in keyof AddSpec]: AddSpec[K] extends Value<infer T, any, any>
        ? T
        : never
    },
    StaticValidatedAs & {
      [K in keyof AddSpec]: AddSpec[K] extends Value<any, infer S, any>
        ? S
        : never
    }
  > {
    const addedValues =
      build instanceof Function ? build(createInputSpecTools<Type>()) : build
    const newSpec = { ...this.spec, ...addedValues } as any
    const newValidator = z.object(
      Object.fromEntries(
        Object.entries(newSpec).map(([k, v]) => [
          k,
          (v as Value<any>).validator,
        ]),
      ),
    )
    return new InputSpec(newSpec, newValidator as any)
  }

  /**
   * Creates an `InputSpec` from a plain record of {@link Value} entries.
   *
   * @example
   * ```ts
   * const spec = InputSpec.of({
   *   username: Value.text({ name: 'Username', required: true, default: null }),
   *   verbose: Value.toggle({ name: 'Verbose Logging', default: false }),
   * })
   * ```
   */
  static of<Spec extends Record<string, Value<any, any>>>(spec: Spec) {
    const validator = z.object(
      Object.fromEntries(
        Object.entries(spec).map(([k, v]) => [k, v.validator]),
      ),
    )
    return new InputSpec<
      {
        [K in keyof Spec]: Spec[K] extends Value<infer T, any, unknown>
          ? T
          : never
      },
      {
        [K in keyof Spec]: Spec[K] extends Value<any, infer T, unknown>
          ? T
          : never
      }
    >(spec, validator as any)
  }
}
