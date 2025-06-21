import { ValueSpec } from "../inputSpecTypes"
import { Value } from "./value"
import { _ } from "../../../util"
import { Effects } from "../../../Effects"
import { Parser, object } from "ts-matches"
import { DeepPartial } from "../../../types"

export type LazyBuildOptions = {
  effects: Effects
}
export type LazyBuild<ExpectedOut> = (
  options: LazyBuildOptions,
) => Promise<ExpectedOut> | ExpectedOut

// prettier-ignore
export type ExtractInputSpecType<A extends InputSpec<Record<string, any>>> = 
  A extends InputSpec<infer B> ? B :
  never

export type ExtractPartialInputSpecType<
  A extends Record<string, any> | InputSpec<Record<string, any>>,
> = A extends InputSpec<infer B> ? DeepPartial<B> : DeepPartial<A>

export type InputSpecOf<A extends Record<string, any>> = {
  [K in keyof A]: Value<A[K]>
}

export type MaybeLazyValues<A> = LazyBuild<A> | A
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
export class InputSpec<Type extends Record<string, any>> {
  private constructor(
    private readonly spec: {
      [K in keyof Type]: Value<Type[K]>
    },
  ) {}
  public _TYPE: Type = null as any as Type
  public _PARTIAL: DeepPartial<Type> = null as any as DeepPartial<Type>
  async build(options: LazyBuildOptions): Promise<{
    spec: {
      [K in keyof Type]: ValueSpec
    }
    validator: Parser<unknown, Type>
  }> {
    const answer = {} as {
      [K in keyof Type]: ValueSpec
    }
    const validator = {} as {
      [K in keyof Type]: Parser<unknown, any>
    }
    for (const k in this.spec) {
      const built = await this.spec[k].build(options as any)
      answer[k] = built.spec
      validator[k] = built.validator
    }
    return {
      spec: answer,
      validator: object(validator) as any,
    }
  }

  static of<Spec extends Record<string, Value<any>>>(spec: Spec) {
    return new InputSpec<{
      [K in keyof Spec]: Spec[K] extends Value<infer T> ? T : never
    }>(spec)
  }
}
