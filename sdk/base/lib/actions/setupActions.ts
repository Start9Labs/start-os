/**
 * @module setupActions
 *
 * This module provides the Action and Actions classes for defining user-callable
 * operations in StartOS services. Actions appear in the StartOS UI and can be
 * triggered by users or programmatically by other services.
 *
 * @example
 * ```typescript
 * import { Action, Actions, InputSpec, Value } from '@start9labs/start-sdk'
 *
 * const resetPasswordAction = Action.withInput(
 *   'reset-password',
 *   { name: 'Reset Password', description: 'Reset the admin password' },
 *   InputSpec.of({
 *     username: Value.text({ name: 'Username', required: true, default: null })
 *   }),
 *   async ({ effects }) => ({ username: 'admin' }), // Pre-fill form
 *   async ({ effects, input }) => {
 *     // Perform the password reset
 *     return { result: { type: 'single', value: 'Password reset successfully' } }
 *   }
 * )
 *
 * export const actions = Actions.of().addAction(resetPasswordAction)
 * ```
 */

import { InputSpec } from "./input/builder"
import { ExtractInputSpecType } from "./input/builder/inputSpec"
import * as T from "../types"
import { once } from "../util"
import { InitScript } from "../inits"
import { Parser } from "ts-matches"

/** @internal Input spec type or null if the action has no input */
type MaybeInputSpec<Type> = {} extends Type ? null : InputSpec<Type>

/**
 * Function signature for executing an action.
 *
 * @typeParam A - The type of the validated input object
 * @param options.effects - Effects instance for system operations
 * @param options.input - The validated user input
 * @param options.spec - The input specification used to generate the form
 * @returns Promise resolving to an ActionResult to display to the user, or null/void for no result
 */
export type Run<A extends Record<string, any>> = (options: {
  effects: T.Effects
  input: A
  spec: T.inputSpecTypes.InputSpec
}) => Promise<(T.ActionResult & { version: "1" }) | null | void | undefined>

/**
 * Function signature for pre-filling action input forms.
 * Called before displaying the input form to populate default values.
 *
 * @typeParam A - The type of the input object
 * @param options.effects - Effects instance for system operations
 * @returns Promise resolving to partial input values to pre-fill, or null for no pre-fill
 */
export type GetInput<A extends Record<string, any>> = (options: {
  effects: T.Effects
}) => Promise<null | void | undefined | T.DeepPartial<A>>

/**
 * A value that can either be static or computed dynamically from Effects.
 * Used for action metadata that may need to change based on service state.
 *
 * @typeParam T - The type of the value
 *
 * @example
 * ```typescript
 * // Static metadata
 * const metadata: MaybeFn<ActionMetadata> = { name: 'My Action' }
 *
 * // Dynamic metadata based on service state
 * const dynamicMetadata: MaybeFn<ActionMetadata> = async ({ effects }) => {
 *   const isEnabled = await checkSomething(effects)
 *   return { name: isEnabled ? 'Disable Feature' : 'Enable Feature' }
 * }
 * ```
 */
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

/**
 * Type information interface for an Action.
 * Used for type inference in the Actions collection.
 *
 * @typeParam Id - The action's unique identifier type
 * @typeParam Type - The action's input type
 */
export interface ActionInfo<
  Id extends T.ActionId,
  Type extends Record<string, any>,
> {
  /** The unique identifier for this action */
  readonly id: Id
  /** @internal Type brand for input type inference */
  readonly _INPUT: Type
}

/**
 * Represents a user-callable action in a StartOS service.
 *
 * Exposed via `sdk.Action`. Actions are operations that users can trigger
 * from the StartOS UI or that can be invoked programmatically. Each action has:
 * - A unique ID
 * - Metadata (name, description, visibility, etc.)
 * - Optional input specification (form fields)
 * - A run function that executes the action
 *
 * Use `sdk.Action.withInput()` for actions that require user input, or
 * `sdk.Action.withoutInput()` for actions that run immediately.
 *
 * See the SDK documentation for detailed examples.
 *
 * @typeParam Id - The action's unique identifier type
 * @typeParam Type - The action's input type (empty object {} for no input)
 */
export class Action<Id extends T.ActionId, Type extends Record<string, any>>
  implements ActionInfo<Id, Type>
{
  /** @internal Type brand for input type inference */
  readonly _INPUT: Type = null as any as Type

  /** @internal Cache of built input specs by event ID */
  private prevInputSpec: Record<
    string,
    { spec: T.inputSpecTypes.InputSpec; validator: Parser<unknown, Type> }
  > = {}

  private constructor(
    /** The unique identifier for this action */
    readonly id: Id,
    private readonly metadataFn: MaybeFn<T.ActionMetadata>,
    private readonly inputSpec: MaybeInputSpec<Type>,
    private readonly getInputFn: GetInput<Type>,
    private readonly runFn: Run<Type>,
  ) {}

  /**
   * Creates an action that requires user input before execution.
   * The input form is defined by an InputSpec.
   *
   * @typeParam Id - The action ID type
   * @typeParam InputSpecType - The input specification type
   *
   * @param id - Unique identifier for the action (used in URLs and API calls)
   * @param metadata - Action metadata (name, description, visibility, etc.) - can be static or dynamic
   * @param inputSpec - Specification for the input form fields
   * @param getInput - Function to pre-populate the form with default/previous values
   * @param run - Function to execute when the action is submitted
   * @returns A new Action instance
   */
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

  /**
   * Creates an action that executes immediately without requiring user input.
   * Use this for simple operations like toggles, restarts, or status checks.
   *
   * @typeParam Id - The action ID type
   *
   * @param id - Unique identifier for the action
   * @param metadata - Action metadata (name, description, visibility, etc.) - can be static or dynamic
   * @param run - Function to execute when the action is triggered
   * @returns A new Action instance with no input
   */
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
  /**
   * Exports the action's metadata to StartOS, making it visible in the UI.
   * Called automatically during initialization by the Actions collection.
   *
   * @param options.effects - Effects instance for system operations
   * @returns Promise resolving to the exported metadata
   * @internal
   */
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

  /**
   * Builds and returns the input specification and pre-filled values for this action.
   * Called by StartOS when a user clicks on the action to display the input form.
   *
   * @param options.effects - Effects instance for system operations
   * @returns Promise resolving to the input specification and pre-filled values
   * @internal
   */
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

  /**
   * Executes the action with the provided input.
   * Called by StartOS when a user submits the action form.
   *
   * @param options.effects - Effects instance for system operations
   * @param options.input - The user-provided input (validated against the input spec)
   * @returns Promise resolving to the action result to display, or null for no result
   * @internal
   */
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

/**
 * A collection of actions for a StartOS service.
 *
 * Exposed via `sdk.Actions`. The Actions class manages the registration and
 * lifecycle of all actions in a service. It implements InitScript so it can
 * be included in the initialization pipeline to automatically register actions
 * with StartOS.
 *
 * @typeParam AllActions - Record type mapping action IDs to Action instances
 *
 * @example
 * ```typescript
 * // Create an actions collection
 * export const actions = sdk.Actions.of()
 *   .addAction(createUserAction)
 *   .addAction(resetPasswordAction)
 *   .addAction(restartAction)
 *
 * // Include in init pipeline
 * export const init = sdk.setupInit(
 *   versionGraph,
 *   setInterfaces,
 *   actions,  // Actions are registered here
 * )
 * ```
 */
export class Actions<
  AllActions extends Record<T.ActionId, Action<T.ActionId, any>>,
> implements InitScript
{
  private constructor(private readonly actions: AllActions) {}

  /**
   * Creates a new empty Actions collection.
   * Use `addAction()` to add actions to the collection.
   *
   * @returns A new empty Actions instance
   */
  static of(): Actions<{}> {
    return new Actions({})
  }

  /**
   * Adds an action to the collection.
   * Returns a new Actions instance with the action included (immutable pattern).
   *
   * @typeParam A - The action type being added
   * @param action - The action to add
   * @returns A new Actions instance containing all previous actions plus the new one
   *
   * @example
   * ```typescript
   * const actions = Actions.of()
   *   .addAction(action1)
   *   .addAction(action2)
   * ```
   */
  addAction<A extends Action<T.ActionId, any>>(
    action: A, // TODO: prevent duplicates
  ): Actions<AllActions & { [id in A["id"]]: A }> {
    return new Actions({ ...this.actions, [action.id]: action })
  }

  /**
   * Initializes all actions by exporting their metadata to StartOS.
   * Called automatically when included in the init pipeline.
   * Also clears any previously registered actions that are no longer in the collection.
   *
   * @param effects - Effects instance for system operations
   * @internal
   */
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

  /**
   * Retrieves an action from the collection by its ID.
   * Useful for programmatically invoking actions or inspecting their configuration.
   *
   * @typeParam Id - The action ID type
   * @param actionId - The ID of the action to retrieve
   * @returns The action instance
   */
  get<Id extends T.ActionId>(actionId: Id): AllActions[Id] {
    return this.actions[actionId]
  }
}
