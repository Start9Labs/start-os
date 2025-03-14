import { Value } from "../../base/lib/actions/input/builder/value"
import {
  InputSpec,
  ExtractInputSpecType,
  LazyBuild,
} from "../../base/lib/actions/input/builder/inputSpec"
import {
  DefaultString,
  ListValueSpecText,
  Pattern,
  RandomString,
  UniqueBy,
  ValueSpecDatetime,
  ValueSpecText,
} from "../../base/lib/actions/input/inputSpecTypes"
import { Variants } from "../../base/lib/actions/input/builder/variants"
import { Action, Actions } from "../../base/lib/actions/setupActions"
import {
  SyncOptions,
  ServiceInterfaceId,
  PackageId,
  ServiceInterfaceType,
  Effects,
} from "../../base/lib/types"
import * as patterns from "../../base/lib/util/patterns"
import { BackupSync, Backups } from "./backup/Backups"
import { smtpInputSpec } from "../../base/lib/actions/input/inputSpecConstants"
import { CommandController, Daemons } from "./mainFn/Daemons"
import { HealthCheck, HealthCheckParams } from "./health/HealthCheck"
import { checkPortListening } from "./health/checkFns/checkPortListening"
import { checkWebUrl, runHealthScript } from "./health/checkFns"
import { List } from "../../base/lib/actions/input/builder/list"
import { Install, InstallFn } from "./inits/setupInstall"
import { SetupBackupsParams, setupBackups } from "./backup/setupBackups"
import { UninstallFn, setupUninstall } from "./inits/setupUninstall"
import { setupMain } from "./mainFn"
import { defaultTrigger } from "./trigger/defaultTrigger"
import { changeOnFirstSuccess, cooldownTrigger } from "./trigger"
import {
  UpdateServiceInterfaces,
  setupServiceInterfaces,
} from "../../base/lib/interfaces/setupInterfaces"
import { successFailure } from "./trigger/successFailure"
import { MultiHost, Scheme } from "../../base/lib/interfaces/Host"
import { ServiceInterfaceBuilder } from "../../base/lib/interfaces/ServiceInterfaceBuilder"
import { GetSystemSmtp } from "./util"
import { nullIfEmpty } from "./util"
import { getServiceInterface, getServiceInterfaces } from "./util"
import { getStore } from "./store/getStore"
import { CommandOptions, MountOptions, SubContainer } from "./util/SubContainer"
import { splitCommand } from "./util"
import { Mounts } from "./mainFn/Mounts"
import { setupDependencies } from "../../base/lib/dependencies/setupDependencies"
import * as T from "../../base/lib/types"
import { testTypeVersion } from "../../base/lib/exver"
import { ExposedStorePaths } from "./store/setupExposeStore"
import {
  PathBuilder,
  extractJsonPath,
  pathBuilder,
} from "../../base/lib/util/PathBuilder"
import {
  CheckDependencies,
  checkDependencies,
} from "../../base/lib/dependencies/dependencies"
import { GetSslCertificate } from "./util"
import { VersionGraph } from "./version"
import { MaybeFn } from "../../base/lib/actions/setupActions"
import { GetInput } from "../../base/lib/actions/setupActions"
import { Run } from "../../base/lib/actions/setupActions"
import * as actions from "../../base/lib/actions"
import { setupInit } from "./inits/setupInit"
import * as fs from "node:fs/promises"

export const OSVersion = testTypeVersion("0.3.6-alpha.16")

// prettier-ignore
type AnyNeverCond<T extends any[], Then, Else> = 
    T extends [] ? Else :
    T extends [never, ...Array<any>] ? Then :
    T extends [any, ...infer U] ? AnyNeverCond<U,Then, Else> :
    never

export class StartSdk<Manifest extends T.SDKManifest, Store> {
  private constructor(readonly manifest: Manifest) {}
  static of() {
    return new StartSdk<never, never>(null as never)
  }
  withManifest<Manifest extends T.SDKManifest = never>(manifest: Manifest) {
    return new StartSdk<Manifest, Store>(manifest)
  }
  withStore<Store extends Record<string, any>>() {
    return new StartSdk<Manifest, Store>(this.manifest)
  }

  build(isReady: AnyNeverCond<[Manifest, Store], "Build not ready", true>) {
    type NestedEffects = "subcontainer" | "store" | "action"
    type InterfaceEffects =
      | "getServiceInterface"
      | "listServiceInterfaces"
      | "exportServiceInterface"
      | "clearServiceInterfaces"
      | "bind"
      | "getHostInfo"
    type MainUsedEffects = "setMainStatus" | "setHealth"
    type CallbackEffects = "constRetry" | "clearCallbacks"
    type AlreadyExposed = "getSslCertificate" | "getSystemSmtp"

    // prettier-ignore
    type StartSdkEffectWrapper = {
      [K in keyof Omit<Effects, NestedEffects | InterfaceEffects | MainUsedEffects | CallbackEffects | AlreadyExposed>]: (effects: Effects, ...args: Parameters<Effects[K]>) => ReturnType<Effects[K]>
    }
    const startSdkEffectWrapper: StartSdkEffectWrapper = {
      restart: (effects, ...args) => effects.restart(...args),
      setDependencies: (effects, ...args) => effects.setDependencies(...args),
      checkDependencies: (effects, ...args) =>
        effects.checkDependencies(...args),
      mount: (effects, ...args) => effects.mount(...args),
      getInstalledPackages: (effects, ...args) =>
        effects.getInstalledPackages(...args),
      exposeForDependents: (effects, ...args) =>
        effects.exposeForDependents(...args),
      getServicePortForward: (effects, ...args) =>
        effects.getServicePortForward(...args),
      clearBindings: (effects, ...args) => effects.clearBindings(...args),
      getContainerIp: (effects, ...args) => effects.getContainerIp(...args),
      getOsIp: (effects, ...args) => effects.getOsIp(...args),
      getSslKey: (effects, ...args) => effects.getSslKey(...args),
      setDataVersion: (effects, ...args) => effects.setDataVersion(...args),
      getDataVersion: (effects, ...args) => effects.getDataVersion(...args),
      shutdown: (effects, ...args) => effects.shutdown(...args),
      getDependencies: (effects, ...args) => effects.getDependencies(...args),
      getStatus: (effects, ...args) => effects.getStatus(...args),
    }

    return {
      manifest: this.manifest,
      ...startSdkEffectWrapper,
      action: {
        run: actions.runAction,
        request: <T extends Action<T.ActionId, any, any>>(
          effects: T.Effects,
          packageId: T.PackageId,
          action: T,
          severity: T.ActionSeverity,
          options?: actions.ActionRequestOptions<T>,
        ) =>
          actions.requestAction({
            effects,
            packageId,
            action,
            severity,
            options: options,
          }),
        requestOwn: <T extends Action<T.ActionId, Store, any>>(
          effects: T.Effects,
          action: T,
          severity: T.ActionSeverity,
          options?: actions.ActionRequestOptions<T>,
        ) =>
          actions.requestAction({
            effects,
            packageId: this.manifest.id,
            action,
            severity,
            options: options,
          }),
        clearRequest: (effects: T.Effects, ...replayIds: string[]) =>
          effects.action.clearRequests({ only: replayIds }),
      },
      checkDependencies: checkDependencies as <
        DependencyId extends keyof Manifest["dependencies"] &
          PackageId = keyof Manifest["dependencies"] & PackageId,
      >(
        effects: Effects,
        packageIds?: DependencyId[],
      ) => Promise<CheckDependencies<DependencyId>>,
      serviceInterface: {
        getOwn: <E extends Effects>(effects: E, id: ServiceInterfaceId) =>
          getServiceInterface(effects, {
            id,
          }),
        get: <E extends Effects>(
          effects: E,
          opts: { id: ServiceInterfaceId; packageId: PackageId },
        ) => getServiceInterface(effects, opts),
        getAllOwn: <E extends Effects>(effects: E) =>
          getServiceInterfaces(effects, {}),
        getAll: <E extends Effects>(
          effects: E,
          opts: { packageId: PackageId },
        ) => getServiceInterfaces(effects, opts),
      },

      store: {
        get: <E extends Effects, StoreValue = unknown>(
          effects: E,
          packageId: string,
          path: PathBuilder<Store, StoreValue>,
        ) =>
          getStore<Store, StoreValue>(effects, path, {
            packageId,
          }),
        getOwn: <E extends Effects, StoreValue = unknown>(
          effects: E,
          path: PathBuilder<Store, StoreValue>,
        ) => getStore<Store, StoreValue>(effects, path),
        setOwn: <E extends Effects, Path extends PathBuilder<Store, unknown>>(
          effects: E,
          path: Path,
          value: Path extends PathBuilder<Store, infer Value> ? Value : never,
        ) =>
          effects.store.set<Store>({
            value,
            path: extractJsonPath(path),
          }),
      },

      MultiHost: {
        of: (effects: Effects, id: string) => new MultiHost({ id, effects }),
      },
      nullIfEmpty,
      useEntrypoint: (overrideCmd?: string[]) =>
        new T.UseEntrypoint(overrideCmd),
      runCommand: async <A extends string>(
        effects: Effects,
        image: {
          imageId: keyof Manifest["images"] & T.ImageId
          sharedRun?: boolean
        },
        command: T.CommandType,
        options: CommandOptions & {
          mounts?: { mountpoint: string; options: MountOptions }[]
        },
        /**
         * A name to use to refer to the ephemeral subcontainer for debugging purposes
         */
        name?: string,
      ): Promise<{ stdout: string | Buffer; stderr: string | Buffer }> => {
        return runCommand<Manifest>(effects, image, command, options, name)
      },
      /**
       * @description Use this class to create an Action. By convention, each Action should receive its own file.
       *
       */
      Action: {
        /**
         * @description Use this function to create an action that accepts form input
         * @param id - a unique ID for this action
         * @param metadata - information describing the action and its availability
         * @param inputSpec - define the form input using the InputSpec and Value classes
         * @param prefillFn - optionally fetch data from the file system to pre-fill the input form. Must returns a deep partial of the input spec
         * @param executionFn - execute the action. Optionally return data for the user to view. Must be in the structure of an ActionResult, version "1"
         * @example
         * In this example, we create an action for a user to provide their name.
         *   We prefill the input form with their existing name from the service's yaml file.
         *   The new name is saved to the yaml file, and we return nothing to the user, which
         *   means they will receive a generic success message.
         * 
         * ```
          import { sdk } from '../sdk'
          import { yamlFile } from '../file-models/config.yml'

          const { InputSpec, Value } = sdk

          export const inputSpec = InputSpec.of({
            name: Value.text({
              name: 'Name',
              description:
                'When you launch the Hello World UI, it will display "Hello [Name]"',
              required: true,
              default: 'World',
            }),
          })

          export const setName = sdk.Action.withInput(
            // id
            'set-name',

            // metadata
            async ({ effects }) => ({
              name: 'Set Name',
              description: 'Set your name so Hello World can say hello to you',
              warning: null,
              allowedStatuses: 'any',
              group: null,
              visibility: 'enabled',
            }),

            // form input specification
            inputSpec,

            // optionally pre-fill the input form
            async ({ effects }) => {
              const name = await yamlFile.read.const(effects)?.name
              return { name }
            },

            // the execution function
            async ({ effects, input }) => yamlFile.merge(input)
          )
         * ```
        */
        withInput: <
          Id extends T.ActionId,
          InputSpecType extends
            | Record<string, any>
            | InputSpec<any, any>
            | InputSpec<any, never>,
        >(
          id: Id,
          metadata: MaybeFn<Omit<T.ActionMetadata, "hasInput">>,
          inputSpec: InputSpecType,
          getInput: GetInput<InputSpecType>,
          run: Run<InputSpecType>,
        ) => Action.withInput(id, metadata, inputSpec, getInput, run),
        /**
         * @description Use this function to create an action that does not accept form input
         * @param id - a unique ID for this action
         * @param metadata - information describing the action and its availability
         * @param executionFn - execute the action. Optionally return data for the user to view. Must be in the structure of an ActionResult, version "1"
         * @example
         * In this example, we create an action that returns a secret phrase for the user to see.
         * 
         * ```
          import { sdk } from '../sdk'

          export const showSecretPhrase = sdk.Action.withoutInput(
            // id
            'show-secret-phrase',

            // metadata
            async ({ effects }) => ({
              name: 'Show Secret Phrase',
              description: 'Reveal the secret phrase for Hello World',
              warning: null,
              allowedStatuses: 'any',
              group: null,
              visibility: 'enabled',
            }),

            // the execution function
            async ({ effects }) => ({
              version: '1',
              title: 'Secret Phrase',
              message:
                'Below is your secret phrase. Use it to gain access to extraordinary places',
              result: {
                type: 'single',
                value: await sdk.store
                  .getOwn(effects, sdk.StorePath.secretPhrase)
                  .const(),
                copyable: true,
                qr: true,
                masked: true,
              },
            }),
          )
         * ```
        */
        withoutInput: <Id extends T.ActionId>(
          id: Id,
          metadata: MaybeFn<Omit<T.ActionMetadata, "hasInput">>,
          run: Run<{}>,
        ) => Action.withoutInput(id, metadata, run),
      },
      inputSpecConstants: { smtpInputSpec },
      /**
       * @description Use this function to create a service interface.
       * @param effects
       * @param options
       * @example
       * In this example, we create a standard web UI
       *
       * ```
        const ui = sdk.createInterface(effects, {
          name: 'Web UI',
          id: 'ui',
          description: 'The primary web app for this service.',
          type: 'ui',
          masked: false,
          schemeOverride: null,
          username: null,
          path: '',
          search: {},
        })
       * ```
       */
      createInterface: (
        effects: Effects,
        options: {
          /** The human readable name of this service interface. */
          name: string
          /** A unique ID for this service interface. */
          id: string
          /** The human readable description. */
          description: string
          /** Affects how the interface appears to the user. One of: 'ui', 'api', 'p2p'. If 'ui', the user will see a "Launch UI" button */
          type: ServiceInterfaceType
          /** (optional) prepends the provided username to all URLs. */
          username: null | string
          /** (optional) appends the provided path to all URLs. */
          path: string
          /** (optional) appends the provided query params to all URLs. */
          search: Record<string, string>
          /** (optional) overrides the protocol prefix provided by the bind function.
           *
           * @example `ftp://`
           */
          schemeOverride: { ssl: Scheme; noSsl: Scheme } | null
          /** TODO Aiden how would someone include a password in the URL? Whether or not to mask the URLs on the screen, for example, when they contain a password */
          masked: boolean
        },
      ) => new ServiceInterfaceBuilder({ ...options, effects }),
      getSystemSmtp: <E extends Effects>(effects: E) =>
        new GetSystemSmtp(effects),
      getSslCerificate: <E extends Effects>(
        effects: E,
        hostnames: string[],
        algorithm?: T.Algorithm,
      ) => new GetSslCertificate(effects, hostnames, algorithm),
      HealthCheck: {
        of(effects: T.Effects, o: Omit<HealthCheckParams, "effects">) {
          return HealthCheck.of({ effects, ...o })
        },
      },
      healthCheck: {
        checkPortListening,
        checkWebUrl,
        runHealthScript,
      },
      patterns,
      /**
       * @description Use this function to list every Action offered by the service. Actions will be displayed in the provided order.
       *
       *   By convention, each Action should receive its own file in the "actions" directory.
       * @example
       * 
       * ```
        import { sdk } from '../sdk'
        import { config } from './config'
        import { nameToLogs } from './nameToLogs'

        export const actions = sdk.Actions.of().addAction(config).addAction(nameToLogs)
       * ```
       */
      Actions: Actions<Store, {}>,
      /**
       * @description Use this function to determine which volumes are backed up when a user creates a backup, including advanced options.
       * @example
       * In this example, we back up the entire "main" volume and nothing else.
       *
       * ```
        import { sdk } from './sdk'

        export const { createBackup, restoreBackup } = sdk.setupBackups(
          async ({ effects }) => sdk.Backups.volumes('main'),
        )
       * ```
       * @example
       * In this example, we back up the "main" volume, but exclude hypothetical directory "excludedDir".
       *
       * ```
        import { sdk } from './sdk'

        export const { createBackup, restoreBackup } = sdk.setupBackups(async () =>
          sdk.Backups.volumes('main').setOptions({
            exclude: ['excludedDir'],
          }),
        )
       * ```
       */
      setupBackups: (options: SetupBackupsParams<Manifest>) =>
        setupBackups<Manifest>(options),
      /**
       * @description Use this function to set dependency information.
       * @example
       * In this example, we create a perpetual dependency on Hello World >=1.0.0:0, where Hello World must be running and passing its "primary" health check.
       *
       * ```
        export const setDependencies = sdk.setupDependencies(
          async ({ effects, input }) => {
            return {
              'hello-world': {
                kind: 'running',
                versionRange: '>=1.0.0',
                healthChecks: ['primary'],
              },
            }
          },
        )
       * ```
       * @example
       * In this example, we create a conditional dependency on Hello World based on a hypothetical "needsWorld" boolean in our Store.
       * Using .const() ensures that if the "needsWorld" boolean changes, setupDependencies will re-run.
       *
       * ```
        export const setDependencies = sdk.setupDependencies(
          async ({ effects }) => {
            if (sdk.store.getOwn(sdk.StorePath.needsWorld).const()) {
              return {
                'hello-world': {
                  kind: 'running',
                  versionRange: '>=1.0.0',
                  healthChecks: ['primary'],
                },
              }
            }
            return {}
          },
        )
       * ```
       */
      setupDependencies: setupDependencies<Manifest>,
      setupInit: setupInit<Manifest, Store>,
      /**
       * @description Use this function to execute arbitrary logic *once*, on initial install only.
       * @example
       * In the this example, we bootstrap our Store with a random, 16-char admin password.
       *
       * ```
        const install = sdk.setupInstall(async ({ effects }) => {
          await sdk.store.setOwn(
            effects,
            sdk.StorePath.adminPassword,
            utils.getDefaultString({
              charset: 'a-z,A-Z,1-9,!,@,$,%,&,',
              len: 16,
            }),
          )
        })
       * ```
       */
      setupInstall: (fn: InstallFn<Manifest, Store>) => Install.of(fn),
      /**
       * @description Use this function to determine how this service will be hosted and served. The function executes on service install, service update, and inputSpec save.
       *
       *   "input" will be of type `Input` for inputSpec save. It will be `null` for install and update.
       *
       *   To learn about creating multi-hosts and interfaces, check out the {@link https://docs.start9.com/packaging-guide/learn/interfaces documentation}.
       * @param inputSpec - The inputSpec spec of this service as exported from /inputSpec/spec.
       * @param fn - an async function that returns an array of interface receipts. The function always has access to `effects`; it has access to `input` only after inputSpec save, otherwise `input` will be null.
       * @example
       * In this example, we create two UIs from one multi-host, and one API from another multi-host.
       *
       * ```
        export const setInterfaces = sdk.setupInterfaces(
          inputSpecSpec,
          async ({ effects, input }) => {
            // ** UI multi-host **
            const uiMulti = sdk.MultiHost.of(effects, 'ui-multi')
            const uiMultiOrigin = await uiMulti.bindPort(80, {
              protocol: 'http',
            })
            // Primary UI
            const primaryUi = sdk.createInterface(effects, {
              name: 'Primary UI',
              id: 'primary-ui',
              description: 'The primary web app for this service.',
              type: 'ui',
              masked: false,
              schemeOverride: null,
              username: null,
              path: '',
              search: {},
            })
            // Admin UI
            const adminUi = sdk.createInterface(effects, {
              name: 'Admin UI',
              id: 'admin-ui',
              description: 'The admin web app for this service.',
              type: 'ui',
              masked: false,
              schemeOverride: null,
              username: null,
              path: '/admin',
              search: {},
            })
            // UI receipt
            const uiReceipt = await uiMultiOrigin.export([primaryUi, adminUi])
       
            // ** API multi-host **
            const apiMulti = sdk.MultiHost.of(effects, 'api-multi')
            const apiMultiOrigin = await apiMulti.bindPort(5959, {
              protocol: 'http',
            })
            // API
            const api = sdk.createInterface(effects, {
              name: 'Admin API',
              id: 'api',
              description: 'The advanced API for this service.',
              type: 'api',
              masked: false,
              schemeOverride: null,
              username: null,
              path: '',
              search: {},
            })
            // API receipt
            const apiReceipt = await apiMultiOrigin.export([api])
       
            // ** Return receipts **
            return [uiReceipt, apiReceipt]
          },
        )
       * ```
       */
      setupInterfaces: setupServiceInterfaces,
      setupMain: (
        fn: (o: {
          effects: Effects
          started(onTerm: () => PromiseLike<void>): PromiseLike<null>
        }) => Promise<Daemons<Manifest, any>>,
      ) => setupMain<Manifest, Store>(fn),
      /**
       * Use this function to execute arbitrary logic *once*, on uninstall only. Most services will not use this.
       */
      setupUninstall: (fn: UninstallFn<Manifest, Store>) =>
        setupUninstall<Manifest, Store>(fn),
      trigger: {
        defaultTrigger,
        cooldownTrigger,
        changeOnFirstSuccess,
        successFailure,
      },
      Mounts: {
        of() {
          return Mounts.of<Manifest>()
        },
      },
      Backups: {
        volumes: (
          ...volumeNames: Array<Manifest["volumes"][number] & string>
        ) => Backups.withVolumes<Manifest>(...volumeNames),
        addSets: (
          ...options: BackupSync<Manifest["volumes"][number] & string>[]
        ) => Backups.withSyncs<Manifest>(...options),
        withOptions: (options?: Partial<SyncOptions>) =>
          Backups.withOptions<Manifest>(options),
      },
      InputSpec: {
        /**
         * @description Use this function to define the inputSpec specification that will ultimately present to the user as validated form inputs.
         *
         *   Most form controls are supported, including text, textarea, number, toggle, select, multiselect, list, color, datetime, object (sub form), and union (conditional sub form).
         * @example
         * In this example, we define a inputSpec form with two value: name and makePublic.
         *
         * ```
          import { sdk } from '../sdk'
          const { InputSpec, Value } = sdk
         
          export const inputSpecSpec = InputSpec.of({
            name: Value.text({
              name: 'Name',
              description:
                'When you launch the Hello World UI, it will display "Hello [Name]"',
              required: true,
              default: 'World'
            }),
            makePublic: Value.toggle({
              name: 'Make Public',
              description: 'Whether or not to expose the service to the network',
              default: false,
            }),
          })
         * ```
         */
        of: <
          Spec extends Record<string, Value<any, Store> | Value<any, never>>,
        >(
          spec: Spec,
        ) => InputSpec.of<Spec, Store>(spec),
      },
      Daemons: {
        of(
          effects: Effects,
          started: (onTerm: () => PromiseLike<void>) => PromiseLike<null>,
          healthChecks: HealthCheck[],
        ) {
          return Daemons.of<Manifest>({ effects, started, healthChecks })
        },
      },
      SubContainer: {
        of(
          effects: Effects,
          image: {
            imageId: T.ImageId & keyof Manifest["images"]
            sharedRun?: boolean
          },
          name: string,
        ) {
          return SubContainer.of(effects, image, name)
        },
        with<T>(
          effects: T.Effects,
          image: {
            imageId: T.ImageId & keyof Manifest["images"]
            sharedRun?: boolean
          },
          mounts: { options: MountOptions; mountpoint: string }[],
          name: string,
          fn: (subContainer: SubContainer) => Promise<T>,
        ): Promise<T> {
          return SubContainer.with(effects, image, mounts, name, fn)
        },
      },
      List: {
        /**
         * @description Create a list of text inputs.
         * @param a - attributes of the list itself.
         * @param aSpec - attributes describing each member of the list.
         */
        text: List.text,
        /**
         * @description Create a list of objects.
         * @param a - attributes of the list itself.
         * @param aSpec - attributes describing each member of the list.
         */
        obj: <Type extends Record<string, any>>(
          a: Parameters<typeof List.obj<Type, Store>>[0],
          aSpec: Parameters<typeof List.obj<Type, Store>>[1],
        ) => List.obj<Type, Store>(a, aSpec),
        /**
         * @description Create a list of dynamic text inputs.
         * @param a - attributes of the list itself.
         * @param aSpec - attributes describing each member of the list.
         */
        dynamicText: List.dynamicText<Store>,
      },
      StorePath: pathBuilder<Store>(),
      Value: {
        /**
         * @description Displays a boolean toggle to enable/disable
         * @example
         * ```
          toggleExample: Value.toggle({
            // required
            name: 'Toggle Example',
            default: true,
         
            // optional
            description: null,
            warning: null,
            immutable: false,
          }),
         * ```
         */
        toggle: Value.toggle,
        /**
         * @description Displays a text input field
         * @example
         * ```
          textExample: Value.text({
            // required
            name: 'Text Example',
            required: false,
            default: null,
         
            // optional
            description: null,
            placeholder: null,
            warning: null,
            generate: null,
            inputmode: 'text',
            masked: false,
            minLength: null,
            maxLength: null,
            patterns: [],
            immutable: false,
          }),
         * ```
         */
        text: Value.text,
        /**
         * @description Displays a large textarea field for long form entry.
         * @example
         * ```
          textareaExample: Value.textarea({
            // required
            name: 'Textarea Example',
            required: false,
            default: null,
         
            // optional
            description: null,
            placeholder: null,
            warning: null,
            minLength: null,
            maxLength: null,
            immutable: false,
          }),
         * ```
         */
        textarea: Value.textarea,
        /**
         * @description Displays a number input field
         * @example
         * ```
          numberExample: Value.number({
            // required
            name: 'Number Example',
            required: false,
            default: null,
            integer: true,
         
            // optional
            description: null,
            placeholder: null,
            warning: null,
            min: null,
            max: null,
            immutable: false,
            step: null,
            units: null,
          }),
         * ```
         */
        number: Value.number,
        /**
         * @description Displays a browser-native color selector.
         * @example
         * ```
          colorExample: Value.color({
            // required
            name: 'Color Example',
            required: false,
            default: null,
         
            // optional
            description: null,
            warning: null,
            immutable: false,
          }),
         * ```
         */
        color: Value.color,
        /**
         * @description Displays a browser-native date/time selector.
         * @example
         * ```
          datetimeExample: Value.datetime({
            // required
            name: 'Datetime Example',
            required: false,
            default: null,
         
            // optional
            description: null,
            warning: null,
            immutable: false,
            inputmode: 'datetime-local',
            min: null,
            max: null,
          }),
         * ```
         */
        datetime: Value.datetime,
        /**
         * @description Displays a select modal with radio buttons, allowing for a single selection.
         * @example
         * ```
          selectExample: Value.select({
            // required
            name: 'Select Example',
            default: 'radio1',
            values: {
              radio1: 'Radio 1',
              radio2: 'Radio 2',
            },
         
            // optional
            description: null,
            warning: null,
            immutable: false,
            disabled: false,
          }),
         * ```
         */
        select: Value.select,
        /**
         * @description Displays a select modal with checkboxes, allowing for multiple selections.
         * @example
         * ```
          multiselectExample: Value.multiselect({
            // required
            name: 'Multiselect Example',
            values: {
              option1: 'Option 1',
              option2: 'Option 2',
            },
            default: [],
         
            // optional
            description: null,
            warning: null,
            immutable: false,
            disabled: false,
            minlength: null,
            maxLength: null,
          }),
         * ```
         */
        multiselect: Value.multiselect,
        /**
         * @description Display a collapsable grouping of additional fields, a "sub form". The second value is the inputSpec spec for the sub form.
         * @example
         * ```
          objectExample: Value.object(
            {
              // required
              name: 'Object Example',
         
              // optional
              description: null,
              warning: null,
            },
            InputSpec.of({}),
          ),
         * ```
         */
        object: Value.object,
        /**
         * @description Displays a dropdown, allowing for a single selection. Depending on the selection, a different object ("sub form") is presented.
         * @example
         * ```
          unionExample: Value.union(
            {
              // required
              name: 'Union Example',
              default: 'option1',
         
              // optional
              description: null,
              warning: null,
              disabled: false,
              immutable: false,
            },
            Variants.of({
              option1: {
                name: 'Option 1',
                spec: InputSpec.of({}),
              },
              option2: {
                name: 'Option 2',
                spec: InputSpec.of({}),
              },
            }),
          ),
         * ```
         */
        union: Value.union,
        /**
         * @description Presents an interface to add/remove/edit items in a list.
         * @example
         * In this example, we create a list of text inputs.
         * 
         * ```
          listExampleText: Value.list(
            List.text(
              {
                // required
                name: 'Text List',

                // optional
                description: null,
                warning: null,
                default: [],
                minLength: null,
                maxLength: null,
              },
              {
                // required
                patterns: [],

                // optional
                placeholder: null,
                generate: null,
                inputmode: 'url',
                masked: false,
                minLength: null,
                maxLength: null,
              },
            ),
          ),
         * ```
         * @example
         * In this example, we create a list of objects.
         * 
         * ```
          listExampleObject: Value.list(
            List.obj(
              {
                // required
                name: 'Object List',

                // optional
                description: null,
                warning: null,
                default: [],
                minLength: null,
                maxLength: null,
              },
              {
                // required
                spec: InputSpec.of({}),

                // optional
                displayAs: null,
                uniqueBy: null,
              },
            ),
          ),
         * ```
         */
        list: Value.list,
        hidden: Value.hidden,
        dynamicToggle: Value.dynamicToggle<Store>,
        dynamicText: Value.dynamicText<Store>,
        dynamicTextarea: Value.dynamicTextarea<Store>,
        dynamicNumber: Value.dynamicNumber<Store>,
        dynamicColor: Value.dynamicColor<Store>,
        dynamicDatetime: Value.dynamicDatetime<Store>,
        dynamicSelect: Value.dynamicSelect<Store>,
        dynamicMultiselect: Value.dynamicMultiselect<Store>,
        filteredUnion: <
          VariantValues extends {
            [K in string]: {
              name: string
              spec: InputSpec<any, Store> | InputSpec<any, never>
            }
          },
        >(
          getDisabledFn: Parameters<
            typeof Value.filteredUnion<VariantValues, Store>
          >[0],
          a: Parameters<typeof Value.filteredUnion<VariantValues, Store>>[1],
          aVariants: Parameters<
            typeof Value.filteredUnion<VariantValues, Store>
          >[2],
        ) =>
          Value.filteredUnion<VariantValues, Store>(
            getDisabledFn,
            a,
            aVariants,
          ),

        dynamicUnion: <
          VariantValues extends {
            [K in string]: {
              name: string
              spec: InputSpec<any, Store> | InputSpec<any, never>
            }
          },
        >(
          getA: Parameters<typeof Value.dynamicUnion<VariantValues, Store>>[0],
          aVariants: Parameters<
            typeof Value.dynamicUnion<VariantValues, Store>
          >[1],
        ) => Value.dynamicUnion<VariantValues, Store>(getA, aVariants),
      },
      Variants: {
        of: <
          VariantValues extends {
            [K in string]: {
              name: string
              spec: InputSpec<any, Store>
            }
          },
        >(
          a: VariantValues,
        ) => Variants.of<VariantValues, Store>(a),
      },
    }
  }
}

export async function runCommand<Manifest extends T.SDKManifest>(
  effects: Effects,
  image: { imageId: keyof Manifest["images"] & T.ImageId; sharedRun?: boolean },
  command: T.CommandType,
  options: CommandOptions & {
    mounts?: { mountpoint: string; options: MountOptions }[]
  },
  name?: string,
): Promise<{ stdout: string | Buffer; stderr: string | Buffer }> {
  let commands: string[]
  if (command instanceof T.UseEntrypoint) {
    const imageMeta: T.ImageMetadata = await fs
      .readFile(`/media/startos/images/${image.imageId}.json`, {
        encoding: "utf8",
      })
      .catch(() => "{}")
      .then(JSON.parse)
    commands = imageMeta.entrypoint ?? []
    commands.concat(...(command.overridCmd ?? imageMeta.cmd ?? []))
  } else commands = splitCommand(command)
  return SubContainer.with(
    effects,
    image,
    options.mounts || [],
    name ||
      commands
        .map((c) => {
          if (c.includes(" ")) {
            return `"${c.replace(/"/g, `\"`)}"`
          } else {
            return c
          }
        })
        .join(" "),
    (subcontainer) => subcontainer.exec(commands),
  )
}
