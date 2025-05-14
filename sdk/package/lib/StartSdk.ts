import { Value } from "../../base/lib/actions/input/builder/value"
import { InputSpec } from "../../base/lib/actions/input/builder/inputSpec"
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
import { Daemon, Daemons } from "./mainFn/Daemons"
import { HealthCheck } from "./health/HealthCheck"
import { checkPortListening } from "./health/checkFns/checkPortListening"
import { checkWebUrl, runHealthScript } from "./health/checkFns"
import { List } from "../../base/lib/actions/input/builder/list"
import { InstallFn, PostInstall, PreInstall } from "./inits/setupInstall"
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
import {
  CommandOptions,
  ExitError,
  SubContainer,
  SubContainerOwned,
} from "./util/SubContainer"
import { splitCommand } from "./util"
import { Mounts } from "./mainFn/Mounts"
import { setupDependencies } from "../../base/lib/dependencies/setupDependencies"
import * as T from "../../base/lib/types"
import { testTypeVersion } from "../../base/lib/exver"
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

export const OSVersion = testTypeVersion("0.4.0-alpha.3")

// prettier-ignore
type AnyNeverCond<T extends any[], Then, Else> = 
    T extends [] ? Else :
    T extends [never, ...Array<any>] ? Then :
    T extends [any, ...infer U] ? AnyNeverCond<U,Then, Else> :
    never

export class StartSdk<Manifest extends T.SDKManifest> {
  private constructor(readonly manifest: Manifest) {}
  static of() {
    return new StartSdk<never>(null as never)
  }
  withManifest<Manifest extends T.SDKManifest = never>(manifest: Manifest) {
    return new StartSdk<Manifest>(manifest)
  }

  build(isReady: AnyNeverCond<[Manifest], "Build not ready", true>) {
    type NestedEffects = "subcontainer" | "store" | "action"
    type InterfaceEffects =
      | "getServiceInterface"
      | "listServiceInterfaces"
      | "exportServiceInterface"
      | "clearServiceInterfaces"
      | "bind"
      | "getHostInfo"
    type MainUsedEffects = "setMainStatus" | "setHealth"
    type CallbackEffects =
      | "child"
      | "constRetry"
      | "isInContext"
      | "onLeaveContext"
      | "clearCallbacks"
    type AlreadyExposed =
      | "getSslCertificate"
      | "getSystemSmtp"
      | "getContainerIp"

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
      getServicePortForward: (effects, ...args) =>
        effects.getServicePortForward(...args),
      clearBindings: (effects, ...args) => effects.clearBindings(...args),
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
        request: <T extends Action<T.ActionId, any>>(
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
        requestOwn: <T extends Action<T.ActionId, any>>(
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
      getContainerIp: (
        effects: T.Effects,
        options: Omit<
          Parameters<T.Effects["getContainerIp"]>[0],
          "callback"
        > = {},
      ) => {
        async function* watch() {
          const resolveCell = { resolve: () => {} }
          effects.onLeaveContext(() => {
            resolveCell.resolve()
          })
          while (effects.isInContext) {
            let callback: () => void = () => {}
            const waitForNext = new Promise<void>((resolve) => {
              callback = resolve
              resolveCell.resolve = resolve
            })
            yield await effects.getContainerIp({ ...options, callback })
            await waitForNext
          }
        }
        return {
          const: () =>
            effects.getContainerIp({
              ...options,
              callback:
                effects.constRetry &&
                (() => effects.constRetry && effects.constRetry()),
            }),
          once: () => effects.getContainerIp(options),
          watch,
          onChange: (
            callback: (
              value: string | null,
              error?: Error,
            ) => void | Promise<void>,
          ) => {
            ;(async () => {
              for await (const value of watch()) {
                try {
                  await callback(value)
                } catch (e) {
                  console.error(
                    "callback function threw an error @ getContainerIp.onChange",
                    e,
                  )
                }
              }
            })()
              .catch((e) => callback(null, e))
              .catch((e) =>
                console.error(
                  "callback function threw an error @ getContainerIp.onChange",
                  e,
                ),
              )
          },
        }
      },

      MultiHost: {
        of: (effects: Effects, id: string) => new MultiHost({ id, effects }),
      },
      nullIfEmpty,
      useEntrypoint: (overrideCmd?: string[]) =>
        new T.UseEntrypoint(overrideCmd),
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
          InputSpecType extends Record<string, any> | InputSpec<any>,
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
          import { store } from '../file-models/store.json'
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
                value: (await store.read.once())?.secretPhrase,
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
      HealthCheck,
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
      Actions: Actions<{}>,
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
       * In this example, we create a dependency on Hello World >=1.0.0:0, where Hello World must be running and passing its "primary" health check.
       *
       * ```
        export const setDependencies = sdk.setupDependencies(
          async ({ effects }) => {
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
       */
      setupDependencies: setupDependencies<Manifest>,
      setupInit: setupInit<Manifest>,
      /**
       * @description Use this function to execute arbitrary logic *once*, on initial install *before* interfaces, actions, and dependencies are updated.
       * @example
       * In the this example, we initialize a config file
       *
       * ```
        const preInstall = sdk.setupPreInstall(async ({ effects }) => {
          await configFile.write(effects, { name: 'World' })
        })
       * ```
       */
      setupPreInstall: (fn: InstallFn<Manifest>) => PreInstall.of(fn),
      /**
       * @description Use this function to execute arbitrary logic *once*, on initial install *after* interfaces, actions, and dependencies are updated.
       * @example
       * In the this example, we create a task for the user to perform.
       *
       * ```
        const postInstall = sdk.setupPostInstall(async ({ effects }) => {
          await sdk.action.requestOwn(effects, showSecretPhrase, 'important', {
            reason: 'Check out your secret phrase!',
          })
        })
       * ```
       */
      setupPostInstall: (fn: InstallFn<Manifest>) => PostInstall.of(fn),
      /**
       * @description Use this function to determine how this service will be hosted and served. The function executes on service install, service update, and inputSpec save.
       * @param inputSpec - The inputSpec spec of this service as exported from /inputSpec/spec.
       * @param fn - an async function that returns an array of interface receipts. The function always has access to `effects`; it has access to `input` only after inputSpec save, otherwise `input` will be null.
       * @example
       * In this example, we create two UIs from one multi-host, and one API from another multi-host.
       *
       * ```
        export const setInterfaces = sdk.setupInterfaces(
          async ({ effects }) => {
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
      ) => setupMain<Manifest>(fn),
      /**
       * Use this function to execute arbitrary logic *once*, on uninstall only. Most services will not use this.
       */
      setupUninstall: (fn: UninstallFn<Manifest>) =>
        setupUninstall<Manifest>(fn),
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
        of: <Spec extends Record<string, Value<any>>>(spec: Spec) =>
          InputSpec.of<Spec>(spec),
      },
      Daemon: {
        get of() {
          return Daemon.of<Manifest>()
        },
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
        /**
         * @description Create a new SubContainer
         * @param effects
         * @param image - what container image to use
         * @param mounts - what to mount to the subcontainer
         * @param name - a name to use to refer to the subcontainer for debugging purposes
         */
        of(
          effects: Effects,
          image: {
            imageId: T.ImageId & keyof Manifest["images"]
            sharedRun?: boolean
          },
          mounts: Mounts<Manifest> | null,
          name: string,
        ) {
          return SubContainerOwned.of(effects, image, mounts, name)
        },
        /**
         * @description Run a function with a temporary SubContainer
         * @param effects
         * @param image - what container image to use
         * @param mounts - what to mount to the subcontainer
         * @param name - a name to use to refer to the ephemeral subcontainer for debugging purposes
         */
        withTemp<T>(
          effects: T.Effects,
          image: {
            imageId: T.ImageId & keyof Manifest["images"]
            sharedRun?: boolean
          },
          mounts: Mounts<Manifest> | null,
          name: string,
          fn: (subContainer: SubContainer<Manifest>) => Promise<T>,
        ): Promise<T> {
          return SubContainerOwned.withTemp(effects, image, mounts, name, fn)
        },
      },
      List,
      Value,
      Variants,
    }
  }
}

export async function runCommand<Manifest extends T.SDKManifest>(
  effects: Effects,
  image: { imageId: keyof Manifest["images"] & T.ImageId; sharedRun?: boolean },
  command: T.CommandType,
  options: CommandOptions & {
    mounts: Mounts<Manifest> | null
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
    commands = commands.concat(...(command.overridCmd ?? imageMeta.cmd ?? []))
  } else commands = splitCommand(command)
  return SubContainerOwned.withTemp(
    effects,
    image,
    options.mounts,
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
    async (subcontainer) => {
      const res = await subcontainer.exec(commands)
      if (res.exitCode || res.exitSignal) {
        throw new ExitError(commands[0], res)
      } else {
        return res
      }
    },
  )
}
