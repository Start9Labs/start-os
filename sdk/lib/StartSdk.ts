import { RequiredDefault, Value } from "./config/builder/value"
import { Config, ExtractConfigType, LazyBuild } from "./config/builder/config"
import {
  DefaultString,
  ListValueSpecText,
  Pattern,
  RandomString,
  UniqueBy,
  ValueSpecDatetime,
  ValueSpecText,
} from "./config/configTypes"
import { Variants } from "./config/builder/variants"
import { CreatedAction, createAction } from "./actions/createAction"
import {
  ActionMetadata,
  Effects,
  ActionResult,
  BackupOptions,
  DeepPartial,
  MaybePromise,
  ServiceInterfaceId,
  PackageId,
} from "./types"
import * as patterns from "./util/patterns"
import { DependencyConfig, Update } from "./dependencies/DependencyConfig"
import { BackupSet, Backups } from "./backup/Backups"
import { smtpConfig } from "./config/configConstants"
import { Daemons } from "./mainFn/Daemons"
import { healthCheck, HealthCheckParams } from "./health/HealthCheck"
import { checkPortListening } from "./health/checkFns/checkPortListening"
import { checkWebUrl, runHealthScript } from "./health/checkFns"
import { List } from "./config/builder/list"
import { Install, InstallFn } from "./inits/setupInstall"
import { setupActions } from "./actions/setupActions"
import { setupDependencyConfig } from "./dependencies/setupDependencyConfig"
import { SetupBackupsParams, setupBackups } from "./backup/setupBackups"
import { setupInit } from "./inits/setupInit"
import { Uninstall, UninstallFn, setupUninstall } from "./inits/setupUninstall"
import { setupMain } from "./mainFn"
import { defaultTrigger } from "./trigger/defaultTrigger"
import { changeOnFirstSuccess, cooldownTrigger } from "./trigger"
import setupConfig, {
  DependenciesReceipt,
  Read,
  Save,
} from "./config/setupConfig"
import {
  InterfacesReceipt,
  SetInterfaces,
  setupInterfaces,
} from "./interfaces/setupInterfaces"
import { successFailure } from "./trigger/successFailure"
import { HealthReceipt } from "./health/HealthReceipt"
import { MultiHost, Scheme } from "./interfaces/Host"
import { ServiceInterfaceBuilder } from "./interfaces/ServiceInterfaceBuilder"
import { GetSystemSmtp } from "./util/GetSystemSmtp"
import nullIfEmpty from "./util/nullIfEmpty"
import { getServiceInterface } from "./util/getServiceInterface"
import { getServiceInterfaces } from "./util/getServiceInterfaces"
import { getStore } from "./store/getStore"
import { CommandOptions, MountOptions, Overlay } from "./util/Overlay"
import { splitCommand } from "./util/splitCommand"
import { Mounts } from "./mainFn/Mounts"
import { Dependency } from "./Dependency"
import * as T from "./types"
import { testTypeVersion } from "./exver"
import { ExposedStorePaths } from "./store/setupExposeStore"
import { PathBuilder, extractJsonPath, pathBuilder } from "./store/PathBuilder"
import {
  CheckDependencies,
  checkDependencies,
} from "./dependencies/dependencies"
import { GetSslCertificate } from "./util/GetSslCertificate"
import { VersionGraph } from "./version"

export const SDKVersion = testTypeVersion("0.3.6")

// prettier-ignore
type AnyNeverCond<T extends any[], Then, Else> = 
    T extends [] ? Else :
    T extends [never, ...Array<any>] ? Then :
    T extends [any, ...infer U] ? AnyNeverCond<U,Then, Else> :
    never

export type ServiceInterfaceType = "ui" | "p2p" | "api"
export type MainEffects = Effects & {
  _type: "main"
  clearCallbacks: () => Promise<void>
}
export type Signals = NodeJS.Signals
export const SIGTERM: Signals = "SIGTERM"
export const SIGKILL: Signals = "SIGKILL"
export const NO_TIMEOUT = -1

function removeCallbackTypes<E extends Effects>(effects: E) {
  return <T extends object>(t: T) => {
    if ("_type" in effects && effects._type === "main") {
      return t as E extends MainEffects ? T : Omit<T, "const" | "watch">
    } else {
      if ("const" in t) {
        delete t.const
      }
      if ("watch" in t) {
        delete t.watch
      }
      return t as E extends MainEffects ? T : Omit<T, "const" | "watch">
    }
  }
}

export class StartSdk<Manifest extends T.Manifest, Store> {
  private constructor(readonly manifest: Manifest) {}
  static of() {
    return new StartSdk<never, never>(null as never)
  }
  withManifest<Manifest extends T.Manifest = never>(manifest: Manifest) {
    return new StartSdk<Manifest, Store>(manifest)
  }
  withStore<Store extends Record<string, any>>() {
    return new StartSdk<Manifest, Store>(this.manifest)
  }

  build(isReady: AnyNeverCond<[Manifest, Store], "Build not ready", true>) {
    type DependencyType = {
      [K in keyof {
        [K in keyof Manifest["dependencies"]]: Manifest["dependencies"][K]["optional"] extends false
          ? K
          : never
      }]: Dependency
    } & {
      [K in keyof {
        [K in keyof Manifest["dependencies"]]: Manifest["dependencies"][K]["optional"] extends true
          ? K
          : never
      }]?: Dependency
    }

    return {
      checkDependencies: checkDependencies as <
        DependencyId extends keyof Manifest["dependencies"] &
          PackageId = keyof Manifest["dependencies"] & PackageId,
      >(
        effects: Effects,
        packageIds?: DependencyId[],
      ) => Promise<CheckDependencies<DependencyId>>,
      serviceInterface: {
        getOwn: <E extends Effects>(effects: E, id: ServiceInterfaceId) =>
          removeCallbackTypes<E>(effects)(
            getServiceInterface(effects, {
              id,
            }),
          ),
        get: <E extends Effects>(
          effects: E,
          opts: { id: ServiceInterfaceId; packageId: PackageId },
        ) =>
          removeCallbackTypes<E>(effects)(getServiceInterface(effects, opts)),
        getAllOwn: <E extends Effects>(effects: E) =>
          removeCallbackTypes<E>(effects)(getServiceInterfaces(effects, {})),
        getAll: <E extends Effects>(
          effects: E,
          opts: { packageId: PackageId },
        ) =>
          removeCallbackTypes<E>(effects)(getServiceInterfaces(effects, opts)),
      },

      store: {
        get: <E extends Effects, StoreValue = unknown>(
          effects: E,
          packageId: string,
          path: PathBuilder<Store, StoreValue>,
        ) =>
          removeCallbackTypes<E>(effects)(
            getStore<Store, StoreValue>(effects, path, {
              packageId,
            }),
          ),
        getOwn: <E extends Effects, StoreValue = unknown>(
          effects: E,
          path: PathBuilder<Store, StoreValue>,
        ) =>
          removeCallbackTypes<E>(effects)(
            getStore<Store, StoreValue>(effects, path),
          ),
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

      host: {
        // static: (effects: Effects, id: string) =>
        //   new StaticHost({ id, effects }),
        // single: (effects: Effects, id: string) =>
        //   new SingleHost({ id, effects }),
        multi: (effects: Effects, id: string) => new MultiHost({ id, effects }),
      },
      nullIfEmpty,
      runCommand: async <A extends string>(
        effects: Effects,
        image: {
          id: keyof Manifest["images"] & T.ImageId
          sharedRun?: boolean
        },
        command: T.CommandType,
        options: CommandOptions & {
          mounts?: { path: string; options: MountOptions }[]
        },
      ): Promise<{ stdout: string | Buffer; stderr: string | Buffer }> => {
        return runCommand<Manifest>(effects, image, command, options)
      },

      createAction: <
        ConfigType extends
          | Record<string, any>
          | Config<any, any>
          | Config<any, never>,
        Type extends Record<string, any> = ExtractConfigType<ConfigType>,
      >(
        id: string,
        metaData: Omit<ActionMetadata, "input"> & {
          input: Config<Type, Store> | Config<Type, never>
        },
        fn: (options: {
          effects: Effects
          input: Type
        }) => Promise<ActionResult>,
      ) => {
        const { input, ...rest } = metaData
        return createAction<Manifest, Store, ConfigType, Type>(
          id,
          rest,
          fn,
          input,
        )
      },
      configConstants: { smtpConfig },
      /**
       * @description Use this function to create a service interface.
       * @param effects
       * @param options
       * @example
       * In this example, we create a standard web UI
       *
       * ```
       * const ui = sdk.createInterface(effects, {
       *   name: 'Web UI',
       *   id: 'ui',
       *   description: 'The primary web app for this service.',
       *   type: 'ui',
       *   hasPrimary: false,
       *   masked: false,
       *   schemeOverride: null,
       *   username: null,
       *   path: '',
       *   search: {},
       * })
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
          /** Not available until StartOS v0.4.0. If true, forces the user to select one URL (i.e. .onion, .local, or IP address) as the primary URL. This is needed by some services to function properly. */
          hasPrimary: boolean
          /** Affects how the interface appears to the user. One of: 'ui', 'api', 'p2p'. */
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
        removeCallbackTypes<E>(effects)(new GetSystemSmtp(effects)),

      getSslCerificate: <E extends Effects>(
        effects: E,
        hostnames: string[],
        algorithm?: T.Algorithm,
      ) =>
        removeCallbackTypes<E>(effects)(
          new GetSslCertificate(effects, hostnames, algorithm),
        ),

      createDynamicAction: <
        ConfigType extends
          | Record<string, any>
          | Config<any, any>
          | Config<any, never>,
        Type extends Record<string, any> = ExtractConfigType<ConfigType>,
      >(
        id: string,
        metaData: (options: {
          effects: Effects
        }) => MaybePromise<Omit<ActionMetadata, "input">>,
        fn: (options: {
          effects: Effects
          input: Type
        }) => Promise<ActionResult>,
        input: Config<Type, Store> | Config<Type, never>,
      ) => {
        return createAction<Manifest, Store, ConfigType, Type>(
          id,
          metaData,
          fn,
          input,
        )
      },
      HealthCheck: {
        of(o: HealthCheckParams) {
          return healthCheck(o)
        },
      },
      Dependency: {
        of(data: Dependency["data"]) {
          return new Dependency({ ...data })
        },
      },
      healthCheck: {
        checkPortListening,
        checkWebUrl,
        runHealthScript,
      },
      patterns,
      setupActions: (...createdActions: CreatedAction<any, any, any>[]) =>
        setupActions<Manifest, Store>(...createdActions),
      setupBackups: (...args: SetupBackupsParams<Manifest>) =>
        setupBackups<Manifest>(this.manifest, ...args),
      setupConfig: <
        ConfigType extends Config<any, Store> | Config<any, never>,
        Type extends Record<string, any> = ExtractConfigType<ConfigType>,
      >(
        spec: ConfigType,
        write: Save<Type>,
        read: Read<Manifest, Store, Type>,
      ) => setupConfig<Store, ConfigType, Manifest, Type>(spec, write, read),
      setupConfigRead: <
        ConfigSpec extends
          | Config<Record<string, any>, any>
          | Config<Record<string, never>, never>,
      >(
        _configSpec: ConfigSpec,
        fn: Read<Manifest, Store, ConfigSpec>,
      ) => fn,
      setupConfigSave: <
        ConfigSpec extends
          | Config<Record<string, any>, any>
          | Config<Record<string, never>, never>,
      >(
        _configSpec: ConfigSpec,
        fn: Save<ConfigSpec>,
      ) => fn,
      setupDependencyConfig: <Input extends Record<string, any>>(
        config: Config<Input, Store> | Config<Input, never>,
        autoConfigs: {
          [K in keyof Manifest["dependencies"]]: DependencyConfig<
            Manifest,
            Store,
            Input,
            any
          > | null
        },
      ) => setupDependencyConfig<Store, Input, Manifest>(config, autoConfigs),
      setupDependencies: <Input extends Record<string, any>>(
        fn: (options: {
          effects: Effects
          input: Input | null
        }) => Promise<DependencyType>,
      ) => {
        return async (options: { effects: Effects; input: Input }) => {
          const dependencyType = await fn(options)
          return await options.effects.setDependencies({
            dependencies: Object.entries(dependencyType).map(
              ([
                id,
                {
                  data: { versionRange, ...x },
                },
              ]) => ({
                id,
                ...x,
                ...(x.type === "running"
                  ? {
                      kind: "running",
                      healthChecks: x.healthChecks,
                    }
                  : {
                      kind: "exists",
                    }),
                versionRange: versionRange.toString(),
              }),
            ),
          })
        }
      },
      setupInit: (
        versions: VersionGraph<Manifest["version"]>,
        install: Install<Manifest, Store>,
        uninstall: Uninstall<Manifest, Store>,
        setInterfaces: SetInterfaces<Manifest, Store, any, any>,
        setDependencies: (options: {
          effects: Effects
          input: any
        }) => Promise<DependenciesReceipt>,
        exposedStore: ExposedStorePaths,
      ) =>
        setupInit<Manifest, Store>(
          versions,
          install,
          uninstall,
          setInterfaces,
          setDependencies,
          exposedStore,
        ),
      setupInstall: (fn: InstallFn<Manifest, Store>) => Install.of(fn),
      /**
       * @description Use this function to determine how this service will be hosted and served. The function executes on service install, service update, and config save.
       *
       *   To learn about creating multi-hosts and interfaces, check out the {@link https://docs.start9.com/packaging-guide/learn/interfaces documentation}.
       * @param config - The config spec of this service as exported from /config/spec.
       * @param fn - an async function that returns an array of interface receipts. The function always has access to `effects`; it has access to `input` only after config save, otherwise `input` will be null.
       * @example
       * In this example, we create two UIs from one multi-host, and one API from another multi-host.
       *
       * ```
       * export const setInterfaces = sdk.setupInterfaces(
       *   configSpec,
       *   async ({ effects, input }) => {
       *     // ** UI multi-host **
       *     const uiMulti = sdk.host.multi(effects, 'ui-multi')
       *     const uiMultiOrigin = await uiMulti.bindPort(80, {
       *       protocol: 'http',
       *     })
       *     // Primary UI
       *     const primaryUi = sdk.createInterface(effects, {
       *       name: 'Primary UI',
       *       id: 'primary-ui',
       *       description: 'The primary web app for this service.',
       *       type: 'ui',
       *       hasPrimary: false,
       *       masked: false,
       *       schemeOverride: null,
       *       username: null,
       *       path: '',
       *       search: {},
       *     })
       *     // Admin UI
       *     const adminUi = sdk.createInterface(effects, {
       *       name: 'Admin UI',
       *       id: 'admin-ui',
       *       description: 'The admin web app for this service.',
       *       type: 'ui',
       *       hasPrimary: false,
       *       masked: false,
       *       schemeOverride: null,
       *       username: null,
       *       path: '/admin',
       *       search: {},
       *     })
       *     // UI receipt
       *     const uiReceipt = await uiMultiOrigin.export([primaryUi, adminUi])
       *
       *     // ** API multi-host **
       *     const apiMulti = sdk.host.multi(effects, 'api-multi')
       *     const apiMultiOrigin = await apiMulti.bindPort(5959, {
       *       protocol: 'http',
       *     })
       *     // API
       *     const api = sdk.createInterface(effects, {
       *       name: 'Admin API',
       *       id: 'api',
       *       description: 'The advanced API for this service.',
       *       type: 'api',
       *       hasPrimary: false,
       *       masked: false,
       *       schemeOverride: null,
       *       username: null,
       *       path: '',
       *       search: {},
       *     })
       *     // API receipt
       *     const apiReceipt = await apiMultiOrigin.export([api])
       *
       *     // ** Return receipts **
       *     return [uiReceipt, apiReceipt]
       *   },
       * )
       * ```
       */
      setupInterfaces: <
        ConfigInput extends Record<string, any>,
        Output extends InterfacesReceipt,
      >(
        config: Config<ConfigInput, Store>,
        fn: SetInterfaces<Manifest, Store, ConfigInput, Output>,
      ) => setupInterfaces(config, fn),
      setupMain: (
        fn: (o: {
          effects: MainEffects
          started(onTerm: () => PromiseLike<void>): PromiseLike<void>
        }) => Promise<Daemons<Manifest, any>>,
      ) => setupMain<Manifest, Store>(fn),
      /**
       * @description Use this function to determine which information to expose to the UI in the "Properties" section.
       *
       *   Values can be obtained from anywhere: the Store, the upstream service, or another service.
       * @example
       * In this example, we retrieve the admin password from the Store and expose it, masked and copyable, to
       * the UI as "Admin Password".
       *
       * ```
       * export const properties = sdk.setupProperties(async ({ effects }) => {
       *   const store = await sdk.store.getOwn(effects, sdk.StorePath).once()
       *
       *   return {
       *     'Admin Password': {
       *       type: 'string',
       *       value: store.adminPassword,
       *       description: 'Used for logging into the admin UI',
       *       copyable: true,
       *       masked: true,
       *       qr: false,
       *     },
       *   }
       * })
       * ```
       */
      setupProperties:
        (
          fn: (options: { effects: Effects }) => Promise<T.SdkPropertiesReturn>,
        ): T.ExpectedExports.properties =>
        (options) =>
          fn(options).then(nullifyProperties),
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
        ) => Backups.volumes<Manifest>(...volumeNames),
        addSets: (
          ...options: BackupSet<Manifest["volumes"][number] & string>[]
        ) => Backups.addSets<Manifest>(...options),
        withOptions: (options?: Partial<BackupOptions>) =>
          Backups.with_options<Manifest>(options),
      },
      Config: {
        of: <
          Spec extends Record<string, Value<any, Store> | Value<any, never>>,
        >(
          spec: Spec,
        ) => Config.of<Spec, Store>(spec),
      },
      Daemons: {
        of(config: {
          effects: Effects
          started: (onTerm: () => PromiseLike<void>) => PromiseLike<void>
          healthReceipts: HealthReceipt[]
        }) {
          return Daemons.of<Manifest>(config)
        },
      },
      DependencyConfig: {
        of<
          LocalConfig extends Record<string, any>,
          RemoteConfig extends Record<string, any>,
        >({
          localConfigSpec,
          remoteConfigSpec,
          dependencyConfig,
          update,
        }: {
          localConfigSpec:
            | Config<LocalConfig, Store>
            | Config<LocalConfig, never>
          remoteConfigSpec:
            | Config<RemoteConfig, any>
            | Config<RemoteConfig, never>
          dependencyConfig: (options: {
            effects: Effects
            localConfig: LocalConfig
          }) => Promise<void | DeepPartial<RemoteConfig>>
          update?: Update<void | DeepPartial<RemoteConfig>, RemoteConfig>
        }) {
          return new DependencyConfig<
            Manifest,
            Store,
            LocalConfig,
            RemoteConfig
          >(dependencyConfig, update)
        },
      },
      List: {
        text: List.text,
        obj: <Type extends Record<string, any>>(
          a: {
            name: string
            description?: string | null
            warning?: string | null
            /** Default [] */
            default?: []
            minLength?: number | null
            maxLength?: number | null
          },
          aSpec: {
            spec: Config<Type, Store>
            displayAs?: null | string
            uniqueBy?: null | UniqueBy
          },
        ) => List.obj<Type, Store>(a, aSpec),
        dynamicText: (
          getA: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              /** Default = [] */
              default?: string[]
              minLength?: number | null
              maxLength?: number | null
              disabled?: false | string
              generate?: null | RandomString
              spec: {
                /** Default = false */
                masked?: boolean
                placeholder?: string | null
                minLength?: number | null
                maxLength?: number | null
                patterns: Pattern[]
                /** Default = "text" */
                inputmode?: ListValueSpecText["inputmode"]
              }
            }
          >,
        ) => List.dynamicText<Store>(getA),
      },
      StorePath: pathBuilder<Store>(),
      Value: {
        toggle: Value.toggle,
        text: Value.text,
        textarea: Value.textarea,
        number: Value.number,
        color: Value.color,
        datetime: Value.datetime,
        select: Value.select,
        multiselect: Value.multiselect,
        object: Value.object,
        union: Value.union,
        list: Value.list,
        dynamicToggle: (
          a: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              default: boolean
              disabled?: false | string
            }
          >,
        ) => Value.dynamicToggle<Store>(a),
        dynamicText: (
          getA: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              required: RequiredDefault<DefaultString>

              /** Default = false */
              masked?: boolean
              placeholder?: string | null
              minLength?: number | null
              maxLength?: number | null
              patterns?: Pattern[]
              /** Default = 'text' */
              inputmode?: ValueSpecText["inputmode"]
              generate?: null | RandomString
            }
          >,
        ) => Value.dynamicText<Store>(getA),
        dynamicTextarea: (
          getA: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              required: boolean
              minLength?: number | null
              maxLength?: number | null
              placeholder?: string | null
              disabled?: false | string
              generate?: null | RandomString
            }
          >,
        ) => Value.dynamicTextarea<Store>(getA),
        dynamicNumber: (
          getA: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              required: RequiredDefault<number>
              min?: number | null
              max?: number | null
              /** Default = '1' */
              step?: number | null
              integer: boolean
              units?: string | null
              placeholder?: string | null
              disabled?: false | string
            }
          >,
        ) => Value.dynamicNumber<Store>(getA),
        dynamicColor: (
          getA: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              required: RequiredDefault<string>

              disabled?: false | string
            }
          >,
        ) => Value.dynamicColor<Store>(getA),
        dynamicDatetime: (
          getA: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              required: RequiredDefault<string>
              /** Default = 'datetime-local' */
              inputmode?: ValueSpecDatetime["inputmode"]
              min?: string | null
              max?: string | null
              disabled?: false | string
            }
          >,
        ) => Value.dynamicDatetime<Store>(getA),
        dynamicSelect: (
          getA: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              required: RequiredDefault<string>
              values: Record<string, string>
              disabled?: false | string
            }
          >,
        ) => Value.dynamicSelect<Store>(getA),
        dynamicMultiselect: (
          getA: LazyBuild<
            Store,
            {
              name: string
              description?: string | null
              warning?: string | null
              default: string[]
              values: Record<string, string>
              minLength?: number | null
              maxLength?: number | null
              disabled?: false | string
            }
          >,
        ) => Value.dynamicMultiselect<Store>(getA),
        filteredUnion: <
          Required extends RequiredDefault<string>,
          Type extends Record<string, any>,
        >(
          getDisabledFn: LazyBuild<Store, string[]>,
          a: {
            name: string
            description?: string | null
            warning?: string | null
            required: Required
          },
          aVariants: Variants<Type, Store> | Variants<Type, never>,
        ) =>
          Value.filteredUnion<Required, Type, Store>(
            getDisabledFn,
            a,
            aVariants,
          ),

        dynamicUnion: <
          Required extends RequiredDefault<string>,
          Type extends Record<string, any>,
        >(
          getA: LazyBuild<
            Store,
            {
              disabled: string[] | false | string
              name: string
              description?: string | null
              warning?: string | null
              required: Required
            }
          >,
          aVariants: Variants<Type, Store> | Variants<Type, never>,
        ) => Value.dynamicUnion<Required, Type, Store>(getA, aVariants),
      },
      Variants: {
        of: <
          VariantValues extends {
            [K in string]: {
              name: string
              spec: Config<any, Store>
            }
          },
        >(
          a: VariantValues,
        ) => Variants.of<VariantValues, Store>(a),
      },
    }
  }
}

export async function runCommand<Manifest extends T.Manifest>(
  effects: Effects,
  image: { id: keyof Manifest["images"] & T.ImageId; sharedRun?: boolean },
  command: string | [string, ...string[]],
  options: CommandOptions & {
    mounts?: { path: string; options: MountOptions }[]
  },
): Promise<{ stdout: string | Buffer; stderr: string | Buffer }> {
  const commands = splitCommand(command)
  return Overlay.with(effects, image, options.mounts || [], (overlay) =>
    overlay.exec(commands),
  )
}
function nullifyProperties(value: T.SdkPropertiesReturn): T.PropertiesReturn {
  return Object.fromEntries(
    Object.entries(value).map(([k, v]) => [k, nullifyProperties_(v)]),
  )
}
function nullifyProperties_(value: T.SdkPropertiesValue): T.PropertiesValue {
  if (value.type === "string") {
    return {
      description: null,
      copyable: null,
      masked: null,
      qr: null,
      ...value,
    }
  }
  return {
    description: null,
    ...value,
    value: Object.fromEntries(
      Object.entries(value.value).map(([k, v]) => [k, nullifyProperties_(v)]),
    ),
  }
}
