import { ManifestVersion, SDKManifest } from "./manifest/ManifestTypes"
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
} from "./types"
import * as patterns from "./util/patterns"
import { Utils } from "./util/utils"
import { DependencyConfig, Update } from "./dependencyConfig/DependencyConfig"
import { BackupSet, Backups } from "./backup/Backups"
import { smtpConfig } from "./config/configConstants"
import { Daemons } from "./mainFn/Daemons"
import { healthCheck } from "./health/HealthCheck"
import { checkPortListening } from "./health/checkFns/checkPortListening"
import { checkWebUrl, runHealthScript } from "./health/checkFns"
import { List } from "./config/builder/list"
import { Migration } from "./inits/migrations/Migration"
import { Install, InstallFn } from "./inits/setupInstall"
import { setupActions } from "./actions/setupActions"
import { setupDependencyConfig } from "./dependencyConfig/setupDependencyConfig"
import { SetupBackupsParams, setupBackups } from "./backup/setupBackups"
import { setupInit } from "./inits/setupInit"
import {
  EnsureUniqueId,
  Migrations,
  setupMigrations,
} from "./inits/migrations/setupMigrations"
import { Uninstall, UninstallFn, setupUninstall } from "./inits/setupUninstall"
import { setupMain } from "./mainFn"
import { defaultTrigger } from "./trigger/defaultTrigger"
import { changeOnFirstSuccess, cooldownTrigger } from "./trigger"
import setupConfig, { Read, Save } from "./config/setupConfig"
import { setupDependencyMounts } from "./dependency/setupDependencyMounts"
import {
  InterfacesReceipt,
  SetInterfaces,
  setupInterfaces,
} from "./interfaces/setupInterfaces"
import { successFailure } from "./trigger/successFailure"
import { SetupExports } from "./inits/setupExports"

// prettier-ignore
type AnyNeverCond<T extends any[], Then, Else> = 
    T extends [] ? Else :
    T extends [never, ...Array<any>] ? Then :
    T extends [any, ...infer U] ? AnyNeverCond<U,Then, Else> :
    never

export class StartSdk<Manifest extends SDKManifest, Store> {
  private constructor(readonly manifest: Manifest) {}
  static of() {
    return new StartSdk<never, never>(null as never)
  }
  withManifest<Manifest extends SDKManifest = never>(manifest: Manifest) {
    return new StartSdk<Manifest, Store>(manifest)
  }
  withStore<Store extends Record<string, any>>() {
    return new StartSdk<Manifest, Store>(this.manifest)
  }

  build(isReady: AnyNeverCond<[Manifest, Store], "Build not ready", true>) {
    return {
      configConstants: { smtpConfig },
      createAction: <
        ConfigType extends
          | Record<string, any>
          | Config<any, any>
          | Config<any, never>,
        Type extends Record<string, any> = ExtractConfigType<ConfigType>,
      >(
        metaData: Omit<ActionMetadata, "input"> & {
          input: Config<Type, Store> | Config<Type, never>
        },
        fn: (options: {
          effects: Effects
          utils: Utils<Manifest, Store>
          input: Type
        }) => Promise<ActionResult>,
      ) => {
        const { input, ...rest } = metaData
        return createAction<Manifest, Store, ConfigType, Type>(rest, fn, input)
      },
      createDynamicAction: <
        ConfigType extends
          | Record<string, any>
          | Config<any, any>
          | Config<any, never>,
        Type extends Record<string, any> = ExtractConfigType<ConfigType>,
      >(
        metaData: (options: {
          effects: Effects
          utils: Utils<Manifest, Store>
        }) => MaybePromise<Omit<ActionMetadata, "input">>,
        fn: (options: {
          effects: Effects
          utils: Utils<Manifest, Store>
          input: Type
        }) => Promise<ActionResult>,
        input: Config<Type, Store> | Config<Type, never>,
      ) => {
        return createAction<Manifest, Store, ConfigType, Type>(
          metaData,
          fn,
          input,
        )
      },

      HealthCheck: {
        of: healthCheck,
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
        setupBackups<Manifest>(...args),
      setupConfig: <
        ConfigType extends Config<any, Store> | Config<any, never>,
        Type extends Record<string, any> = ExtractConfigType<ConfigType>,
      >(
        spec: ConfigType,
        write: Save<Store, Type, Manifest>,
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
        fn: Save<Store, ConfigSpec, Manifest>,
      ) => fn,
      setupDependencyConfig: <Input extends Record<string, any>>(
        config: Config<Input, Store> | Config<Input, never>,
        autoConfigs: {
          [K in keyof Manifest["dependencies"]]: DependencyConfig<
            Manifest,
            Store,
            Input,
            any
          >
        },
      ) => setupDependencyConfig<Store, Input, Manifest>(config, autoConfigs),
      setupExports: (fn: SetupExports<Store>) => fn,
      setupDependencyMounts,
      setupInit: (
        migrations: Migrations<Manifest, Store>,
        install: Install<Manifest, Store>,
        uninstall: Uninstall<Manifest, Store>,
        setInterfaces: SetInterfaces<Manifest, Store, any, any>,
        setupExports: SetupExports<Store>,
      ) =>
        setupInit<Manifest, Store>(
          migrations,
          install,
          uninstall,
          setInterfaces,
          setupExports,
        ),
      setupInstall: (fn: InstallFn<Manifest, Store>) => Install.of(fn),
      setupInterfaces: <
        ConfigInput extends Record<string, any>,
        Output extends InterfacesReceipt,
      >(
        config: Config<ConfigInput, Store>,
        fn: SetInterfaces<Manifest, Store, ConfigInput, Output>,
      ) => setupInterfaces(config, fn),
      setupMain: (
        fn: (o: {
          effects: Effects
          started(onTerm: () => PromiseLike<void>): PromiseLike<void>
          utils: Utils<Manifest, Store, {}>
        }) => Promise<Daemons<Manifest, any>>,
      ) => setupMain<Manifest, Store>(fn),
      setupMigrations: <
        Migrations extends Array<Migration<Manifest, Store, any>>,
      >(
        ...migrations: EnsureUniqueId<Migrations>
      ) =>
        setupMigrations<Manifest, Store, Migrations>(
          this.manifest,
          ...migrations,
        ),
      setupUninstall: (fn: UninstallFn<Manifest, Store>) =>
        setupUninstall<Manifest, Store>(fn),
      trigger: {
        defaultTrigger,
        cooldownTrigger,
        changeOnFirstSuccess,
        successFailure,
      },

      Backups: {
        volumes: (...volumeNames: Array<keyof Manifest["volumes"] & string>) =>
          Backups.volumes<Manifest>(...volumeNames),
        addSets: (
          ...options: BackupSet<keyof Manifest["volumes"] & string>[]
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
      Daemons: { of: Daemons.of },
      DependencyConfig: {
        of<
          LocalConfig extends Record<string, any>,
          RemoteConfig extends Record<string, any>,
        >({
          localConfig,
          remoteConfig,
          dependencyConfig,
          update,
        }: {
          localConfig: Config<LocalConfig, Store> | Config<LocalConfig, never>
          remoteConfig: Config<RemoteConfig, any> | Config<RemoteConfig, never>
          dependencyConfig: (options: {
            effects: Effects
            localConfig: LocalConfig
            utils: Utils<Manifest, Store>
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
        number: List.number,
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
        dynamicNumber: (
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
              spec: {
                integer: boolean
                min?: number | null
                max?: number | null
                step?: number | null
                units?: string | null
                placeholder?: string | null
              }
            }
          >,
        ) => List.dynamicNumber<Store>(getA),
      },
      Migration: {
        of: <Version extends ManifestVersion>(options: {
          version: Version
          up: (opts: {
            effects: Effects
            utils: Utils<Manifest, Store>
          }) => Promise<void>
          down: (opts: {
            effects: Effects
            utils: Utils<Manifest, Store>
          }) => Promise<void>
        }) => Migration.of<Manifest, Store, Version>(options),
      },
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
