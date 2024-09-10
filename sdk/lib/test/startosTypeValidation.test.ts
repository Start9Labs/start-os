import { Effects } from "../types"
import {
  CheckDependenciesParam,
  ClearActionsParams,
  ClearBindingsParams,
  ClearServiceInterfacesParams,
  ExecuteAction,
  GetInputSpecuredParams,
  GetStoreParams,
  SetDataVersionParams,
  SetMainStatus,
  SetStoreParams,
} from ".././osBindings"
import { CreateSubcontainerFsParams } from ".././osBindings"
import { DestroySubcontainerFsParams } from ".././osBindings"
import { BindParams } from ".././osBindings"
import { GetHostInfoParams } from ".././osBindings"
import { SetInputSpecured } from ".././osBindings"
import { SetHealth } from ".././osBindings"
import { ExposeForDependentsParams } from ".././osBindings"
import { GetSslCertificateParams } from ".././osBindings"
import { GetSslKeyParams } from ".././osBindings"
import { GetServiceInterfaceParams } from ".././osBindings"
import { SetDependenciesParams } from ".././osBindings"
import { GetSystemSmtpParams } from ".././osBindings"
import { GetServicePortForwardParams } from ".././osBindings"
import { ExportServiceInterfaceParams } from ".././osBindings"
import { GetPrimaryUrlParams } from ".././osBindings"
import { ListServiceInterfacesParams } from ".././osBindings"
import { ExportActionParams } from ".././osBindings"
import { MountParams } from ".././osBindings"
import { StringObject } from "../util"
function typeEquality<ExpectedType>(_a: ExpectedType) {}

type WithCallback<T> = Omit<T, "callback"> & { callback: () => void }

type EffectsTypeChecker<T extends StringObject = Effects> = {
  [K in keyof T]: T[K] extends (args: infer A) => any
    ? A
    : T[K] extends StringObject
      ? EffectsTypeChecker<T[K]>
      : never
}

describe("startosTypeValidation ", () => {
  test(`checking the params match`, () => {
    const testInput: any = {}
    typeEquality<EffectsTypeChecker>({
      executeAction: {} as ExecuteAction,
      subcontainer: {
        createFs: {} as CreateSubcontainerFsParams,
        destroyFs: {} as DestroySubcontainerFsParams,
      },
      clearBindings: {} as ClearBindingsParams,
      getInstalledPackages: undefined,
      bind: {} as BindParams,
      getHostInfo: {} as WithCallback<GetHostInfoParams>,
      getInputSpecured: {} as GetInputSpecuredParams,
      restart: undefined,
      shutdown: undefined,
      setInputSpecured: {} as SetInputSpecured,
      setDataVersion: {} as SetDataVersionParams,
      getDataVersion: undefined,
      setHealth: {} as SetHealth,
      exposeForDependents: {} as ExposeForDependentsParams,
      getSslCertificate: {} as WithCallback<GetSslCertificateParams>,
      getSslKey: {} as GetSslKeyParams,
      getServiceInterface: {} as WithCallback<GetServiceInterfaceParams>,
      setDependencies: {} as SetDependenciesParams,
      store: {
        get: {} as any, // as GetStoreParams,
        set: {} as any, // as SetStoreParams,
      },
      getSystemSmtp: {} as WithCallback<GetSystemSmtpParams>,
      getContainerIp: undefined,
      getServicePortForward: {} as GetServicePortForwardParams,
      clearServiceInterfaces: {} as ClearServiceInterfacesParams,
      exportServiceInterface: {} as ExportServiceInterfaceParams,
      getPrimaryUrl: {} as WithCallback<GetPrimaryUrlParams>,
      listServiceInterfaces: {} as WithCallback<ListServiceInterfacesParams>,
      exportAction: {} as ExportActionParams,
      clearActions: {} as ClearActionsParams,
      mount: {} as MountParams,
      checkDependencies: {} as CheckDependenciesParam,
      getDependencies: undefined,
      setMainStatus: {} as SetMainStatus,
    })
    typeEquality<Parameters<Effects["executeAction"]>[0]>(
      testInput as ExecuteAction,
    )
  })
})
