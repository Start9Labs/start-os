import { Effects } from "../types"
import {
  CheckDependenciesParam,
  ExecuteAction,
  SetMainStatus,
} from ".././osBindings"
import { CreateOverlayedImageParams } from ".././osBindings"
import { DestroyOverlayedImageParams } from ".././osBindings"
import { BindParams } from ".././osBindings"
import { GetHostInfoParams } from ".././osBindings"
import { ParamsPackageId } from ".././osBindings"
import { ParamsMaybePackageId } from ".././osBindings"
import { SetConfigured } from ".././osBindings"
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
import { RemoveAddressParams } from ".././osBindings"
import { ExportActionParams } from ".././osBindings"
import { RemoveActionParams } from ".././osBindings"
import { MountParams } from ".././osBindings"
function typeEquality<ExpectedType>(_a: ExpectedType) {}

type WithCallback<T> = Omit<T, "callback"> & { callback: () => void }

describe("startosTypeValidation ", () => {
  test(`checking the params match`, () => {
    const testInput: any = {}
    typeEquality<{
      [K in keyof Effects]: Effects[K] extends (args: infer A) => any
        ? A
        : never
    }>({
      executeAction: {} as ExecuteAction,
      createOverlayedImage: {} as CreateOverlayedImageParams,
      destroyOverlayedImage: {} as DestroyOverlayedImageParams,
      clearBindings: undefined,
      bind: {} as BindParams,
      getHostInfo: {} as WithCallback<GetHostInfoParams>,
      exists: {} as ParamsPackageId,
      getConfigured: undefined,
      stopped: {} as ParamsMaybePackageId,
      running: {} as ParamsPackageId,
      restart: undefined,
      shutdown: undefined,
      setConfigured: {} as SetConfigured,
      setHealth: {} as SetHealth,
      exposeForDependents: {} as ExposeForDependentsParams,
      getSslCertificate: {} as GetSslCertificateParams,
      getSslKey: {} as GetSslKeyParams,
      getServiceInterface: {} as WithCallback<GetServiceInterfaceParams>,
      setDependencies: {} as SetDependenciesParams,
      store: {} as never,
      getSystemSmtp: {} as WithCallback<GetSystemSmtpParams>,
      getContainerIp: undefined,
      getServicePortForward: {} as GetServicePortForwardParams,
      clearServiceInterfaces: undefined,
      exportServiceInterface: {} as ExportServiceInterfaceParams,
      getPrimaryUrl: {} as WithCallback<GetPrimaryUrlParams>,
      listServiceInterfaces: {} as WithCallback<ListServiceInterfacesParams>,
      removeAddress: {} as RemoveAddressParams,
      exportAction: {} as ExportActionParams,
      removeAction: {} as RemoveActionParams,
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
