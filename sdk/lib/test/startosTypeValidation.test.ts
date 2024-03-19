import { Effects } from "../types"
import { ExecuteAction } from "../../../core/startos/bindings/ExecuteAction"
import { CreateOverlayedImageParams } from "../../../core/startos/bindings/CreateOverlayedImageParams"
import { DestroyOverlayedImageParams } from "../../../core/startos/bindings/DestroyOverlayedImageParams"
import { BindParams } from "../../../core/startos/bindings/BindParams"
import { GetHostInfoParams } from "../../../core/startos/bindings/GetHostInfoParams"
import { ParamsPackageId } from "../../../core/startos/bindings/ParamsPackageId"
import { ParamsMaybePackageId } from "../../../core/startos/bindings/ParamsMaybePackageId"
import { SetConfigured } from "../../../core/startos/bindings/SetConfigured"
import { SetHealth } from "../../../core/startos/bindings/SetHealth"
import { ExposeForDependentsParams } from "../../../core/startos/bindings/ExposeForDependentsParams"
import { ExposeUiParams } from "../../../core/startos/bindings/ExposeUiParams"
import { GetSslCertificateParams } from "../../../core/startos/bindings/GetSslCertificateParams"
import { GetSslKeyParams } from "../../../core/startos/bindings/GetSslKeyParams"
import { GetServiceInterfaceParams } from "../../../core/startos/bindings/GetServiceInterfaceParams"
import { SetDependenciesParams } from "../../../core/startos/bindings/SetDependenciesParams"
import { GetSystemSmtpParams } from "../../../core/startos/bindings/GetSystemSmtpParams"
import { GetServicePortForwardParams } from "../../../core/startos/bindings/GetServicePortForwardParams"
import { ExportServiceInterfaceParams } from "../../../core/startos/bindings/ExportServiceInterfaceParams"
import { GetPrimaryUrlParams } from "../../../core/startos/bindings/GetPrimaryUrlParams"
import { ListServiceInterfacesParams } from "../../../core/startos/bindings/ListServiceInterfacesParams"
import { RemoveAddressParams } from "../../../core/startos/bindings/RemoveAddressParams"
import { ExportActionParams } from "../../../core/startos/bindings/ExportActionParams"
import { RemoveActionParams } from "../../../core/startos/bindings/RemoveActionParams"
import { ReverseProxyParams } from "../../../core/startos/bindings/ReverseProxyParams"
import { MountParams } from "../../../core/startos/bindings/MountParams"
import { ExposedUI } from "../../../core/startos/bindings/ExposedUI"
function typeEquality<ExpectedType>(_a: ExpectedType) {}
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
      getHostInfo: {} as GetHostInfoParams,
      exists: {} as ParamsPackageId,
      getConfigured: undefined,
      stopped: {} as ParamsMaybePackageId,
      running: {} as ParamsPackageId,
      restart: undefined,
      shutdown: undefined,
      setConfigured: {} as SetConfigured,
      setHealth: {} as SetHealth,
      exposeForDependents: {} as ExposeForDependentsParams,
      exposeUi: {} as { [key: string]: ExposedUI },
      getSslCertificate: {} as GetSslCertificateParams,
      getSslKey: {} as GetSslKeyParams,
      getServiceInterface: {} as GetServiceInterfaceParams,
      setDependencies: {} as SetDependenciesParams,
      store: {} as never,
      getSystemSmtp: {} as GetSystemSmtpParams,
      getContainerIp: undefined,
      getServicePortForward: {} as GetServicePortForwardParams,
      clearServiceInterfaces: undefined,
      exportServiceInterface: {} as ExportServiceInterfaceParams,
      getPrimaryUrl: {} as GetPrimaryUrlParams,
      listServiceInterfaces: {} as ListServiceInterfacesParams,
      removeAddress: {} as RemoveAddressParams,
      exportAction: {} as ExportActionParams,
      removeAction: {} as RemoveActionParams,
      reverseProxy: {} as ReverseProxyParams,
      mount: {} as MountParams,
    })
    typeEquality<Parameters<Effects["executeAction"]>[0]>(
      testInput as ExecuteAction,
    )
  })
})
