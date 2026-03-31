import { Effects } from '../Effects'
import { PackageId } from '../osBindings'
import { deepEqual } from './deepEqual'
import { ServiceInterfaceFilled, filledAddress } from './getServiceInterface'
import { Watchable } from './Watchable'

const makeManyInterfaceFilled = async ({
  effects,
  packageId,
  callback,
}: {
  effects: Effects
  packageId?: string
  callback?: () => void
}) => {
  const serviceInterfaceValues = await effects.listServiceInterfaces({
    packageId,
    callback,
  })

  const serviceInterfacesFilled: ServiceInterfaceFilled[] = await Promise.all(
    Object.values(serviceInterfaceValues).map(async (serviceInterfaceValue) => {
      const hostId = serviceInterfaceValue.addressInfo.hostId
      const host = await effects.getHostInfo({
        packageId,
        hostId,
        callback,
      })
      if (!host) {
        throw new Error(`host ${hostId} not found!`)
      }
      return {
        ...serviceInterfaceValue,
        host,
        addressInfo: filledAddress(host, serviceInterfaceValue.addressInfo),
      }
    }),
  )
  return serviceInterfacesFilled
}

export class GetServiceInterfaces<
  Mapped = ServiceInterfaceFilled[],
> extends Watchable<ServiceInterfaceFilled[], Mapped> {
  protected readonly label = 'GetServiceInterfaces'

  constructor(
    effects: Effects,
    readonly opts: { packageId?: string },
    options?: {
      map?: (value: ServiceInterfaceFilled[]) => Mapped
      eq?: (a: Mapped, b: Mapped) => boolean
    },
  ) {
    super(effects, options)
  }

  protected fetch(callback?: () => void) {
    return makeManyInterfaceFilled({
      effects: this.effects,
      packageId: this.opts.packageId,
      callback,
    })
  }
}

export function getOwnServiceInterfaces(effects: Effects): GetServiceInterfaces
export function getOwnServiceInterfaces<Mapped>(
  effects: Effects,
  map: (interfaces: ServiceInterfaceFilled[]) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterfaces<Mapped>
export function getOwnServiceInterfaces<Mapped>(
  effects: Effects,
  map?: (interfaces: ServiceInterfaceFilled[]) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterfaces<Mapped> {
  return new GetServiceInterfaces<Mapped>(
    effects,
    {},
    {
      map: map ?? ((a) => a as Mapped),
      eq: eq ?? ((a, b) => deepEqual(a, b)),
    },
  )
}

export function getServiceInterfaces(
  effects: Effects,
  opts: { packageId: PackageId },
): GetServiceInterfaces
export function getServiceInterfaces<Mapped>(
  effects: Effects,
  opts: { packageId: PackageId },
  map: (interfaces: ServiceInterfaceFilled[]) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterfaces<Mapped>
export function getServiceInterfaces<Mapped>(
  effects: Effects,
  opts: { packageId: PackageId },
  map?: (interfaces: ServiceInterfaceFilled[]) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterfaces<Mapped> {
  return new GetServiceInterfaces<Mapped>(effects, opts, {
    map: map ?? ((a) => a as Mapped),
    eq: eq ?? ((a, b) => deepEqual(a, b)),
  })
}
