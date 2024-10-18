import { Affine } from "../util"

const pathValue = Symbol("pathValue")
export type PathValue = typeof pathValue

export type PathBuilderStored<AllStore, Store> = {
  [K in PathValue]: [AllStore, Store]
}

export type PathBuilder<AllStore, Store = AllStore> = (Store extends Record<
  string,
  unknown
>
  ? {
      [K in keyof Store]: PathBuilder<AllStore, Store[K]>
    }
  : {}) &
  PathBuilderStored<AllStore, Store>

export type StorePath = string & Affine<"StorePath">
const privateSymbol = Symbol("jsonPath")
export const extractJsonPath = (builder: PathBuilder<unknown>) => {
  return (builder as any)[privateSymbol] as StorePath
}

export const pathBuilder = <Store, StorePath = Store>(
  paths: string[] = [],
): PathBuilder<Store, StorePath> => {
  return new Proxy({} as PathBuilder<Store, StorePath>, {
    get(target, prop) {
      if (prop === privateSymbol) {
        if (paths.length === 0) return ""
        return `/${paths.join("/")}`
      }
      return pathBuilder<any>([...paths, prop as string])
    },
  }) as PathBuilder<Store, StorePath>
}
