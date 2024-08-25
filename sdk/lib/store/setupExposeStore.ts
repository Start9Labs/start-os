import { Affine, _ } from "../util"
import { PathBuilder, extractJsonPath, pathBuilder } from "./PathBuilder"

export type ExposedStorePaths = string[] & Affine<"ExposedStorePaths">

/**
 * @description Use this function to determine which Store values to expose and make available to other services running on StartOS. Store values not exposed here will be kept private. Use the type safe pathBuilder to traverse your Store's structure.
 * @example
 * In this example, we expose the hypothetical Store values "adminPassword" and "nameLastUpdatedAt".
 *
 * ```
  export const exposedStore = setupExposeStore<Store>((pathBuilder) => [
    pathBuilder.adminPassword
    pathBuilder.nameLastUpdatedAt,
  ])
 * ```
 */
export const setupExposeStore = <Store extends Record<string, any>>(
  fn: (pathBuilder: PathBuilder<Store>) => PathBuilder<Store, any>[],
) => {
  return fn(pathBuilder<Store>()).map(
    (x) => extractJsonPath(x) as string,
  ) as ExposedStorePaths
}
