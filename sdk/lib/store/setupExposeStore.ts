import { Affine, _ } from "../util"
import { PathBuilder, extractJsonPath, pathBuilder } from "./PathBuilder"

export type ExposedStorePaths = string[] & Affine<"ExposedStorePaths">

export const setupExposeStore = <Store extends Record<string, any>>(
  fn: (pathBuilder: PathBuilder<Store>) => PathBuilder<Store, any>[],
) => {
  return fn(pathBuilder<Store>()).map(
    (x) => extractJsonPath(x) as string,
  ) as ExposedStorePaths
}
