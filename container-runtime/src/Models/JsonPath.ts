import { literals, some, string } from "ts-matches"

type NestedPath<A extends string, B extends string> = `/${A}/${string}/${B}`
type NestedPaths =
  | NestedPath<"actions", "run" | "get">
  | NestedPath<"dependencies", "query" | "update">
// prettier-ignore
type UnNestPaths<A> = 
  A extends `${infer A}/${infer B}` ? [...UnNestPaths<A>, ... UnNestPaths<B>] : 
  [A]

export function unNestPath<A extends string>(a: A): UnNestPaths<A> {
  return a.split("/") as UnNestPaths<A>
}
function isNestedPath(path: string): path is NestedPaths {
  const paths = path.split("/")
  if (paths.length !== 4) return false
  if (paths[1] === "action" && (paths[3] === "run" || paths[3] === "get"))
    return true
  if (
    paths[1] === "dependencyConfig" &&
    (paths[3] === "query" || paths[3] === "update")
  )
    return true
  return false
}
export const jsonPath = some(
  literals(
    "/init",
    "/uninit",
    "/main/start",
    "/main/stop",
    "/config/set",
    "/config/get",
    "/backup/create",
    "/backup/restore",
    "/actions/metadata",
    "/properties",
  ),
  string.refine(isNestedPath, "isNestedPath"),
)

export type JsonPath = typeof jsonPath._TYPE
