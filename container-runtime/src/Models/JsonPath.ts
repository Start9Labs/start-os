import { literals, some, string } from "ts-matches"

type NestedPath<A extends string, B extends string> = `/${A}/${string}/${B}`
type NestedPaths = NestedPath<"actions", "run" | "getInput">
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
  if (paths[1] === "actions" && (paths[3] === "run" || paths[3] === "getInput"))
    return true
  return false
}
export const jsonPath = some(
  literals(
    "/init",
    "/uninit",
    "/backup/create",
    "/backup/restore",
    "/actions/metadata",
    "/properties",
  ),
  string.refine(isNestedPath, "isNestedPath"),
)

export type JsonPath = typeof jsonPath._TYPE
