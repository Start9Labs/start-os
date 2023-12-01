import { literals, some, string } from "ts-matches"

function isAction(path: string): path is `/action/${string}` {
  const paths = path.split("/")
  return paths.length === 3 && paths[1] === "action"
}

type DependencyPath =
  | `/dependencies/${string}/check`
  | `/dependencies/${string}/autoConfigure`

function isDependencies(path: string): path is DependencyPath {
  const paths = path.split("/")
  return (
    paths.length === 4 &&
    paths[1] === "dependencies" &&
    (paths[3] === "check" || paths[3] === "autoConfigure")
  )
}

export const jsonPath = some(
  literals(
    "/createBackup",
    "/restoreBackup",
    "/getConfig",
    "/setConfig",
    "migration",
    "/properties",
    "/handleSignal",
  ),
  string.refine(isAction, "isAction"),
  string.refine(isDependencies, "isDependencies"),
)

export type JsonPath = typeof jsonPath._TYPE
