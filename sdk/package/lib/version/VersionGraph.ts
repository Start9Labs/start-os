import { ExtendedVersion, VersionRange } from "../../../base/lib/exver"
import * as T from "../../../base/lib/types"
import {
  InitFn,
  InitKind,
  InitScript,
  InitScriptOrFn,
  UninitFn,
  UninitScript,
  UninitScriptOrFn,
} from "../../../base/lib/inits"
import { Graph, Vertex, once } from "../util"
import { IMPOSSIBLE, VersionInfo } from "./VersionInfo"

export async function getDataVersion(effects: T.Effects) {
  const versionStr = await effects.getDataVersion()
  if (!versionStr) return null
  try {
    return ExtendedVersion.parse(versionStr)
  } catch (_) {
    return VersionRange.parse(versionStr)
  }
}

export async function setDataVersion(
  effects: T.Effects,
  version: ExtendedVersion | VersionRange | null,
) {
  return effects.setDataVersion({ version: version?.toString() || null })
}

function isExver(v: ExtendedVersion | VersionRange): v is ExtendedVersion {
  return "satisfies" in v
}

function isRange(v: ExtendedVersion | VersionRange): v is VersionRange {
  return "satisfiedBy" in v
}

export function overlaps(
  a: ExtendedVersion | VersionRange,
  b: ExtendedVersion | VersionRange,
) {
  return (
    (isRange(a) && isRange(b) && a.intersects(b)) ||
    (isRange(a) && isExver(b) && a.satisfiedBy(b)) ||
    (isExver(a) && isRange(b) && a.satisfies(b)) ||
    (isExver(a) && isExver(b) && a.equals(b))
  )
}

export class VersionGraph<CurrentVersion extends string>
  implements InitScript, UninitScript
{
  protected initFn = this.init.bind(this)
  protected uninitFn = this.uninit.bind(this)
  private readonly graph: () => Graph<
    ExtendedVersion | VersionRange,
    ((opts: { effects: T.Effects }) => Promise<void>) | undefined
  >
  private constructor(
    readonly current: VersionInfo<CurrentVersion>,
    versions: Array<VersionInfo<any>>,
    private readonly preInstall?: InitScriptOrFn<"install">,
    private readonly uninstall?: UninitScript | UninitFn,
  ) {
    this.graph = once(() => {
      const graph = new Graph<
        ExtendedVersion | VersionRange,
        ((opts: { effects: T.Effects }) => Promise<void>) | undefined
      >()
      const flavorMap: Record<
        string,
        [
          ExtendedVersion,
          VersionInfo<any>,
          Vertex<
            ExtendedVersion | VersionRange,
            ((opts: { effects: T.Effects }) => Promise<void>) | undefined
          >,
        ][]
      > = {}
      for (let version of [current, ...versions]) {
        const v = ExtendedVersion.parse(version.options.version)
        const vertex = graph.addVertex(v, [], [])
        const flavor = v.flavor || ""
        if (!flavorMap[flavor]) {
          flavorMap[flavor] = []
        }
        flavorMap[flavor].push([v, version, vertex])
      }
      for (let flavor in flavorMap) {
        flavorMap[flavor].sort((a, b) => a[0].compareForSort(b[0]))
        let prev:
          | [
              ExtendedVersion,
              VersionInfo<any>,
              Vertex<
                ExtendedVersion | VersionRange,
                (opts: { effects: T.Effects }) => Promise<void>
              >,
            ]
          | undefined = undefined
        for (let [v, version, vertex] of flavorMap[flavor]) {
          if (version.options.migrations.up !== IMPOSSIBLE) {
            let range
            if (prev) {
              graph.addEdge(version.options.migrations.up, prev[2], vertex)
              range = VersionRange.anchor(">=", prev[0]).and(
                VersionRange.anchor("<", v),
              )
            } else {
              range = VersionRange.anchor("<", v)
            }
            const vRange = graph.addVertex(range, [], [])
            graph.addEdge(version.options.migrations.up, vRange, vertex)
          }

          if (version.options.migrations.down !== IMPOSSIBLE) {
            let range
            if (prev) {
              graph.addEdge(version.options.migrations.down, vertex, prev[2])
              range = VersionRange.anchor(">=", prev[0]).and(
                VersionRange.anchor("<", v),
              )
            } else {
              range = VersionRange.anchor("<", v)
            }
            const vRange = graph.addVertex(range, [], [])
            graph.addEdge(version.options.migrations.down, vertex, vRange)
          }

          if (version.options.migrations.other) {
            for (let rangeStr in version.options.migrations.other) {
              const range = VersionRange.parse(rangeStr)
              const vRange = graph.addVertex(range, [], [])
              graph.addEdge(
                version.options.migrations.other[rangeStr],
                vRange,
                vertex,
              )
              for (let matching of graph.findVertex(
                (v) => isExver(v.metadata) && v.metadata.satisfies(range),
              )) {
                graph.addEdge(
                  version.options.migrations.other[rangeStr],
                  matching,
                  vertex,
                )
              }
            }
          }
        }
      }
      return graph
    })
  }
  currentVersion = once(() =>
    ExtendedVersion.parse(this.current.options.version),
  )
  /**
   * Each exported `VersionInfo.of()` should be imported and provided as an argument to this function.
   *
   * ** The current version must be the FIRST argument. **
   */
  static of<
    CurrentVersion extends string,
    OtherVersions extends Array<VersionInfo<any>>,
  >(options: {
    current: VersionInfo<CurrentVersion>
    other: OtherVersions
    /**
     * A script to run only on fresh install
     */
    preInstall?: InitScriptOrFn<"install">
    /**
     * A script to run only on uninstall
     */
    uninstall?: UninitScriptOrFn
  }) {
    return new VersionGraph(
      options.current,
      options.other,
      options.preInstall,
      options.uninstall,
    )
  }
  async migrate({
    effects,
    from,
    to,
  }: {
    effects: T.Effects
    from: ExtendedVersion | VersionRange
    to: ExtendedVersion | VersionRange
  }): Promise<ExtendedVersion | VersionRange> {
    if (overlaps(from, to)) return from
    const graph = this.graph()
    if (from && to) {
      const path = graph.shortestPath(
        (v) => overlaps(v.metadata, from),
        (v) => overlaps(v.metadata, to),
      )
      if (path) {
        let dataVersion = from
        for (let edge of path) {
          if (edge.metadata) {
            await edge.metadata({ effects })
          }
          dataVersion = edge.to.metadata
          await setDataVersion(effects, edge.to.metadata)
        }
        return dataVersion
      }
    }
    throw new Error(
      `cannot migrate from ${from.toString()} to ${to.toString()}`,
    )
  }
  canMigrateFrom = once(() =>
    Array.from(
      this.graph().reverseBreadthFirstSearch((v) =>
        overlaps(v.metadata, this.currentVersion()),
      ),
    )
      .reduce(
        (acc, x) =>
          acc.or(
            isRange(x.metadata)
              ? x.metadata
              : VersionRange.anchor("=", x.metadata),
          ),
        VersionRange.none(),
      )
      .normalize(),
  )
  canMigrateTo = once(() =>
    Array.from(
      this.graph().breadthFirstSearch((v) =>
        overlaps(v.metadata, this.currentVersion()),
      ),
    )
      .reduce(
        (acc, x) =>
          acc.or(
            isRange(x.metadata)
              ? x.metadata
              : VersionRange.anchor("=", x.metadata),
          ),
        VersionRange.none(),
      )
      .normalize(),
  )

  async init(effects: T.Effects, kind: InitKind): Promise<void> {
    const from = await getDataVersion(effects)
    if (from) {
      await this.migrate({
        effects,
        from,
        to: this.currentVersion(),
      })
    } else {
      kind = "install" // implied by !dataVersion
      if (this.preInstall)
        if ("init" in this.preInstall) await this.preInstall.init(effects, kind)
        else await this.preInstall(effects, kind)
      await effects.setDataVersion({ version: this.current.options.version })
    }
  }

  async uninit(
    effects: T.Effects,
    target: VersionRange | ExtendedVersion | null,
  ): Promise<void> {
    if (target) {
      const from = await getDataVersion(effects)
      if (from) {
        target = await this.migrate({
          effects,
          from,
          to: target,
        })
      }
    } else {
      if (this.uninstall)
        if ("uninit" in this.uninstall)
          await this.uninstall.uninit(effects, target)
        else await this.uninstall(effects, target)
    }
    await setDataVersion(effects, target)
  }
}

// prettier-ignore
export type EnsureUniqueId<A, B = A, OtherVersions = never> =
  B extends [] ? A : 
  B extends [VersionInfo<infer Version>, ...infer Rest] ? (
    Version extends OtherVersions ?  "One or more versions are not unique"[] :
    EnsureUniqueId<A, Rest, Version | OtherVersions>
  ) : "There exists a migration that is not a Migration"[]
