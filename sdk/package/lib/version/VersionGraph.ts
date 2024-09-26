import { ExtendedVersion, VersionRange } from "../../../base/lib/exver"
import * as T from "../../../base/lib/types"
import { Graph, Vertex, once } from "../util"
import { IMPOSSIBLE, VersionInfo } from "./VersionInfo"

export class VersionGraph<CurrentVersion extends string> {
  private readonly graph: () => Graph<
    ExtendedVersion | VersionRange,
    ((opts: { effects: T.Effects }) => Promise<null>) | undefined
  >
  private constructor(
    readonly current: VersionInfo<CurrentVersion>,
    versions: Array<VersionInfo<any>>,
  ) {
    this.graph = once(() => {
      const graph = new Graph<
        ExtendedVersion | VersionRange,
        ((opts: { effects: T.Effects }) => Promise<null>) | undefined
      >()
      const flavorMap: Record<
        string,
        [
          ExtendedVersion,
          VersionInfo<any>,
          Vertex<
            ExtendedVersion | VersionRange,
            ((opts: { effects: T.Effects }) => Promise<null>) | undefined
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
                (opts: { effects: T.Effects }) => Promise<null>
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
                (v) =>
                  v.metadata instanceof ExtendedVersion &&
                  v.metadata.satisfies(range),
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
  >(
    currentVersion: VersionInfo<CurrentVersion>,
    ...other: EnsureUniqueId<OtherVersions, OtherVersions, CurrentVersion>
  ) {
    return new VersionGraph(currentVersion, other as Array<VersionInfo<any>>)
  }
  async migrate({
    effects,
    from,
    to,
  }: {
    effects: T.Effects
    from: ExtendedVersion
    to: ExtendedVersion
  }) {
    const graph = this.graph()
    if (from && to) {
      const path = graph.shortestPath(
        (v) =>
          (v.metadata instanceof VersionRange &&
            v.metadata.satisfiedBy(from)) ||
          (v.metadata instanceof ExtendedVersion && v.metadata.equals(from)),
        (v) =>
          (v.metadata instanceof VersionRange && v.metadata.satisfiedBy(to)) ||
          (v.metadata instanceof ExtendedVersion && v.metadata.equals(to)),
      )
      if (path) {
        for (let edge of path) {
          if (edge.metadata) {
            await edge.metadata({ effects })
          }
          await effects.setDataVersion({ version: edge.to.metadata.toString() })
        }
        return
      }
    }
    throw new Error()
  }
  canMigrateFrom = once(() =>
    Array.from(
      this.graph().reverseBreadthFirstSearch(
        (v) =>
          (v.metadata instanceof VersionRange &&
            v.metadata.satisfiedBy(this.currentVersion())) ||
          (v.metadata instanceof ExtendedVersion &&
            v.metadata.equals(this.currentVersion())),
      ),
    ).reduce(
      (acc, x) =>
        acc.or(
          x.metadata instanceof VersionRange
            ? x.metadata
            : VersionRange.anchor("=", x.metadata),
        ),
      VersionRange.none(),
    ),
  )
  canMigrateTo = once(() =>
    Array.from(
      this.graph().breadthFirstSearch(
        (v) =>
          (v.metadata instanceof VersionRange &&
            v.metadata.satisfiedBy(this.currentVersion())) ||
          (v.metadata instanceof ExtendedVersion &&
            v.metadata.equals(this.currentVersion())),
      ),
    ).reduce(
      (acc, x) =>
        acc.or(
          x.metadata instanceof VersionRange
            ? x.metadata
            : VersionRange.anchor("=", x.metadata),
        ),
      VersionRange.none(),
    ),
  )
}

// prettier-ignore
export type EnsureUniqueId<A, B = A, OtherVersions = never> =
  B extends [] ? A : 
  B extends [VersionInfo<infer Version>, ...infer Rest] ? (
    Version extends OtherVersions ?  "One or more versions are not unique"[] :
    EnsureUniqueId<A, Rest, Version | OtherVersions>
  ) : "There exists a migration that is not a Migration"[]
