import { inject, Injectable } from '@angular/core'
import { Marketplace, MarketplacePkg } from '@start9labs/marketplace'
import { Exver } from '@start9labs/shared'
import {
  catchError,
  combineLatest,
  distinctUntilChanged,
  map,
  merge,
  Observable,
  of,
  scan,
  shareReplay,
  startWith,
  switchMap,
} from 'rxjs'
import { LocalPackagesService } from 'src/app/services/local-packages.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  InstalledState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'

export type RefinedUpdates = {
  marketplace: Marketplace
  // Keys currently being re-fetched. Empty set once all refinements resolve.
  pending: Set<string>
}

type Unreachable = {
  url: string
  id: string
  flavor: string | null
  localVer: string
  key: string
}

type RefineResult = { key: string; pkg: MarketplacePkg | null }

// Packages can appear under the same id with different flavors (e.g. bitcoind
// vanilla vs. #knots). The refinement key must distinguish them so a replace
// only hits the matching flavor row.
export function refinementKey(
  url: string,
  id: string,
  flavor: string | null,
): string {
  return `${url}|${id}|${flavor ?? ''}`
}

// Singleton owner of the "reachable best" refinement pipeline. The registry
// may report a "best" version that the locally installed version can't
// migrate into directly (e.g. NextCloud requires sequential majors). For each
// such pkg we re-fetch with sourceVersion=local so the registry returns a
// reachable stepping-stone instead.
//
// This MUST live in a singleton, not on UpdatesComponent — otherwise each
// navigation to the Updates tab rebuilds the pipeline and replays the
// "Finding suitable version..." pending state. The navbar badge keeps a
// continuous subscription via BadgeService, so refinement runs in the
// background from app startup and the Updates tab replays the already-
// resolved state on mount.
@Injectable({
  providedIn: 'root',
})
export class UpdatesRefinementService {
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly localPkgs$ = inject(LocalPackagesService)
  private readonly exver = inject(Exver)

  // Refinement depends only on the marketplace catalog and the *set of
  // installed versions*. distinct'ing localPkgs on that set prevents
  // install-progress ticks (same versions, new progress numbers) from
  // restarting refinement and re-flashing pending state.
  readonly refined$: Observable<RefinedUpdates> = combineLatest([
    this.marketplaceService.marketplace$,
    this.localPkgs$.pipe(distinctUntilChanged(sameInstalledVersions)),
  ]).pipe(
    switchMap(([marketplace, localPkgs]) =>
      this.refine(marketplace, localPkgs),
    ),
    // No refCount: keep the pipeline warm across navigation so the Updates
    // tab replays the final resolved state instead of re-running refinement.
    shareReplay(1),
  )

  private refine(
    marketplace: Marketplace,
    localPkgs: Record<string, PackageDataEntry<InstalledState | UpdatingState>>,
  ): Observable<RefinedUpdates> {
    const unreachable = this.findUnreachable(marketplace, localPkgs)
    const pending = new Set(unreachable.map(u => u.key))
    const initial: RefinedUpdates = { marketplace, pending }

    if (unreachable.length === 0) return of(initial)

    // Re-fetch each unreachable pkg with sourceVersion=local so the registry
    // returns a "best" that the user can actually migrate to. A null result
    // (fetch failed, or the returned pkg is still unreachable/not newer)
    // silently drops the row — there's nothing the user could do with it.
    const refetches = unreachable.map(u =>
      this.marketplaceService
        .fetchPackage$(u.url, u.id, null, u.flavor, u.localVer)
        .pipe(
          map<MarketplacePkg, RefineResult>(pkg => ({
            key: u.key,
            pkg: this.isReachableAndNewer(pkg, u.localVer) ? pkg : null,
          })),
          catchError(() => of<RefineResult>({ key: u.key, pkg: null })),
        ),
    )

    // Emit the initial state immediately so pending rows render as "Finding
    // suitable version...", then fold each refetch result into the state as
    // it arrives. Rows update in-place; silent drops shrink the list.
    return merge(...refetches).pipe(
      scan<RefineResult, RefinedUpdates>((state, result) => {
        const nextPending = new Set(state.pending)
        nextPending.delete(result.key)
        return this.applyReplacement(state, result.key, result.pkg, nextPending)
      }, initial),
      startWith(initial),
    )
  }

  // Mirrors the filter-updates pipe's candidate rules (installed, newer,
  // flavor-matched) and then keeps only the pkgs where the registry's best
  // version declares a sourceVersion range that the local version does NOT
  // satisfy — i.e. the ones the registry says we can't migrate into.
  //
  // `sourceVersion` on the registry pkg is the same range the manifest calls
  // `canMigrateFrom`: "which installed versions are allowed to upgrade to me".
  // A null value means "anything can migrate here" (treated as '*').
  private findUnreachable(
    marketplace: Marketplace,
    localPkgs: Record<string, PackageDataEntry<InstalledState | UpdatingState>>,
  ): Unreachable[] {
    const out: Unreachable[] = []
    for (const [url, store] of Object.entries(marketplace)) {
      if (!store) continue
      for (const pkg of store.packages) {
        const local = localPkgs[pkg.id]
        if (!local) continue
        const state = local.stateInfo.state
        if (state !== 'installed' && state !== 'updating') continue
        const localVer = local.stateInfo.manifest.version
        if (this.exver.compareExver(pkg.version, localVer) !== 1) continue
        if (this.exver.getFlavor(localVer) !== pkg.flavor) continue
        if (this.exver.satisfies(localVer, pkg.sourceVersion || '*')) continue
        out.push({
          url,
          id: pkg.id,
          flavor: pkg.flavor,
          localVer,
          key: refinementKey(url, pkg.id, pkg.flavor),
        })
      }
    }
    return out
  }

  // Defense-in-depth: the registry *should* only return reachable bests when
  // sourceVersion is passed, but we re-verify here so a buggy or permissive
  // registry can't slip an unreachable row back onto the page.
  private isReachableAndNewer(
    pkg: MarketplacePkg | null,
    localVer: string,
  ): boolean {
    if (!pkg || !pkg.version) return false
    if (this.exver.compareExver(pkg.version, localVer) !== 1) return false
    return this.exver.satisfies(localVer, pkg.sourceVersion || '*')
  }

  // Called once per refetch result as they stream in via scan(). A null
  // replacement silently drops the pkg from the updates list; the user never
  // sees a row they can't act on.
  private applyReplacement(
    state: RefinedUpdates,
    key: string,
    replacement: MarketplacePkg | null,
    nextPending: Set<string>,
  ): RefinedUpdates {
    const marketplace: Marketplace = {}
    for (const [url, store] of Object.entries(state.marketplace)) {
      if (!store) {
        marketplace[url] = store
        continue
      }
      const packages: MarketplacePkg[] = []
      for (const pkg of store.packages) {
        if (refinementKey(url, pkg.id, pkg.flavor) !== key) {
          packages.push(pkg)
          continue
        }
        if (replacement) packages.push(replacement)
        // else: silent drop
      }
      marketplace[url] = { ...store, packages }
    }
    return { marketplace, pending: nextPending }
  }
}

// distinctUntilChanged comparator: only treat localPkgs as "changed" when the
// set of installed (id, version) pairs actually differs. Status/progress ticks
// re-emit the full record but don't shift versions, so this prevents spurious
// refinement restarts.
function sameInstalledVersions(
  a: Record<string, PackageDataEntry<InstalledState | UpdatingState>>,
  b: Record<string, PackageDataEntry<InstalledState | UpdatingState>>,
): boolean {
  const aKeys = Object.keys(a)
  const bKeys = Object.keys(b)
  if (aKeys.length !== bKeys.length) return false
  for (const id of aKeys) {
    if (
      a[id]?.stateInfo.manifest.version !== b[id]?.stateInfo.manifest.version
    ) {
      return false
    }
  }
  return true
}
