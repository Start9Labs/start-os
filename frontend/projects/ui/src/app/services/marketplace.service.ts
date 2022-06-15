import { Injectable } from '@angular/core'
import { LoadingController } from '@ionic/angular'
import { Emver, ErrorToastService } from '@start9labs/shared'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
  Marketplace,
} from '@start9labs/marketplace'
import { defer, from, Observable, of } from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import {
  ServerInfo,
  UIMarketplaceData,
} from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import {
  catchError,
  filter,
  map,
  shareReplay,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs/operators'

@Injectable()
export class MarketplaceService extends AbstractMarketplaceService {
  private readonly notes = new Map<string, Record<string, string>>()

  private readonly altMarketplaceData$: Observable<
    UIMarketplaceData | undefined
  > = this.patch.watch$('ui', 'marketplace').pipe(shareReplay())

  private readonly marketplace$ = this.altMarketplaceData$.pipe(
    map(data => this.toMarketplace(data)),
  )

  private readonly serverInfo$: Observable<ServerInfo> = this.patch
    .watch$('server-info')
    .pipe(take(1), shareReplay())

  private readonly categories$: Observable<string[]> = this.marketplace$.pipe(
    switchMap(({ url }) =>
      this.serverInfo$.pipe(
        switchMap(({ id }) =>
          from(this.getMarketplaceData({ 'server-id': id }, url)),
        ),
      ),
    ),
    map(({ categories }) => categories),
    shareReplay(),
  )

  private readonly pkg$: Observable<MarketplacePkg[]> =
    this.altMarketplaceData$.pipe(
      switchMap(data =>
        this.serverInfo$.pipe(
          switchMap(info =>
            from(
              this.getMarketplacePkgs(
                { page: 1, 'per-page': 100 },
                this.toMarketplace(data).url,
                info['eos-version-compat'],
              ),
            ).pipe(tap(() => this.onPackages(data))),
          ),
        ),
      ),
      catchError(e => {
        this.errToast.present(e)

        return of([])
      }),
      shareReplay(),
    )

  constructor(
    private readonly api: ApiService,
    private readonly patch: PatchDbService,
    private readonly config: ConfigService,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly emver: Emver,
  ) {
    super()
  }

  getMarketplace(): Observable<Marketplace> {
    return this.marketplace$
  }

  getCategories(): Observable<string[]> {
    return this.categories$
  }

  getPackages(): Observable<MarketplacePkg[]> {
    return this.pkg$
  }

  getPackage(id: string, version: string): Observable<MarketplacePkg | null> {
    const params = { ids: [{ id, version }] }
    const fallback$ = this.marketplace$.pipe(
      switchMap(({ url }) =>
        this.serverInfo$.pipe(
          switchMap(info =>
            from(
              this.getMarketplacePkgs(params, url, info['eos-version-compat']),
            ),
          ),
        ),
      ),
      map(pkgs => this.findPackage(pkgs, id, version)),
      startWith(null),
    )

    return this.getPackages().pipe(
      map(pkgs => this.findPackage(pkgs, id, version)),
      switchMap(pkg => (pkg ? of(pkg) : fallback$)),
      filter((pkg): pkg is MarketplacePkg | null => {
        if (pkg === undefined) {
          throw new Error(`No results for ${id}${version ? ' ' + version : ''}`)
        }

        return true
      }),
    )
  }

  getReleaseNotes(id: string): Observable<Record<string, string>> {
    if (this.notes.has(id)) {
      return of(this.notes.get(id) || {})
    }

    return this.marketplace$.pipe(
      switchMap(({ url }) => this.loadReleaseNotes(id, url)),
      tap(response => this.notes.set(id, response)),
      catchError(e => {
        this.errToast.present(e)

        return of({})
      }),
    )
  }

  install(id: string, version?: string): Observable<unknown> {
    return defer(() =>
      from(
        this.loadingCtrl.create({
          spinner: 'lines',
          message: 'Beginning Installation',
        }),
      ),
    ).pipe(
      tap(loader => loader.present()),
      switchMap(loader =>
        this.installPackage({
          id,
          'version-spec': version ? `=${version}` : undefined,
        }).pipe(
          catchError(e => from(this.errToast.present(e))),
          tap(() => loader.dismiss()),
        ),
      ),
    )
  }

  installPackage(
    req: Omit<RR.InstallPackageReq, 'marketplace-url'>,
  ): Observable<unknown> {
    return this.getMarketplace().pipe(
      take(1),
      switchMap(({ url }) =>
        from(
          this.api.installPackage({
            ...req,
            'marketplace-url': url,
          }),
        ),
      ),
    )
  }

  getPackageMarkdown(type: string, pkgId: string): Observable<string> {
    return this.getMarketplace().pipe(
      switchMap(({ url }) =>
        from(
          this.api.marketplaceProxy<string>(
            `/package/v0/${type}/${pkgId}`,
            {},
            url,
          ),
        ),
      ),
    )
  }

  async getMarketplaceData(
    params: RR.GetMarketplaceDataReq,
    url: string,
  ): Promise<RR.GetMarketplaceDataRes> {
    return this.api.marketplaceProxy('/package/v0/info', params, url)
  }

  async getMarketplacePkgs(
    params: Omit<RR.GetMarketplacePackagesReq, 'eos-version-compat'>,
    url: string,
    eosVersionCompat: string,
  ): Promise<RR.GetMarketplacePackagesRes> {
    if (params.query) delete params.category
    if (params.ids) params.ids = JSON.stringify(params.ids)

    const qp: RR.GetMarketplacePackagesReq = {
      ...params,
      'eos-version-compat': eosVersionCompat,
    }

    return this.api.marketplaceProxy('/package/v0/index', qp, url)
  }

  private loadReleaseNotes(
    id: string,
    url: string,
  ): Observable<Record<string, string>> {
    return from(
      this.api.marketplaceProxy<Record<string, string>>(
        `/package/v0/release-notes/${id}`,
        {},
        url,
      ),
    )
  }

  private onPackages(data?: UIMarketplaceData) {
    const { name } = this.toMarketplace(data)

    if (!data?.['selected-id']) {
      return
    }

    const selectedId = data['selected-id']
    const knownHosts = data['known-hosts']

    if (knownHosts[selectedId].name !== name) {
      this.api.setDbValue({
        pointer: `/marketplace/known-hosts/${selectedId}/name`,
        value: name,
      })
    }
  }

  private toMarketplace(marketplace?: UIMarketplaceData): Marketplace {
    return marketplace?.['selected-id']
      ? marketplace['known-hosts'][marketplace['selected-id']]
      : this.config.marketplace
  }

  private findPackage(
    pkgs: readonly MarketplacePkg[],
    id: string,
    version: string,
  ): MarketplacePkg | undefined {
    return pkgs.find(pkg => {
      const versionIsSame =
        version === '*' ||
        this.emver.compare(pkg.manifest.version, version) === 0

      return pkg.manifest.id === id && versionIsSame
    })
  }
}
