import { Injectable } from '@angular/core'
import { LoadingController } from '@ionic/angular'
import { Emver, ErrorToastService } from '@start9labs/shared'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
  Marketplace,
  MarketplaceData,
} from '@start9labs/marketplace'
import { defer, from, Observable, of } from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { ServerInfo } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import {
  catchError,
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

  private readonly init$: Observable<Marketplace> = this.patch
    .watch$('ui', 'marketplace')
    .pipe(
      map(marketplace =>
        marketplace?.['selected-id']
          ? marketplace['known-hosts'][marketplace['selected-id']]
          : this.config.marketplace,
      ),
      shareReplay(),
    )

  private readonly data$: Observable<MarketplaceData> = this.init$.pipe(
    switchMap(({ url }) =>
      from(this.getMarketplaceData({ 'server-id': this.serverInfo.id }, url)),
    ),
    shareReplay(),
  )

  private readonly pkg$: Observable<MarketplacePkg[]> = this.init$.pipe(
    take(1),
    switchMap(({ url, name }) =>
      from(this.getMarketplacePkgs({ page: 1, 'per-page': 100 }, url)).pipe(
        tap(() => this.onPackages(name)),
      ),
    ),
    shareReplay(),
    catchError(e => this.errToast.present(e) && of([])),
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
    return this.init$
  }

  getCategories(): Observable<string[]> {
    return this.data$.pipe(map(({ categories }) => categories))
  }

  getPackages(): Observable<MarketplacePkg[]> {
    return this.pkg$
  }

  getPackage(id: string, version: string): Observable<MarketplacePkg> {
    const params = { ids: [{ id, version }] }
    const fallback$ = this.init$.pipe(
      take(1),
      switchMap(({ url }) => from(this.getMarketplacePkgs(params, url))),
      map(pkgs => this.findPackage(pkgs, id, version)),
      startWith(null),
    )

    return this.getPackages().pipe(
      map(pkgs => this.findPackage(pkgs, id, version)),
      switchMap(pkg => (pkg ? of(pkg) : fallback$)),
      tap(pkg => {
        if (pkg === undefined) {
          throw new Error(`No results for ${id}${version ? ' ' + version : ''}`)
        }
      }),
    )
  }

  getReleaseNotes(id: string): Observable<Record<string, string>> {
    if (this.notes.has(id)) {
      return of(this.notes.get(id))
    }

    return this.init$.pipe(
      switchMap(({ url }) => this.loadReleaseNotes(id, url)),
      tap(response => this.notes.set(id, response)),
      catchError(e => this.errToast.present(e) && of({})),
    )
  }

  install(id: string, version?: string): Observable<unknown> {
    return defer(() =>
      from(
        this.loadingCtrl.create({
          spinner: 'lines',
          message: 'Beginning Installation',
          cssClass: 'loader',
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
  ): Promise<RR.GetMarketplacePackagesRes> {
    if (params.query) delete params.category
    if (params.ids) params.ids = JSON.stringify(params.ids)

    const qp: RR.GetMarketplacePackagesReq = {
      ...params,
      'eos-version-compat': this.serverInfo['eos-version-compat'],
    }

    return this.api.marketplaceProxy('/package/v0/index', qp, url)
  }

  private get serverInfo(): ServerInfo {
    return this.patch.getData()['server-info']
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

  private onPackages(name: string) {
    const { marketplace } = this.patch.getData().ui

    if (!marketplace?.['selected-id']) {
      return
    }

    const selectedId = marketplace['selected-id']
    const knownHosts = marketplace['known-hosts']

    if (knownHosts[selectedId].name !== name) {
      this.api.setDbValue({
        pointer: `/marketplace/known-hosts/${selectedId}/name`,
        value: name,
      })
    }
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
