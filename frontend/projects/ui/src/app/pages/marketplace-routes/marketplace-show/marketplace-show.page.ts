import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ErrorToastService } from '@start9labs/shared'
import {
  LocalPkg,
  MarketplacePkg,
  AbstractMarketplaceService,
  spreadProgress,
} from '@start9labs/marketplace'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { BehaviorSubject, defer, Observable, of } from 'rxjs'
import {
  catchError,
  filter,
  shareReplay,
  startWith,
  switchMap,
  tap,
} from 'rxjs/operators'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowPage {
  private readonly pkgId = this.route.snapshot.paramMap.get('pkgId')

  readonly loadVersion$ = new BehaviorSubject<string>('*')

  readonly localPkg$ = defer(() =>
    this.patch.watch$('package-data', this.pkgId),
  ).pipe(
    filter<LocalPkg>(Boolean),
    tap(spreadProgress),
    shareReplay({ bufferSize: 1, refCount: true }),
  )

  readonly pkg$: Observable<MarketplacePkg> = this.loadVersion$.pipe(
    switchMap(version =>
      this.marketplaceService
        .getPackage(this.pkgId, version)
        .pipe(startWith(null)),
    ),
    // TODO: Better fallback
    catchError(e => this.errToast.present(e) && of({} as MarketplacePkg)),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly patch: PatchDbService,
    private readonly marketplaceService: AbstractMarketplaceService,
  ) {}

  getIcon(icon: string): string {
    return `data:image/png;base64,${icon}`
  }

  // async getPkg(version: string): Promise<void> {
  // this.loading = true
  // try {
  //   this.pkg = await this.marketplaceService.getPkg(this.pkgId, version)
  // } catch (e) {
  //   this.errToast.present(e)
  // } finally {
  //   await pauseFor(100)
  // this.loading = false
  // }
  // }
}
