import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ErrorToastService, getPkgId } from '@start9labs/shared'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
} from '@start9labs/marketplace'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { BehaviorSubject, Observable, of } from 'rxjs'
import { catchError, filter, shareReplay, switchMap } from 'rxjs/operators'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowPage {
  private readonly pkgId = getPkgId(this.route)

  readonly loadVersion$ = new BehaviorSubject<string>('*')

  readonly localPkg$ = this.patch
    .watch$('package-data', this.pkgId)
    .pipe(filter(Boolean), shareReplay({ bufferSize: 1, refCount: true }))

  readonly pkg$: Observable<MarketplacePkg | null> = this.loadVersion$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage(this.pkgId, version),
    ),
    // TODO: Better fallback
    catchError(e => {
      this.errToast.present(e)

      return of({} as MarketplacePkg)
    }),
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
}
