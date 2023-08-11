import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { AbstractMarketplaceService } from '../../services/marketplace.service'
import { PolymorpheusContent } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'

@Component({
  selector: 'release-notes',
  templateUrl: './release-notes.component.html',
  styleUrls: ['./release-notes.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ReleaseNotesComponent {
  constructor(
    private readonly route: ActivatedRoute,
    private readonly marketplaceService: AbstractMarketplaceService,
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
  ) {}

  private readonly pkgId = getPkgId(this.route)
  readonly pkg$ = this.marketplaceService.getPackage$(this.pkgId, '*')
  readonly notes$ = this.marketplaceService.fetchReleaseNotes$(this.pkgId)

  asIsOrder(a: any, b: any) {
    return 0
  }

  async showReleaseNotes(content: PolymorpheusContent<TuiDialogContext>) {
    this.dialogs
      .open(content, {
        label: 'Previous Release Notes',
      })
      .subscribe()
  }
}
