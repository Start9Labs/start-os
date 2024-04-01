import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AbstractMarketplaceService } from '../../services/marketplace.service'
import { PolymorpheusContent } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'
import { MarketplacePkg } from '../../types'
import { Observable } from 'rxjs'

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

  @Input({ required: true })
  pkg!: MarketplacePkg

  notes$!: Observable<Record<string, string>>

  ngOnChanges() {
    this.notes$ = this.marketplaceService.fetchReleaseNotes$(
      this.pkg.manifest.id,
    )
  }

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
