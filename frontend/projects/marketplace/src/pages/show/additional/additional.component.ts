import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
  TemplateRef,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import {
  TuiAlertService,
  TuiDialogContext,
  TuiDialogService,
} from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import {
  CopyService,
  copyToClipboard,
  displayEmver,
  Emver,
  MarkdownComponent,
} from '@start9labs/shared'
import { filter } from 'rxjs'
import { MarketplacePkg } from '../../../types'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'

@Component({
  selector: 'marketplace-additional',
  templateUrl: 'additional.component.html',
  styleUrls: ['additional.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AdditionalComponent {
  @Input()
  pkg!: MarketplacePkg

  @Output()
  version = new EventEmitter<string>()

  readonly displayEmver = displayEmver

  constructor(
    readonly copyService: CopyService,
    private readonly alerts: TuiAlertService,
    private readonly dialogs: TuiDialogService,
    private readonly emver: Emver,
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly route: ActivatedRoute,
  ) {}

  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined

  presentAlertVersions(version: TemplateRef<TuiDialogContext>) {
    this.dialogs
      .open<string>(version, {
        label: 'Versions',
        size: 's',
        data: {
          value: this.pkg.manifest.version,
          items: this.pkg.versions.sort(
            (a, b) => -1 * (this.emver.compare(a, b) || 0),
          ),
        },
      })
      .pipe(filter(Boolean))
      .subscribe(version => this.version.emit(version))
  }

  presentModalMd(label: string) {
    this.dialogs
      .open(new PolymorpheusComponent(MarkdownComponent), {
        label,
        size: 'l',
        data: {
          content: this.marketplaceService.fetchStatic$(
            this.pkg.manifest.id,
            label.toLowerCase(),
            this.url,
          ),
        },
      })
      .subscribe()
  }
}
