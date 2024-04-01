import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
  Pipe,
  PipeTransform,
} from '@angular/core'
import { AbstractMarketplaceService } from '../../services/marketplace.service'
import { PolymorpheusContent } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'
import { MarketplacePkg } from '../../types'
import { Observable } from 'rxjs'
import { Emver } from '@start9labs/shared'
import { KeyValue } from '@angular/common'

@Component({
  selector: 'release-notes',
  templateUrl: './release-notes.component.html',
  styleUrls: ['./release-notes.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ReleaseNotesComponent {
  constructor(
    private readonly emver: Emver,
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

  asIsOrder(a: KeyValue<string, string>, b: KeyValue<string, string>) {
    return a.key > b.key ? -1 : b.key > a.key ? 1 : 0
  }

  async showReleaseNotes(content: PolymorpheusContent<TuiDialogContext>) {
    this.dialogs
      .open(content, {
        label: 'Previous Release Notes',
      })
      .subscribe()
  }
}

@Pipe({
  name: 'filterVersions',
  standalone: true,
})
export class FilterVersionsPipe implements PipeTransform {
  transform(
    notes: Record<string, string>,
    pkgVersion: string,
  ): Record<string, string> {
    delete notes[pkgVersion]
    return notes
  }
}
