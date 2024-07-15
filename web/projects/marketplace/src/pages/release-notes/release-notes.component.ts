import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
  Pipe,
  PipeTransform,
} from '@angular/core'
import { AbstractMarketplaceService } from '../../services/marketplace.service'
import { PolymorpheusContent } from '@taiga-ui/polymorpheus'
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
    const a1 = a.key.split('.')
    const b1 = b.key.split('.')
    // contingency in case there's a 4th or 5th version
    const len = Math.min(a1.length, b1.length)
    // look through each version number and compare.
    for (let i = 0; i < len; i++) {
      const a2 = +a1[i] || 0
      const b2 = +b1[i] || 0

      if (a2 !== b2) {
        // sort descending
        return a2 > b2 ? -1 : 1
      }
    }
    return a1.length - b1.length
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
