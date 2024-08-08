import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  Input,
} from '@angular/core'
import { AbstractMarketplaceService } from '../../services/marketplace.service'
import { MarketplacePkg } from '../../types'
import { map } from 'rxjs'
import { Exver } from '@start9labs/shared'
// @TODO Alex use Taiga modal
import { ModalController } from '@ionic/angular'

@Component({
  selector: 'release-notes',
  templateUrl: './release-notes.component.html',
  styleUrls: ['./release-notes.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ReleaseNotesComponent {
  @Input() pkg!: MarketplacePkg

  private selected: string | null = null

  readonly notes$ = this.marketplaceService.getSelectedStore$().pipe(
    map(s => {
      return Object.entries(this.pkg.otherVersions)
        .filter(
          ([v, _]) =>
            this.exver.getFlavor(v) === this.pkg.flavor &&
            this.exver.compareExver(this.pkg.version, v) === 1,
        )
        .reduce(
          (obj, [version, info]) => ({
            ...obj,
            [version]: info.releaseNotes,
          }),
          {
            [`${this.pkg.version} (current)`]: this.pkg.releaseNotes,
          },
        )
    }),
  )

  constructor(
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly exver: Exver,
    private readonly modalCtrl: ModalController,
  ) {}

  async dismiss() {
    return this.modalCtrl.dismiss()
  }

  isSelected(key: string): boolean {
    return this.selected === key
  }

  setSelected(selected: string) {
    this.selected = this.isSelected(selected) ? null : selected
  }

  getDocSize(key: string, { nativeElement }: ElementRef<HTMLElement>) {
    return this.isSelected(key) ? nativeElement.scrollHeight : 0
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
