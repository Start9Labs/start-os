import { ChangeDetectionStrategy, Component, ElementRef } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { AbstractMarketplaceService } from '../../services/marketplace.service'

@Component({
  selector: 'release-notes',
  templateUrl: './release-notes.component.html',
  styleUrls: ['./release-notes.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ReleaseNotesComponent {
  private readonly pkgId = getPkgId(this.route)

  private selected: string | null = null

  readonly notes$ = this.marketplaceService.fetchReleaseNotes(this.pkgId)

  constructor(
    private readonly route: ActivatedRoute,
    private readonly marketplaceService: AbstractMarketplaceService,
  ) {}

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
