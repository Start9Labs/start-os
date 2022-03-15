import { ChangeDetectionStrategy, Component, ElementRef } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AbstractMarketplaceService } from '@start9labs/marketplace'

@Component({
  selector: 'release-notes',
  templateUrl: './release-notes.page.html',
  styleUrls: ['./release-notes.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ReleaseNotesPage {
  private readonly pkgId = this.route.snapshot.paramMap.get('pkgId')

  private selected: string | null = null

  readonly notes$ = this.marketplaceService.getReleaseNotes(this.pkgId)

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
