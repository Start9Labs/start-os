import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'

@Component({
  templateUrl: './release-notes.page.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ReleaseNotesPage {
  readonly href = `/marketplace/${this.route.snapshot.paramMap.get('pkgId')}`

  constructor(private readonly route: ActivatedRoute) {}
}
