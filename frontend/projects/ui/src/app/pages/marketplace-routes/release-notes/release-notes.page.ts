import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'

@Component({
  templateUrl: './release-notes.page.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ReleaseNotesPage {
  readonly href = `/marketplace/${getPkgId(this.route)}`

  constructor(private readonly route: ActivatedRoute) {}
}
