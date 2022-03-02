import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'

@Component({
  selector: 'release-notes-header',
  templateUrl: 'release-notes-header.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ReleaseNotesHeaderComponent {
  readonly href = `/marketplace/${this.route.snapshot.paramMap.get('pkgId')}`

  constructor(private readonly route: ActivatedRoute) {}
}
