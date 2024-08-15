import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TuiDialogService } from '@taiga-ui/core'
import { RELEASE_NOTES } from '../../../modals/release-notes.component'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-about',
  templateUrl: 'about.component.html',
  styleUrls: ['about.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AboutComponent {
  private readonly dialogs = inject(TuiDialogService)

  @Input({ required: true })
  pkg!: MarketplacePkg

  async onPast() {
    this.dialogs
      .open(RELEASE_NOTES, { label: 'Past Release Notes' })
      .subscribe()
  }
}
