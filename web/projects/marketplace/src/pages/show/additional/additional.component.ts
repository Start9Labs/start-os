import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { CopyService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-additional',
  templateUrl: 'additional.component.html',
  styleUrls: ['additional.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AdditionalComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  @Output()
  readonly static = new EventEmitter<string>()

  constructor(
    readonly copyService: CopyService,
    private readonly dialogs: TuiDialogService,
    private readonly route: ActivatedRoute,
  ) {}

  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined
}
