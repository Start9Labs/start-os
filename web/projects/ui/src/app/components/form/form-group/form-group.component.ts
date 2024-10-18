import {
  ChangeDetectionStrategy,
  Component,
  Input,
  ViewEncapsulation,
} from '@angular/core'
import { IST } from '@start9labs/start-sdk'
import { FORM_GROUP_PROVIDERS } from './form-group.providers'

export const ERRORS = [
  'required',
  'pattern',
  'notNumber',
  'numberNotInteger',
  'numberNotInRange',
  'listNotUnique',
  'listNotInRange',
  'listItemIssue',
]

@Component({
  selector: 'form-group',
  templateUrl: './form-group.component.html',
  styleUrls: ['./form-group.component.scss'],
  encapsulation: ViewEncapsulation.None,
  changeDetection: ChangeDetectionStrategy.OnPush,
  viewProviders: [FORM_GROUP_PROVIDERS],
})
export class FormGroupComponent {
  @Input() spec: IST.InputSpec = {}

  asIsOrder() {
    return 0
  }
}
