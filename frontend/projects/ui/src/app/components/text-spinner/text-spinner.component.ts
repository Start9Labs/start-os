import { Component, Input } from '@angular/core'

@Component({
  selector: 'text-spinner',
  templateUrl: './text-spinner.component.html',
  styleUrls: ['./text-spinner.component.scss'],
})
export class TextSpinnerComponent {
  @Input() text: string
}
