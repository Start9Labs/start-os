import { Component, Input, OnInit, ViewEncapsulation } from '@angular/core'
import { DomSanitizer, SafeHtml } from '@angular/platform-browser'

@Component({
  selector: 'app-information-popover',
  templateUrl: './information-popover.component.html',
  styleUrls: ['./information-popover.component.scss'],
  encapsulation: ViewEncapsulation.None,
})
export class InformationPopoverComponent implements OnInit {
  @Input() title: string
  @Input() information: string
  unsafeInformation: SafeHtml
  constructor (private sanitizer: DomSanitizer) { }
  ngOnInit () {
    this.unsafeInformation = this.sanitizer.bypassSecurityTrustHtml(this.information)
  }
}
