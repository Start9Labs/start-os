import { Component, Input } from '@angular/core'
import { Router } from '@angular/router'
import { PopoverController } from '@ionic/angular'
import { filter, take } from 'rxjs/operators'
import { capitalizeFirstLetter } from 'src/app/util/misc.util'
import { InformationPopoverComponent } from '../information-popover/information-popover.component'

@Component({
  selector: 'recommendation-button',
  templateUrl: './recommendation-button.component.html',
  styleUrls: ['./recommendation-button.component.scss'],
})
export class RecommendationButtonComponent {
  @Input() rec: Recommendation
  @Input() raise?: { id: string }
  constructor (
    private readonly router: Router,
    private readonly popoverController: PopoverController,
  ) { }

  ngOnInit () {
    if (!this.raise) return
    const mainContent = document.getElementsByTagName('ion-app')[0]
    const recButton = document.getElementById(this.raise.id)
    mainContent.appendChild(recButton)

    this.router.events.pipe(filter(e => !!(e as any).urlAfterRedirects, take(1))).subscribe((e: any) => {
      recButton.remove()
    })
  }

  disabled = false

  async presentPopover (ev: any) {
    const popover = await this.popoverController.create({
      component: InformationPopoverComponent,
      event: ev,
      translucent: false,
      showBackdrop: true,
      backdropDismiss: true,
      componentProps: {
        information: `
          <div style="font-size: medium; font-style: italic; margin: 5px 0px;">
            ${capitalizeFirstLetter(this.rec.dependentTitle)} Installation Recommendations
          </div>
          <div>
            ${this.rec.description}
          </div>`,
      },
    })
    popover.onWillDismiss().then(() => {
      this.disabled = false
    })
    this.disabled = true
    return await popover.present()
  }
}

export type Recommendation = {
  dependentId: string
  dependentTitle: string
  dependentIcon: string,
  description: string
  version?: string
}
