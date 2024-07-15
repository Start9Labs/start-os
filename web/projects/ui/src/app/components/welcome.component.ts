import { Component, inject, Inject } from '@angular/core'
import { TuiDialogContext, TuiButton } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  template: `
    <h3>
      This release:
      <em>0.3.5</em>
    </h3>
    <p>
      View the complete
      <a
        href="https://github.com/Start9Labs/start-os/releases/tag/v0.3.5"
        target="_blank"
        noreferrer
      >
        release notes
      </a>
      for more details.
    </p>
    <h5>Highlights</h5>
    <ul class="spaced-list">
      <li>
        This release contains significant under-the-hood improvements to
        performance and reliability
      </li>
      <li>Ditch Docker, replace with Podman</li>
      <li>Remove locking behavior from PatchDB and optimize</li>
      <li>Boost efficiency of service manager</li>
      <li>Require HTTPS on LAN, and improve setup flow for trusting Root CA</li>
      <li>Better default privacy settings for Firefox kiosk mode</li>
      <li>Eliminate memory leak from Javascript runtime</li>
      <li>Other small bug fixes</li>
      <li>Update license to MIT</li>
    </ul>

    <p [style.text-align]="'center'">
      <button tuiButton (click)="context.$implicit.complete()">Begin</button>
    </p>
  `,
  styles: 'li { margin-bottom: 0.5rem }',
  imports: [TuiButton],
})
export class WelcomeComponent {
  readonly context = inject<TuiDialogContext>(POLYMORPHEUS_CONTEXT)
}
