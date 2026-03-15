import {
  AfterViewInit,
  Component,
  DOCUMENT,
  ElementRef,
  inject,
  ViewChild,
} from '@angular/core'
import {
  DialogService,
  DownloadHTMLService,
  ErrorService,
  i18nPipe,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiCell, TuiIcon, TuiLoader, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { DocumentationComponent } from '../components/documentation.component'
import { MatrixComponent } from '../components/matrix.component'
import { MokEnrollmentDialog } from '../components/mok-enrollment.dialog'
import { RemoveMediaDialog } from '../components/remove-media.dialog'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'

@Component({
  template: `
    <canvas matrix></canvas>
    <section tuiCardLarge>
      <header tuiHeader>
        <h2 tuiTitle>
          <span class="inline-title">
            <tui-icon icon="@tui.circle-check-big" class="g-positive" />
            {{ 'Setup Complete!' | i18n }}
          </span>
          @if (!stateService.kiosk) {
            <span tuiSubtitle>
              {{
                'http://start.local was for setup only. It will no longer work.'
                  | i18n
              }}
            </span>
          }
        </h2>
      </header>

      @if (!result) {
        <tui-loader />
      } @else {
        <!-- Step: Download Address Info (non-kiosk only) -->
        @if (!stateService.kiosk) {
          <button tuiCell="l" (click)="download()">
            <span tuiAvatar="@tui.download" appearance="secondary"></span>
            <div tuiTitle>
              {{ 'Download Address Info' | i18n }}
              <div tuiSubtitle>
                {{
                  "Contains your server's permanent local address and Root CA"
                    | i18n
                }}
              </div>
            </div>
            @if (downloaded) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>
        }

        <!-- Step: Restart flow -->
        @if (result.needsRestart) {
          <button
            tuiCell="l"
            [class.disabled]="!stateService.kiosk && !downloaded"
            [disabled]="!stateService.kiosk && !downloaded"
            (click)="removeMedia()"
          >
            <span tuiAvatar="@tui.usb" appearance="secondary"></span>
            <div tuiTitle>
              {{ 'Remove Installation Media' | i18n }}
              <div tuiSubtitle>
                {{
                  'Remove USB stick or other installation media from your server'
                    | i18n
                }}
              </div>
            </div>
            @if (usbRemoved) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>

          <!-- Step: Secure Boot Enrollment (when MOK enrolled) -->
          @if (stateService.mokEnrolled) {
            <button
              tuiCell="l"
              [class.disabled]="!usbRemoved"
              [disabled]="!usbRemoved"
              (click)="acknowledgeMok()"
            >
              <span tuiAvatar="@tui.shield-check" appearance="secondary"></span>
              <div tuiTitle>
                {{ 'Secure Boot Enrollment' | i18n }}
                <div tuiSubtitle>
                  {{
                    'Prepare for Secure Boot key enrollment on the next reboot'
                      | i18n
                  }}
                </div>
              </div>
              @if (mokAcknowledged) {
                <tui-icon icon="@tui.circle-check" class="g-positive" />
              }
            </button>
          }

          <!-- Step: Restart Server -->
          <button
            tuiCell="l"
            [class.disabled]="
              !usbRemoved || (stateService.mokEnrolled && !mokAcknowledged)
            "
            [disabled]="
              !usbRemoved || (stateService.mokEnrolled && !mokAcknowledged)
            "
            (click)="reboot()"
          >
            <span tuiAvatar="@tui.rotate-cw" appearance="secondary"></span>
            <div tuiTitle>
              {{ 'Restart Server' | i18n }}
              <div tuiSubtitle>
                @if (rebooting) {
                  {{ 'Waiting for server to come back online' | i18n }}
                } @else if (rebooted) {
                  {{ 'Server is back online' | i18n }}
                } @else {
                  {{ 'Restart your server to complete setup' | i18n }}
                }
              </div>
            </div>
            @if (rebooting) {
              <tui-loader />
            } @else if (rebooted) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>
        } @else if (stateService.kiosk) {
          <button tuiCell="l" (click)="exitKiosk()">
            <span tuiAvatar="@tui.log-in" appearance="secondary"></span>
            <div tuiTitle>
              {{ 'Continue to Login' | i18n }}
              <div tuiSubtitle>
                {{ 'Proceed to the StartOS login screen' | i18n }}
              </div>
            </div>
          </button>
        }

        <!-- Step: Open Local Address (non-kiosk only) -->
        @if (!stateService.kiosk) {
          <button
            tuiCell="l"
            [class.disabled]="!canOpenAddress"
            [disabled]="!canOpenAddress"
            (click)="openLocalAddress()"
          >
            <span tuiAvatar="@tui.external-link" appearance="secondary"></span>
            <div tuiTitle>
              {{ 'Open Local Address' | i18n }}
              <div tuiSubtitle>{{ lanAddress }}</div>
            </div>
          </button>

          <app-documentation hidden [lanAddress]="lanAddress" />
        }
      }
    </section>
  `,
  styles: `
    .inline-title {
      display: inline-flex;
      align-items: center;
      gap: 0.5rem;
    }

    [tuiCell].disabled {
      opacity: var(--tui-disabled-opacity);
      pointer-events: none;
    }
  `,
  imports: [
    TuiCardLarge,
    TuiCell,
    TuiIcon,
    TuiLoader,
    TuiAvatar,
    MatrixComponent,
    DocumentationComponent,
    TuiHeader,
    TuiTitle,
    i18nPipe,
  ],
})
export default class SuccessPage implements AfterViewInit {
  @ViewChild(DocumentationComponent, { read: ElementRef })
  private readonly documentation?: ElementRef<HTMLElement>

  private readonly document = inject(DOCUMENT)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly downloadHtml = inject(DownloadHTMLService)
  private readonly dialogs = inject(DialogService)
  private readonly i18n = inject(i18nPipe)

  readonly stateService = inject(StateService)

  result?: T.SetupResult
  lanAddress = ''
  downloaded = false
  usbRemoved = false
  mokAcknowledged = false
  rebooting = false
  rebooted = false

  get canOpenAddress(): boolean {
    if (!this.downloaded) return false
    if (this.result?.needsRestart && !this.rebooted) return false
    return true
  }

  ngAfterViewInit() {
    setTimeout(() => this.complete(), 500)
  }

  download() {
    const lanElem = this.document.getElementById('lan-addr')
    if (lanElem) lanElem.innerHTML = this.lanAddress

    this.document
      .getElementById('cert')
      ?.setAttribute(
        'href',
        `data:application/octet-stream;base64,${this.result!.rootCa}`,
      )

    const html = this.documentation?.nativeElement.innerHTML || ''

    this.downloadHtml.download('StartOS-info.html', html).then(() => {
      this.downloaded = true
    })
  }

  removeMedia() {
    this.dialogs
      .openComponent<boolean>(new PolymorpheusComponent(RemoveMediaDialog), {
        dismissible: false,
        closable: false,
      })
      .subscribe(() => {
        this.usbRemoved = true
      })
  }

  acknowledgeMok() {
    this.dialogs
      .openComponent<boolean>(new PolymorpheusComponent(MokEnrollmentDialog), {
        label: 'Secure Boot',
        size: 'm',
        dismissible: false,
        closable: false,
      })
      .subscribe(() => {
        this.mokAcknowledged = true
      })
  }

  exitKiosk() {
    this.api.exit()
  }

  openLocalAddress() {
    window.open(this.lanAddress, '_blank')
  }

  async reboot() {
    if (this.rebooting || this.rebooted) return

    this.rebooting = true

    try {
      await this.api.exit()
      await this.pollForServer()
      this.rebooted = true
      this.rebooting = false
    } catch (e: any) {
      this.errorService.handleError(e)
      this.rebooting = false
    }
  }

  private async complete() {
    try {
      this.result = await this.api.complete()

      if (!this.stateService.kiosk) {
        this.lanAddress = `http://${this.result.hostname}.local`

        if (!this.result.needsRestart) {
          await this.api.exit()
        }
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  private async pollForServer(): Promise<void> {
    const maxAttempts = 60
    let attempts = 0

    while (attempts < maxAttempts) {
      try {
        await this.api.echo({ message: 'ping' }, `${this.lanAddress}/rpc/v1`)
        return
      } catch {
        await new Promise(resolve => setTimeout(resolve, 5000))
        attempts++
      }
    }

    throw new Error(
      this.i18n.transform(
        'Server did not come back online. Please check your server and try accessing it manually.',
      ),
    )
  }
}
