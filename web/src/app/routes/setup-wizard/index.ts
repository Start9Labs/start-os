import { DecimalPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  linkedSignal,
  signal,
} from '@angular/core'
import {
  AbstractControl,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidationErrors,
  Validators,
} from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiCell,
  TuiError,
  TuiIcon,
  TuiInput,
  TuiLabel,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiButtonLoading, TuiPassword, TuiProgress } from '@taiga-ui/kit'
import { SetupFlashEvent } from 'src/app/services/api/api.service'
import { AuthService } from 'src/app/services/auth.service'

type Step = 'welcome' | 'password' | 'confirm' | 'flashing' | 'complete'
type FlashMode = 'update' | 'fresh-start'

function passwordsMatch(control: AbstractControl): ValidationErrors | null {
  const password = control.get('password')?.value
  const confirm = control.get('confirm')?.value
  if (password && confirm && password !== confirm) {
    control.get('confirm')?.setErrors({ mismatch: true })
    return { mismatch: true }
  }
  return null
}

@Component({
  template: `
    <img alt="StartWRT" src="assets/favicon.svg" />

    @switch (step()) {
      @case ('welcome') {
        <h2>StartWRT Setup</h2>
        <p class="subtitle">Choose how to set up your router</p>
        <div class="options">
          @if (canUpdate()) {
            <button tuiCell="l" (click)="selectMode('update')">
              <div tuiTitle>
                Keep settings
                <div tuiSubtitle>
                  Preserve your configuration, update firmware
                </div>
              </div>
            </button>
          }
          <button tuiCell="l" (click)="selectMode('fresh-start')">
            <div tuiTitle>
              Fresh Start
              <div tuiSubtitle>
                Erase all router configuration, install fresh firmware
              </div>
            </div>
          </button>
          @if (!emmcFound()) {
            <tui-error error="No eMMC device found" />
          }
        </div>
      }
      @case ('password') {
        <h2>Create admin password</h2>
        <form [formGroup]="form" (ngSubmit)="onPasswordSubmit()">
          <tui-textfield>
            <label tuiLabel>Password</label>
            <input tuiInput formControlName="password" type="password" />
            <tui-icon tuiPassword />
          </tui-textfield>
          <tui-error formControlName="password" />
          <tui-textfield>
            <label tuiLabel>Confirm password</label>
            <input tuiInput formControlName="confirm" type="password" />
            <tui-icon tuiPassword />
          </tui-textfield>
          <tui-error formControlName="confirm" />
          <div class="form-actions">
            <button
              tuiButton
              appearance="flat"
              type="button"
              (click)="step.set('welcome')"
            >
              Back
            </button>
            <button tuiButton appearance="primary" type="submit">Next</button>
          </div>
        </form>
      }
      @case ('confirm') {
        <h2>Confirm</h2>
        @if (mode() === 'update') {
          <p>Firmware will be updated. Your settings will be preserved.</p>
        } @else {
          <p>
            All router configuration (network, firewall, packages) will be
            erased and firmware freshly installed. Your admin password and WiFi
            credentials will be applied to the new installation.
          </p>
        }
        <p class="subtitle">This will take a few minutes.</p>
        <div class="form-actions">
          <button tuiButton appearance="flat" (click)="step.set('password')">
            Back
          </button>
          <button
            tuiButton
            appearance="primary"
            autofocus
            [loading]="flashing()"
            (click)="startFlash()"
          >
            Flash
          </button>
        </div>
      }
      @case ('flashing') {
        <h2>Flashing</h2>
        <p>{{ flashStatus() }}</p>
        @if (flashTotalSteps() > 0) {
          <progress
            tuiProgressBar
            [max]="flashTotalSteps()"
            [value]="flashStep() - 1"
          ></progress>
          <p class="hint">Step {{ flashStep() }} of {{ flashTotalSteps() }}</p>
        }
        @if (flashTotal() > 0) {
          <progress
            tuiProgressBar
            [max]="flashTotal()"
            [value]="flashCopied()"
          ></progress>
          <p class="hint">
            {{ flashCopied() / 1000000 | number: '1.0-0' }} /
            {{ flashTotal() / 1000000 | number: '1.0-0' }} MB
          </p>
        }
        @if (flashError()) {
          <tui-error [error]="flashError()" />
          <button tuiButton appearance="flat" (click)="step.set('confirm')">
            Try Again
          </button>
        }
      }
      @case ('complete') {
        <h2>Setup complete</h2>
        <p>Remove the microSD card and reboot your router.</p>
      }
    }
  `,
  styles: `
    :host {
      height: 100%;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 1.5rem;
    }

    img {
      width: 5rem;
      height: 5rem;
    }

    h2 {
      margin: 0;
    }

    p {
      margin: 0;
      text-align: center;
      max-width: 24rem;
    }

    .subtitle {
      color: var(--tui-text-secondary);
    }

    .hint {
      color: var(--tui-text-tertiary);
      font-size: 0.85rem;
    }

    .options {
      display: flex;
      flex-direction: column;
      gap: 0.25rem;
      width: 20rem;
    }

    form {
      display: flex;
      flex-direction: column;
      gap: 0.75rem;
      width: 20rem;
    }

    .form-actions {
      display: flex;
      justify-content: space-between;
      margin-top: 0.5rem;
    }

    progress[tuiProgressBar] {
      width: 20rem;
    }

    :host-context(body:not([tuiTheme])) {
      img {
        filter: invert(1);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [
    tuiValidationErrorsProvider({
      required: 'This field is required',
      minlength: 'Password must be at least 12 characters',
      mismatch: 'Passwords do not match',
    }),
  ],
  imports: [
    DecimalPipe,
    ReactiveFormsModule,
    TuiButton,
    TuiButtonLoading,
    TuiCell,
    TuiError,
    TuiIcon,
    TuiInput,
    TuiLabel,
    TuiPassword,
    TuiProgress,
    TuiTextfield,
    TuiTitle,
  ],
})
export default class SetupWizard {
  private readonly auth = inject(AuthService)

  protected readonly step = signal<Step>('welcome')
  protected readonly mode = signal<FlashMode>('fresh-start')
  protected readonly flashing = signal(false)
  protected readonly flashStatus = signal('Preparing...')
  protected readonly flashStep = signal(0)
  protected readonly flashTotalSteps = signal(0)
  protected readonly flashCopied = signal(0)
  protected readonly flashTotal = signal(0)
  protected readonly flashError = linkedSignal<string | null>(() => null)

  protected readonly emmcFound = computed(
    () => this.auth.setupDisk()?.emmcFound ?? false,
  )
  protected readonly canUpdate = computed(
    () => this.auth.setupDisk()?.hasFirmware ?? false,
  )

  public readonly form = inject(NonNullableFormBuilder).group(
    {
      password: ['', [Validators.required, Validators.minLength(12)]],
      confirm: ['', Validators.required],
    },
    { validators: passwordsMatch },
  )

  selectMode(mode: FlashMode): void {
    this.mode.set(mode)
    this.step.set('password')
  }

  onPasswordSubmit(): void {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }
    this.step.set('confirm')
  }

  async startFlash(): Promise<void> {
    this.flashing.set(true)
    this.flashError.set(null)
    this.flashStep.set(0)
    this.flashTotalSteps.set(0)
    this.flashCopied.set(0)
    this.flashTotal.set(0)
    this.flashStatus.set('Starting flash...')
    this.step.set('flashing')

    try {
      const response = await fetch('/api/setup/flash', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          mode: this.mode(),
          password: this.form.value.password,
        }),
      })

      if (!response.ok || !response.body) {
        throw new Error(`HTTP ${response.status}`)
      }

      const reader = response.body.getReader()
      const decoder = new TextDecoder()
      let buffer = ''

      while (true) {
        const { done, value } = await reader.read()
        if (done) break

        buffer += decoder.decode(value, { stream: true })
        const lines = buffer.split('\n')
        buffer = lines.pop()!

        for (const line of lines) {
          if (!line.trim()) continue
          const event: SetupFlashEvent = JSON.parse(line)
          this.handleFlashEvent(event)
        }
      }

      // Process any remaining buffer
      if (buffer.trim()) {
        const event: SetupFlashEvent = JSON.parse(buffer)
        this.handleFlashEvent(event)
      }
    } catch (e: any) {
      this.flashError.set(e?.message || 'Flash failed')
    } finally {
      this.flashing.set(false)
    }
  }

  private handleFlashEvent(event: SetupFlashEvent): void {
    if (event.step) this.flashStep.set(event.step)
    if (event.totalSteps) this.flashTotalSteps.set(event.totalSteps)

    switch (event.phase) {
      case 'copying':
        this.flashStatus.set('Copying firmware...')
        this.flashCopied.set(event.copied ?? 0)
        this.flashTotal.set(event.total ?? 0)
        break
      case 'status':
        this.flashStatus.set(event.message ?? '')
        this.flashTotal.set(0)
        break
      case 'complete':
        this.step.set('complete')
        break
      case 'error':
        this.flashError.set(event.message ?? 'Unknown error')
        break
    }
  }
}
