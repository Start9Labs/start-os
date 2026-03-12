import { Component, computed, inject, Injectable, signal } from '@angular/core'
import { toObservable } from '@angular/core/rxjs-interop'
import { ErrorService } from '@start9labs/shared'
import { TuiLoader } from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import {
  catchError,
  EMPTY,
  filter,
  from,
  interval,
  Subscription,
  switchMap,
  takeWhile,
} from 'rxjs'
import { T } from '@start9labs/start-sdk'
import { ApiService } from './api/api.service'
import { AuthService } from './auth.service'

@Component({
  template: '<tui-loader size="xl" [textContent]="text" />',
  imports: [TuiLoader],
})
class UpdatingDialog {
  protected readonly text = 'StartTunnel is updating...'
}

@Injectable({
  providedIn: 'root',
})
export class UpdateService {
  private readonly api = inject(ApiService)
  private readonly auth = inject(AuthService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)

  readonly result = signal<T.Tunnel.TunnelUpdateResult | null>(null)
  readonly hasUpdate = computed(
    () => this.result()?.status === 'update-available',
  )
  readonly installed = computed(() => this.result()?.installed ?? null)
  readonly candidate = computed(() => this.result()?.candidate ?? null)

  private polling = false
  private updatingDialog: Subscription | null = null

  constructor() {
    toObservable(this.auth.authenticated)
      .pipe(filter(Boolean))
      .subscribe(() => this.initCheck())
  }

  async checkUpdate(): Promise<void> {
    const result = await this.api.checkUpdate()
    this.setResult(result)
  }

  async applyUpdate(): Promise<void> {
    const result = await this.api.applyUpdate()
    this.setResult(result)
  }

  private setResult(result: T.Tunnel.TunnelUpdateResult): void {
    this.result.set(result)

    if (result.status === 'updating') {
      this.showUpdatingDialog()
      this.startPolling()
    } else {
      this.hideUpdatingDialog()
    }
  }

  private async initCheck(): Promise<void> {
    try {
      await this.checkUpdate()
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  private startPolling(): void {
    if (this.polling) return
    this.polling = true

    interval(5000)
      .pipe(
        switchMap(() =>
          from(this.api.checkUpdate()).pipe(catchError(() => EMPTY)),
        ),
        takeWhile(result => result.status === 'updating', true),
      )
      .subscribe({
        next: result => this.result.set(result),
        complete: () => {
          this.polling = false
          this.hideUpdatingDialog()
        },
        error: () => {
          this.polling = false
          this.hideUpdatingDialog()
        },
      })
  }

  private showUpdatingDialog(): void {
    if (this.updatingDialog) return
    this.updatingDialog = this.dialogs
      .open(new PolymorpheusComponent(UpdatingDialog), {
        closable: false,
        dismissible: false,
      })
      .subscribe({ complete: () => (this.updatingDialog = null) })
  }

  private hideUpdatingDialog(): void {
    this.updatingDialog?.unsubscribe()
    this.updatingDialog = null
  }
}
