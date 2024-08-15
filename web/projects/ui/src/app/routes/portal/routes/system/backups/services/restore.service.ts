import { inject, Injectable } from '@angular/core'
import { Router } from '@angular/router'
import * as argon2 from '@start9labs/argon2'
import {
  ErrorService,
  LoadingService,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import {
  catchError,
  EMPTY,
  exhaustMap,
  map,
  Observable,
  of,
  switchMap,
  take,
  tap,
} from 'rxjs'
import {
  PROMPT,
  PromptOptions,
} from 'src/app/routes/portal/modals/prompt.component'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { RECOVER } from '../modals/recover.component'
import { SERVERS } from '../modals/servers.component'
import { TARGET, TARGET_RESTORE } from '../modals/target.component'
import { RecoverData } from '../types/recover-data'

@Injectable({
  providedIn: 'root',
})
export class BackupsRestoreService {
  private readonly errorService = inject(ErrorService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly router = inject(Router)
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)

  readonly handle = () => {
    this.dialogs
      .open<BackupTarget>(TARGET, TARGET_RESTORE)
      .pipe(
        // @TODO Alex implement servers
        switchMap(target =>
          this.dialogs
            .open<StartOSDiskInfo & { id: string }>(SERVERS, {
              data: { servers: [] },
            })
            .pipe(
              switchMap(({ id, passwordHash }) =>
                this.dialogs.open<string>(PROMPT, PROMPT_OPTIONS).pipe(
                  exhaustMap(password =>
                    this.getRecoverData(
                      target.id,
                      id,
                      password,
                      passwordHash || '',
                    ),
                  ),
                  take(1),
                  switchMap(data =>
                    this.dialogs.open(RECOVER, {
                      label: 'Select Services to Restore',
                      data,
                    }),
                  ),
                ),
              ),
            ),
        ),
      )
      .subscribe(() => {
        this.router.navigate(['/portal/dashboard'])
      })
  }

  private getRecoverData(
    targetId: string,
    serverId: string,
    password: string,
    hash: string,
  ): Observable<RecoverData> {
    return of(password).pipe(
      tap(() => argon2.verify(hash, password)),
      switchMap(() => {
        const loader = this.loader.open('Decrypting drive...').subscribe()

        return this.api
          .getBackupInfo({ targetId, password })
          .finally(() => loader.unsubscribe())
      }),
      catchError(e => {
        this.errorService.handleError(e)

        return EMPTY
      }),
      map(backupInfo => ({ targetId, password, backupInfo, serverId })),
    )
  }
}

const PROMPT_OPTIONS: Partial<TuiDialogOptions<PromptOptions>> = {
  label: 'Password Required',
  data: {
    message: `Enter the master password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.`,
    label: 'Master Password',
    placeholder: 'Enter master password',
    useMask: true,
  },
}
