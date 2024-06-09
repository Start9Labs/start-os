import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { delay, filter } from 'rxjs'
import { InitService } from 'src/app/pages/init/init.service'

@Component({
  selector: 'init-page',
  templateUrl: 'init.page.html',
  styleUrls: ['init.page.scss'],
})
export class InitPage {
  private readonly router = inject(Router)
  readonly progress$ = inject(InitService)

  readonly sub = this.progress$
    .pipe(
      filter(progress => progress === 1),
      delay(500),
    )
    .subscribe(() => {
      this.router.navigate([''])
    })

  getMessage(progress: number | null): string {
    if (!progress) {
      return 'Preparing data. This can take a while'
    } else if (progress < 1) {
      return 'Copying data'
    } else {
      return 'Finalizing'
    }
  }
}
