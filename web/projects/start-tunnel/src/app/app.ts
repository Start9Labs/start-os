import { TuiRoot } from '@taiga-ui/core'
import { Component, inject } from '@angular/core'
import { RouterOutlet } from '@angular/router'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { PatchService } from './services/patch.service'

@Component({
  selector: 'app-root',
  imports: [RouterOutlet, TuiRoot],
  templateUrl: './app.html',
  styleUrl: './app.scss',
})
export class App {
  readonly subscription = inject(PatchService)
    .pipe(takeUntilDestroyed())
    .subscribe()
}
