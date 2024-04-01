// import { CommonModule } from '@angular/common'
// import {
//   ChangeDetectionStrategy,
//   Component,
//   HostListener,
//   Input,
// } from '@angular/core'
// import {
//   TuiBadgedContentModule,
//   TuiBadgeNotificationModule,
//   TuiButtonModule,
//   TuiIconModule,
// } from '@taiga-ui/experimental'
// import { RouterLink } from '@angular/router'
// import { TickerModule } from '@start9labs/shared'
// import { TuiDataListModule, TuiHostedDropdownModule } from '@taiga-ui/core'
// import { Action, ActionsComponent } from './actions.component'

// @Component({
//   selector: '[appCard]',
//   template: `
//     <span class="link">
//       <tui-badged-content [style.--tui-radius.rem]="1.5">
//         @if (badge) {
//           <tui-badge-notification size="m" tuiSlot="top">
//             {{ badge }}
//           </tui-badge-notification>
//         }
//         @if (icon?.startsWith('tuiIcon')) {
//           <tui-icon class="icon" [icon]="icon" />
//         } @else {
//           <img alt="" class="icon" [src]="icon" />
//         }
//       </tui-badged-content>
//       <label ticker class="title">{{ title }}</label>
//     </span>
//     @if (isService) {
//       <span class="side">
//         <tui-hosted-dropdown
//           [content]="content"
//           (click.stop.prevent)="(0)"
//           (pointerdown.stop)="(0)"
//         >
//           <button
//             tuiIconButton
//             appearance="outline"
//             size="xs"
//             iconLeft="tuiIconMoreHorizontal"
//             [style.border-radius.%]="100"
//           >
//             Actions
//           </button>
//           <ng-template #content let-close="close">
//             <app-actions [actions]="actions" (click)="close()">
//               {{ title }}
//             </app-actions>
//           </ng-template>
//         </tui-hosted-dropdown>
//       </span>
//     }
//   `,
//   styles: [
//     `
//       :host {
//         display: flex;
//         height: 5.5rem;
//         width: 12.5rem;
//         border-radius: var(--tui-radius-l);
//         overflow: hidden;
//         box-shadow: 0 0.25rem 0.25rem rgb(0 0 0 / 25%);
//         // TODO: Theme
//         background: rgb(111 109 109);
//       }

//       .link {
//         display: flex;
//         flex: 1;
//         flex-direction: column;
//         align-items: center;
//         justify-content: center;
//         color: white;
//         gap: 0.25rem;
//         padding: 0 0.5rem;
//         font: var(--tui-font-text-m);
//         white-space: nowrap;
//         overflow: hidden;
//       }

//       .icon {
//         width: 2.5rem;
//         height: 2.5rem;
//         border-radius: 100%;
//         color: var(--tui-text-01-night);
//       }

//       .side {
//         width: 3rem;
//         display: flex;
//         align-items: center;
//         justify-content: center;
//         box-shadow: 0 0.25rem 0.25rem rgb(0 0 0 / 25%);
//         // TODO: Theme
//         background: #4b4a4a;
//       }
//     `,
//   ],
//   standalone: true,
//   changeDetection: ChangeDetectionStrategy.OnPush,
//   imports: [
//     CommonModule,
//     RouterLink,
//     TuiButtonModule,
//     TuiHostedDropdownModule,
//     TuiDataListModule,
//     TuiIconModule,
//     TickerModule,
//     TuiBadgedContentModule,
//     TuiBadgeNotificationModule,
//     ActionsComponent,
//   ],
// })
// export class CardComponent {
//   @Input({ required: true })
//   id!: string

//   @Input({ required: true })
//   icon!: string

//   @Input({ required: true })
//   title!: string

//   @Input()
//   actions: Record<string, readonly Action[]> = {}

//   @Input()
//   badge: number | null = null

//   get isService(): boolean {
//     return !this.id.includes('/')
//   }

//   // Prevents Firefox from starting a native drag
//   @HostListener('pointerdown.prevent')
//   onDown() {}
// }
