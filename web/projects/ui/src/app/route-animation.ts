import {
  animate,
  group,
  query,
  style,
  transition,
  trigger,
} from '@angular/animations'
export const slideInAnimation = trigger('routeAnimations', [
  transition('* => *', [
    query(':enter, :leave', style({ position: 'fixed', width: '100%' }), {
      optional: true,
    }),
    group([
      query(
        ':enter',
        [
          style({ transform: 'translateX(-100%)' }),
          animate('1s ease-in-out', style({ transform: 'translateX(0%)' })),
        ],
        { optional: true },
      ),
      query(
        ':leave',
        [
          style({ transform: 'translateX(0%)' }),
          animate('1s ease-in-out', style({ transform: 'translateX(100%)' })),
        ],
        { optional: true },
      ),
    ]),
  ]),
])
