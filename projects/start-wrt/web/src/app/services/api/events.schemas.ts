import { z } from 'zod'
const wire = <A extends z.ZodType, B extends z.ZodType>(input: A, output: B) =>
  z.codec(input, output, { decode: v => v as never, encode: v => v as never })
export const FlashMode = z.enum(['update', 'fresh-start']).meta({
  id: 'FlashMode',
})
export const ProgressUnits = z.enum(['bytes', 'steps']).meta({
  id: 'ProgressUnits',
})

/**
 * Streaming event sent to the frontend during flash.
 */
export const SetupEvent = z
  .union([
    z.intersection(
      z.object({ phase: z.literal('copying') }),
      z.object({
        copied: z.int(),
        total: z.int(),
        step: z.int(),
        totalSteps: z.int(),
      }),
    ),
    z.intersection(
      z.object({ phase: z.literal('status') }),
      z.object({ message: z.string(), step: z.int(), totalSteps: z.int() }),
    ),
    z.object({ phase: z.literal('complete') }),
    z.intersection(
      z.object({ phase: z.literal('error') }),
      z.object({ message: z.string() }),
    ),
  ])
  .meta({
    id: 'SetupEvent',
    description: 'Streaming event sent to the frontend during flash.',
  })
export const FlashParams = z
  .object({
    mode: FlashMode,
    password: z.string(),
    /**
     * Browser IANA timezone (e.g. "America/New_York") captured by the wizard.
     * Carried into the fresh eMMC config since the wizard's live set-timezone
     * only reaches the throwaway microSD overlay. Optional for older clients.
     */
    timezone: wire(
      z.string().nullable().optional(),
      z.string().nullable(),
    ).meta({
      description:
        'Browser IANA timezone (e.g. "America/New_York") captured by the wizard.\nCarried into the fresh eMMC config since the wizard\'s live set-timezone\nonly reaches the throwaway microSD overlay. Optional for older clients.',
    }),
  })
  .meta({
    id: 'FlashParams',
  })

/**
 * Progress state for a single phase or overall operation.
 * Wire-compatible with start-os's `Progress` (serialized as untagged).
 */
export const Progress = z
  .union([
    z.null().meta({
      description: 'Not started yet — serializes as `null`',
    }),
    z.boolean().meta({
      description:
        'Complete — serializes as `true` (success) or `false` (failure)',
    }),
    z
      .object({
        done: z.int(),
        total: wire(z.int().nullable().optional(), z.int().nullable()),
        units: wire(
          ProgressUnits.nullable().optional(),
          ProgressUnits.nullable(),
        ),
      })
      .meta({
        description: 'In progress',
      }),
  ])
  .meta({
    id: 'Progress',
    description:
      "Progress state for a single phase or overall operation.\nWire-compatible with start-os's `Progress` (serialized as untagged).",
  })
export const NamedProgress = z
  .object({ name: z.string(), progress: Progress })
  .meta({
    id: 'NamedProgress',
  })

/**
 * Snapshot of overall + per-phase progress, sent over WebSocket.
 */
export const FullProgress = z
  .object({ overall: Progress, phases: z.array(NamedProgress) })
  .meta({
    id: 'FullProgress',
    description:
      'Snapshot of overall + per-phase progress, sent over WebSocket.',
  })
