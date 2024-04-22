declare const HealthProof: unique symbol
export type HealthReceipt = {
  [HealthProof]: never
}
