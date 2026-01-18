;; EcoCanyon Care - Environmental Restoration Platform
;; A blockchain-powered platform for verified canyon ecosystem rehabilitation

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-milestone-not-complete (err u104))
(define-constant err-already-verified (err u105))
(define-constant err-insufficient-funds (err u106))

;; Data Variables
(define-data-var project-nonce uint u0)
(define-data-var total-carbon-offset uint u0)

;; Data Maps
(define-map projects
  uint
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    target-amount: uint,
    raised-amount: uint,
    disbursed-amount: uint,
    creator: principal,
    active: bool,
    carbon-credits: uint
  }
)

(define-map milestones
  {project-id: uint, milestone-id: uint}
  {
    description: (string-ascii 200),
    target-value: uint,
    current-value: uint,
    funding-amount: uint,
    verified: bool,
    completed: bool
  }
)

(define-map project-milestones
  uint
  {milestone-count: uint}
)

(define-map donations
  {donor: principal, project-id: uint}
  {amount: uint}
)

(define-map community-validators
  principal
  {reputation: uint, validations: uint}
)

(define-map governance-tokens
  principal
  uint
)

;; Governance token rewards per validation
(define-constant tokens-per-validation u10)

;; Read-only functions
(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? milestones {project-id: project-id, milestone-id: milestone-id})
)

(define-read-only (get-donation (donor principal) (project-id uint))
  (map-get? donations {donor: donor, project-id: project-id})
)

(define-read-only (get-validator-info (validator principal))
  (map-get? community-validators validator)
)

(define-read-only (get-governance-tokens (holder principal))
  (default-to u0 (map-get? governance-tokens holder))
)

(define-read-only (get-total-carbon-offset)
  (ok (var-get total-carbon-offset))
)

;; Public functions

;; Create a new restoration project
(define-public (create-project (name (string-ascii 100)) (location (string-ascii 100)) (target-amount uint))
  (let
    (
      (project-id (+ (var-get project-nonce) u1))
    )
    (asserts! (> target-amount u0) err-invalid-amount)
    (map-set projects project-id
      {
        name: name,
        location: location,
        target-amount: target-amount,
        raised-amount: u0,
        disbursed-amount: u0,
        creator: tx-sender,
        active: true,
        carbon-credits: u0
      }
    )
    (map-set project-milestones project-id {milestone-count: u0})
    (var-set project-nonce project-id)
    (ok project-id)
  )
)

;; Add milestone to a project
(define-public (add-milestone 
    (project-id uint) 
    (description (string-ascii 200)) 
    (target-value uint) 
    (funding-amount uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) err-not-found))
      (milestone-data (default-to {milestone-count: u0} (map-get? project-milestones project-id)))
      (milestone-id (+ (get milestone-count milestone-data) u1))
    )
    (asserts! (is-eq (get creator project) tx-sender) err-unauthorized)
    (map-set milestones {project-id: project-id, milestone-id: milestone-id}
      {
        description: description,
        target-value: target-value,
        current-value: u0,
        funding-amount: funding-amount,
        verified: false,
        completed: false
      }
    )
    (map-set project-milestones project-id {milestone-count: milestone-id})
    (ok milestone-id)
  )
)

;; Donate to a project
(define-public (donate (project-id uint) (amount uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) err-not-found))
      (current-donation (default-to {amount: u0} (map-get? donations {donor: tx-sender, project-id: project-id})))
    )
    (asserts! (get active project) err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set projects project-id
      (merge project {raised-amount: (+ (get raised-amount project) amount)})
    )
    (map-set donations {donor: tx-sender, project-id: project-id}
      {amount: (+ (get amount current-donation) amount)}
    )
    (ok true)
  )
)

;; Community validator verification of milestone progress
(define-public (verify-milestone-progress (project-id uint) (milestone-id uint) (measured-value uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) err-not-found))
      (validator-info (default-to {reputation: u0, validations: u0} (map-get? community-validators tx-sender)))
      (current-tokens (get-governance-tokens tx-sender))
    )
    (asserts! (not (get verified milestone)) err-already-verified)
    (map-set milestones {project-id: project-id, milestone-id: milestone-id}
      (merge milestone {
        current-value: measured-value,
        verified: true,
        completed: (>= measured-value (get target-value milestone))
      })
    )
    ;; Reward validator with governance tokens
    (map-set community-validators tx-sender
      {
        reputation: (+ (get reputation validator-info) u1),
        validations: (+ (get validations validator-info) u1)
      }
    )
    (map-set governance-tokens tx-sender (+ current-tokens tokens-per-validation))
    (ok true)
  )
)

;; Release funds when milestone is completed
(define-public (release-milestone-funds (project-id uint) (milestone-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) err-not-found))
      (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) err-not-found))
      (funding-amount (get funding-amount milestone))
    )
    (asserts! (get completed milestone) err-milestone-not-complete)
    (asserts! (get verified milestone) err-unauthorized)
    (asserts! (<= (+ (get disbursed-amount project) funding-amount) (get raised-amount project)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? funding-amount tx-sender (get creator project))))
    (map-set projects project-id
      (merge project {disbursed-amount: (+ (get disbursed-amount project) funding-amount)})
    )
    (ok true)
  )
)

;; Record carbon credits earned from restoration
(define-public (record-carbon-credits (project-id uint) (credits uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) err-not-found))
    )
    (asserts! (is-eq (get creator project) tx-sender) err-unauthorized)
    (map-set projects project-id
      (merge project {carbon-credits: (+ (get carbon-credits project) credits)})
    )
    (var-set total-carbon-offset (+ (var-get total-carbon-offset) credits))
    (ok true)
  )
)

;; Deactivate a project
(define-public (deactivate-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) err-not-found))
    )
    (asserts! (is-eq (get creator project) tx-sender) err-unauthorized)
    (map-set projects project-id (merge project {active: false}))
    (ok true)
  )
)