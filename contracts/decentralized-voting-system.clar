;; Decentralized Voting System Smart Contract
;; This contract enables secure, transparent voting on proposals

;; Contract owner
(define-constant contract-owner tx-sender)

;; Error codes
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-voting-closed (err u104))
(define-constant err-voting-active (err u105))
(define-constant err-invalid-option (err u106))
(define-constant err-already-registered (err u107))
(define-constant err-not-registered (err u108))

;; Data structures
(define-map voters
  { voter: principal }
  { registered: bool, registration-block: uint }
)

(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    start-block: uint,
    end-block: uint,
    options: (list 10 (string-ascii 50)),
    total-votes: uint,
    finalized: bool
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { option-index: uint, vote-block: uint }
)

(define-map vote-counts
  { proposal-id: uint, option-index: uint }
  { count: uint }
)

;; Contract state
(define-data-var proposal-counter uint u0)
(define-data-var registration-enabled bool true)

;; Helper functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

(define-read-only (is-voter-registered (voter principal))
  (default-to false (get registered (map-get? voters { voter: voter })))
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: voter }))
)

(define-read-only (is-voting-active (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal
    (and 
      (>= stacks-block-height (get start-block proposal))
      (<= stacks-block-height (get end-block proposal))
      (not (get finalized proposal))
    )
    false
  )
)

;; Public functions

;; Voter registration
(define-public (register-voter)
  (begin
    (asserts! (var-get registration-enabled) err-unauthorized)
    (asserts! (not (is-voter-registered tx-sender)) err-already-registered)
    (ok (map-set voters
      { voter: tx-sender }
      { registered: true, registration-block: stacks-block-height }
    ))
  )
)

;; Create a new proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (voting-duration uint)
  (options (list 10 (string-ascii 50)))
)
  (let
    (
      (new-id (+ (var-get proposal-counter) u1))
      (start-block (+ stacks-block-height u1))
      (end-block (+ start-block voting-duration))
    )
    (asserts! (is-voter-registered tx-sender) err-not-registered)
    (asserts! (> (len options) u1) err-invalid-option)
    
    ;; Create the proposal
    (map-set proposals
      { proposal-id: new-id }
      {
        title: title,
        description: description,
        creator: tx-sender,
        start-block: start-block,
        end-block: end-block,
        options: options,
        total-votes: u0,
        finalized: false
      }
    )
    
    ;; Initialize vote counts for each option
    (initialize-all-vote-counts new-id options)
    
    ;; Update proposal counter
    (var-set proposal-counter new-id)
    (ok new-id)
  )
)

;; Helper function to initialize vote counts for all options
(define-private (initialize-all-vote-counts (proposal-id uint) (options (list 10 (string-ascii 50))))
  (begin
    (initialize-vote-count-at-index proposal-id u0)
    (initialize-vote-count-at-index proposal-id u1)
    (initialize-vote-count-at-index proposal-id u2)
    (initialize-vote-count-at-index proposal-id u3)
    (initialize-vote-count-at-index proposal-id u4)
    (initialize-vote-count-at-index proposal-id u5)
    (initialize-vote-count-at-index proposal-id u6)
    (initialize-vote-count-at-index proposal-id u7)
    (initialize-vote-count-at-index proposal-id u8)
    (initialize-vote-count-at-index proposal-id u9)
  )
)

(define-private (initialize-vote-count-at-index (proposal-id uint) (option-index uint))
  (map-set vote-counts
    { proposal-id: proposal-id, option-index: option-index }
    { count: u0 }
  )
)

;; Cast a vote
(define-public (cast-vote (proposal-id uint) (option-index uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
    )
    (asserts! (is-voter-registered tx-sender) err-not-registered)
    (asserts! (is-voting-active proposal-id) err-voting-closed)
    (asserts! (not (has-voted proposal-id tx-sender)) err-already-voted)
    (asserts! (< option-index (len (get options proposal))) err-invalid-option)
    
    ;; Record the vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { option-index: option-index, vote-block: stacks-block-height }
    )
    
    ;; Update vote count for the selected option
    (map-set vote-counts
      { proposal-id: proposal-id, option-index: option-index }
      { count: (+ (get-vote-count proposal-id option-index) u1) }
    )
    
    ;; Update total votes for the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { total-votes: (+ (get total-votes proposal) u1) })
    )
    
    (ok true)
  )
)

;; Get vote count for a specific option
(define-read-only (get-vote-count (proposal-id uint) (option-index uint))
  (default-to u0 (get count (map-get? vote-counts { proposal-id: proposal-id, option-index: option-index })))
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get voter information
(define-read-only (get-voter-info (voter principal))
  (map-get? voters { voter: voter })
)

;; Get vote details for a specific voter and proposal
(define-read-only (get-vote-details (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

;; Get all vote counts for a proposal
(define-read-only (get-proposal-results (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal
    (some {
      proposal-id: proposal-id,
      total-votes: (get total-votes proposal),
      option-0-votes: (get-vote-count proposal-id u0),
      option-1-votes: (get-vote-count proposal-id u1),
      option-2-votes: (get-vote-count proposal-id u2),
      option-3-votes: (get-vote-count proposal-id u3),
      option-4-votes: (get-vote-count proposal-id u4),
      option-5-votes: (get-vote-count proposal-id u5),
      option-6-votes: (get-vote-count proposal-id u6),
      option-7-votes: (get-vote-count proposal-id u7),
      option-8-votes: (get-vote-count proposal-id u8),
      option-9-votes: (get-vote-count proposal-id u9),
      finalized: (get finalized proposal)
    })
    none
  )
)

;; Get vote count for a specific option (simpler version)
(define-read-only (get-option-votes (proposal-id uint) (option-index uint))
  (get-vote-count proposal-id option-index)
)

;; Finalize a proposal (can only be done after voting period ends)
(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
    )
    (asserts! (is-contract-owner) err-owner-only)
    (asserts! (> stacks-block-height (get end-block proposal)) err-voting-active)
    (asserts! (not (get finalized proposal)) err-voting-closed)
    
    (ok (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { finalized: true })
    ))
  )
)

;; Admin functions

;; Toggle voter registration
(define-public (toggle-registration)
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (ok (var-set registration-enabled (not (var-get registration-enabled))))
  )
)

;; Emergency function to close a proposal
(define-public (emergency-close-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
    )
    (asserts! (is-contract-owner) err-owner-only)
    
    (ok (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { 
        end-block: stacks-block-height,
        finalized: true 
      })
    ))
  )
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-proposals: (var-get proposal-counter),
    registration-enabled: (var-get registration-enabled),
    contract-owner: contract-owner,
    current-block: stacks-block-height
  }
)