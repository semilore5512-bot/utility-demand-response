;; Utility Demand Response Contract
;; Grid management platform with load reduction requests, participation tracking, incentive payments, and performance monitoring

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_EVENT_NOT_FOUND (err u102))
(define-constant ERR_EVENT_EXPIRED (err u103))
(define-constant ERR_ALREADY_PARTICIPATED (err u104))
(define-constant ERR_NOT_PARTICIPATING (err u105))
(define-constant ERR_INSUFFICIENT_BALANCE (err u106))

;; Data Variables
(define-data-var event-counter uint u0)
(define-data-var total-incentive-pool uint u0)

;; Data Maps
(define-map demand-events
  uint
  {
    start-time: uint,
    end-time: uint, 
    target-reduction: uint,
    incentive-rate: uint,
    total-participation: uint,
    total-reduction: uint,
    status: (string-ascii 20)
  }
)

(define-map participant-records
  { event-id: uint, participant: principal }
  {
    baseline-usage: uint,
    actual-usage: uint,
    reduction-achieved: uint,
    incentive-earned: uint,
    participation-time: uint
  }
)

(define-map customer-profiles
  principal
  {
    total-events-participated: uint,
    total-reduction-achieved: uint,
    total-incentives-earned: uint,
    performance-score: uint
  }
)

(define-map incentive-balances principal uint)

;; Public Functions

;; Create a new demand response event
(define-public (create-demand-event (start-time uint) (end-time uint) (target-reduction uint) (incentive-rate uint))
  (let ((event-id (+ (var-get event-counter) u1)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> end-time start-time) ERR_INVALID_AMOUNT)
    (asserts! (> target-reduction u0) ERR_INVALID_AMOUNT)
    (asserts! (> incentive-rate u0) ERR_INVALID_AMOUNT)
    
    (map-set demand-events event-id {
      start-time: start-time,
      end-time: end-time,
      target-reduction: target-reduction,
      incentive-rate: incentive-rate,
      total-participation: u0,
      total-reduction: u0,
      status: "active"
    })
    
    (var-set event-counter event-id)
    (ok event-id)
  )
)

;; Register customer participation in demand response event
(define-public (register-participation (event-id uint) (baseline-usage uint))
  (let ((event-data (unwrap! (map-get? demand-events event-id) ERR_EVENT_NOT_FOUND))
        (current-time burn-block-height))
    
    (asserts! (>= current-time (get start-time event-data)) ERR_EVENT_EXPIRED)
    (asserts! (<= current-time (get end-time event-data)) ERR_EVENT_EXPIRED)
    (asserts! (> baseline-usage u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? participant-records { event-id: event-id, participant: tx-sender })) ERR_ALREADY_PARTICIPATED)
    
    (map-set participant-records { event-id: event-id, participant: tx-sender } {
      baseline-usage: baseline-usage,
      actual-usage: u0,
      reduction-achieved: u0,
      incentive-earned: u0,
      participation-time: current-time
    })
    
    ;; Update event participation count
    (map-set demand-events event-id (merge event-data {
      total-participation: (+ (get total-participation event-data) u1)
    }))
    
    (ok true)
  )
)

;; Submit actual usage for incentive calculation
(define-public (submit-usage (event-id uint) (actual-usage uint))
  (let ((event-data (unwrap! (map-get? demand-events event-id) ERR_EVENT_NOT_FOUND))
        (participation-data (unwrap! (map-get? participant-records { event-id: event-id, participant: tx-sender }) ERR_NOT_PARTICIPATING))
        (baseline (get baseline-usage participation-data))
        (reduction (if (> baseline actual-usage) (- baseline actual-usage) u0))
        (incentive (/ (* reduction (get incentive-rate event-data)) u100))
        (current-time burn-block-height))
    
    (asserts! (> current-time (get end-time event-data)) ERR_EVENT_EXPIRED)
    (asserts! (is-eq (get status event-data) "active") ERR_EVENT_EXPIRED)
    
    ;; Update participant record
    (map-set participant-records { event-id: event-id, participant: tx-sender } (merge participation-data {
      actual-usage: actual-usage,
      reduction-achieved: reduction,
      incentive-earned: incentive
    }))
    
    ;; Update event totals
    (map-set demand-events event-id (merge event-data {
      total-reduction: (+ (get total-reduction event-data) reduction)
    }))
    
    ;; Update customer profile
    (let ((profile (default-to { total-events-participated: u0, total-reduction-achieved: u0, total-incentives-earned: u0, performance-score: u0 }
                                 (map-get? customer-profiles tx-sender))))
      (map-set customer-profiles tx-sender {
        total-events-participated: (+ (get total-events-participated profile) u1),
        total-reduction-achieved: (+ (get total-reduction-achieved profile) reduction),
        total-incentives-earned: (+ (get total-incentives-earned profile) incentive),
        performance-score: (calculate-performance-score (+ (get total-reduction-achieved profile) reduction) (+ (get total-events-participated profile) u1))
      })
    )
    
    ;; Update incentive balance
    (let ((current-balance (default-to u0 (map-get? incentive-balances tx-sender))))
      (map-set incentive-balances tx-sender (+ current-balance incentive))
    )
    
    (ok incentive)
  )
)

;; Claim earned incentives
(define-public (claim-incentives)
  (let ((balance (default-to u0 (map-get? incentive-balances tx-sender))))
    (asserts! (> balance u0) ERR_INSUFFICIENT_BALANCE)
    
    (map-set incentive-balances tx-sender u0)
    (var-set total-incentive-pool (- (var-get total-incentive-pool) balance))
    
    ;; In a real implementation, this would transfer tokens
    (ok balance)
  )
)

;; Close demand response event
(define-public (close-event (event-id uint))
  (let ((event-data (unwrap! (map-get? demand-events event-id) ERR_EVENT_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> burn-block-height (get end-time event-data)) ERR_EVENT_EXPIRED)
    
    (map-set demand-events event-id (merge event-data {
      status: "closed"
    }))
    
    (ok true)
  )
)

;; Fund incentive pool
(define-public (fund-incentive-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (var-set total-incentive-pool (+ (var-get total-incentive-pool) amount))
    (ok true)
  )
)

;; Read-Only Functions

;; Get demand response event details
(define-read-only (get-demand-event (event-id uint))
  (map-get? demand-events event-id)
)

;; Get participant record for specific event
(define-read-only (get-participation-record (event-id uint) (participant principal))
  (map-get? participant-records { event-id: event-id, participant: participant })
)

;; Get customer profile
(define-read-only (get-customer-profile (customer principal))
  (map-get? customer-profiles customer)
)

;; Get incentive balance
(define-read-only (get-incentive-balance (customer principal))
  (default-to u0 (map-get? incentive-balances customer))
)

;; Get current event counter
(define-read-only (get-event-counter)
  (var-get event-counter)
)

;; Get total incentive pool
(define-read-only (get-total-incentive-pool)
  (var-get total-incentive-pool)
)

;; Get event performance metrics
(define-read-only (get-event-performance (event-id uint))
  (match (map-get? demand-events event-id)
    event-data (some {
      event-id: event-id,
      target-reduction: (get target-reduction event-data),
      actual-reduction: (get total-reduction event-data),
      participation-count: (get total-participation event-data),
      success-rate: (if (> (get target-reduction event-data) u0) 
                       (/ (* (get total-reduction event-data) u100) (get target-reduction event-data))
                       u0),
      status: (get status event-data)
    })
    none
  )
)

;; Private Functions

;; Calculate customer performance score based on participation history
(define-private (calculate-performance-score (total-reduction uint) (total-events uint))
  (if (> total-events u0)
    (/ (* total-reduction u100) total-events)
    u0
  )
)
