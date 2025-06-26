;; Decentralized Community Kitchen Network Contract
;; Manages kitchen spaces, equipment scheduling, and user memberships

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-KITCHEN-NOT-FOUND (err u101))
(define-constant ERR-EQUIPMENT-NOT-FOUND (err u102))
(define-constant ERR-INVALID-TIME-SLOT (err u103))
(define-constant ERR-ALREADY-BOOKED (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-INVALID-RATING (err u106))
(define-constant ERR-NOT-MEMBER (err u107))
(define-constant ERR-INVALID-CAPACITY (err u108))

;; Data Variables
(define-data-var next-kitchen-id uint u1)
(define-data-var next-equipment-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var platform-fee-rate uint u25) ;; 2.5% in basis points

;; Kitchen Data Structure
(define-map kitchens
  { kitchen-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    location: (string-ascii 100),
    capacity: uint,
    hourly-rate: uint,
    deposit-required: uint,
    is-active: bool,
    rating-sum: uint,
    rating-count: uint,
    created-at: uint
  }
)

;; Equipment Data Structure
(define-map equipment
  { equipment-id: uint }
  {
    kitchen-id: uint,
    name: (string-ascii 30),
    equipment-type: (string-ascii 20),
    hourly-rate: uint,
    is-available: bool,
    maintenance-due: uint
  }
)

;; Booking Data Structure
(define-map bookings
  { booking-id: uint }
  {
    kitchen-id: uint,
    renter: principal,
    start-time: uint,
    end-time: uint,
    total-cost: uint,
    deposit-paid: uint,
    status: (string-ascii 10), ;; "pending", "active", "completed", "cancelled"
    equipment-ids: (list 10 uint),
    created-at: uint
  }
)

;; User membership and balances
(define-map user-balances { user: principal } { balance: uint })
(define-map user-memberships
  { user: principal }
  {
    member-since: uint,
    reputation-score: uint,
    total-bookings: uint,
    is-active: bool
  }
)

;; Time slot tracking for conflict prevention
(define-map time-slots
  { kitchen-id: uint, time-slot: uint }
  { is-booked: bool, booking-id: uint }
)

;; Kitchen management functions
(define-public (register-kitchen (name (string-ascii 50))
                                (location (string-ascii 100))
                                (capacity uint)
                                (hourly-rate uint)
                                (deposit-required uint))
  (let ((kitchen-id (var-get next-kitchen-id)))
    (asserts! (> capacity u0) ERR-INVALID-CAPACITY)
    (asserts! (> hourly-rate u0) ERR-INVALID-CAPACITY)

    (map-set kitchens
      { kitchen-id: kitchen-id }
      {
        owner: tx-sender,
        name: name,
        location: location,
        capacity: capacity,
        hourly-rate: hourly-rate,
        deposit-required: deposit-required,
        is-active: true,
        rating-sum: u0,
        rating-count: u0,
        created-at: stacks-block-height
      }
    )

    (var-set next-kitchen-id (+ kitchen-id u1))
    (ok kitchen-id)
  )
)

(define-public (add-equipment (kitchen-id uint)
                             (name (string-ascii 30))
                             (equipment-type (string-ascii 20))
                             (hourly-rate uint))
  (let ((equipment-id (var-get next-equipment-id))
        (kitchen-info (unwrap! (map-get? kitchens { kitchen-id: kitchen-id }) ERR-KITCHEN-NOT-FOUND)))

    (asserts! (is-eq (get owner kitchen-info) tx-sender) ERR-NOT-AUTHORIZED)

    (map-set equipment
      { equipment-id: equipment-id }
      {
        kitchen-id: kitchen-id,
        name: name,
        equipment-type: equipment-type,
        hourly-rate: hourly-rate,
        is-available: true,
        maintenance-due: (+ stacks-block-height u4320) ;; ~30 days
      }
    )

    (var-set next-equipment-id (+ equipment-id u1))
    (ok equipment-id)
  )
)

;; Booking functions
(define-public (create-booking (kitchen-id uint)
                              (start-time uint)
                              (end-time uint)
                              (equipment-ids (list 10 uint)))
  (let ((kitchen-info (unwrap! (map-get? kitchens { kitchen-id: kitchen-id }) ERR-KITCHEN-NOT-FOUND))
        (booking-id (var-get next-booking-id))
        (duration (- end-time start-time))
        (kitchen-cost (* (get hourly-rate kitchen-info) duration))
        (equipment-cost (calculate-equipment-cost equipment-ids duration))
        (total-cost (+ kitchen-cost equipment-cost))
        (deposit (get deposit-required kitchen-info))
        (user-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))

    (asserts! (get is-active kitchen-info) ERR-KITCHEN-NOT-FOUND)
    (asserts! (> end-time start-time) ERR-INVALID-TIME-SLOT)
    (asserts! (> end-time stacks-block-height) ERR-INVALID-TIME-SLOT)
    (asserts! (>= user-balance (+ total-cost deposit)) ERR-INSUFFICIENT-BALANCE)
    (asserts! (is-time-slot-available kitchen-id start-time end-time) ERR-ALREADY-BOOKED)

    ;; Reserve time slot
    (reserve-time-slots kitchen-id start-time end-time booking-id)

    ;; Deduct payment from user balance
    (map-set user-balances
      { user: tx-sender }
      { balance: (- user-balance (+ total-cost deposit)) }
    )

    ;; Create booking
    (map-set bookings
      { booking-id: booking-id }
      {
        kitchen-id: kitchen-id,
        renter: tx-sender,
        start-time: start-time,
        end-time: end-time,
        total-cost: total-cost,
        deposit-paid: deposit,
        status: "pending",
        equipment-ids: equipment-ids,
        created-at: stacks-block-height
      }
    )

    (var-set next-booking-id (+ booking-id u1))
    (ok booking-id)
  )
)

;; User balance management
(define-public (deposit-funds (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-balances
      { user: tx-sender }
      { balance: (+ current-balance amount) }
    )
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
    (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)

    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set user-balances
      { user: tx-sender }
      { balance: (- current-balance amount) }
    )
    (ok true)
  )
)

;; Membership management
(define-public (register-membership)
  (let ((existing-member (map-get? user-memberships { user: tx-sender })))
    (asserts! (is-none existing-member) ERR-NOT-AUTHORIZED)

    (map-set user-memberships
      { user: tx-sender }
      {
        member-since: stacks-block-height,
        reputation-score: u100, ;; Starting score
        total-bookings: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Rating system
(define-public (rate-kitchen (kitchen-id uint) (rating uint))
  (let ((kitchen-info (unwrap! (map-get? kitchens { kitchen-id: kitchen-id }) ERR-KITCHEN-NOT-FOUND)))
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)

    (map-set kitchens
      { kitchen-id: kitchen-id }
      (merge kitchen-info {
        rating-sum: (+ (get rating-sum kitchen-info) rating),
        rating-count: (+ (get rating-count kitchen-info) u1)
      })
    )
    (ok true)
  )
)

;; Helper functions
(define-private (calculate-equipment-cost (equipment-ids (list 10 uint)) (duration uint))
  (fold + (map get-equipment-hourly-rate equipment-ids) u0)
)

(define-private (get-equipment-hourly-rate (equipment-id uint))
  (match (map-get? equipment { equipment-id: equipment-id })
    equipment-info (get hourly-rate equipment-info)
    u0
  )
)

(define-private (is-time-slot-available (kitchen-id uint) (start-time uint) (end-time uint))
  (is-none (map-get? time-slots { kitchen-id: kitchen-id, time-slot: start-time }))
)

(define-private (is-slot-booked (time-slot uint))
  (default-to false (get is-booked (map-get? time-slots { kitchen-id: u1, time-slot: time-slot })))
)

(define-private (generate-time-slots (start-time uint) (end-time uint))
  ;; Simplified - in production would generate hourly slots
  (list start-time)
)

(define-private (reserve-time-slots (kitchen-id uint) (start-time uint) (end-time uint) (booking-id uint))
  (map-set time-slots
    { kitchen-id: kitchen-id, time-slot: start-time }
    { is-booked: true, booking-id: booking-id }
  )
)

;; Read-only functions
(define-read-only (get-kitchen (kitchen-id uint))
  (map-get? kitchens { kitchen-id: kitchen-id })
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

(define-read-only (get-user-membership (user principal))
  (map-get? user-memberships { user: user })
)

(define-read-only (get-kitchen-rating (kitchen-id uint))
  (match (map-get? kitchens { kitchen-id: kitchen-id })
    kitchen-info
      (if (> (get rating-count kitchen-info) u0)
        (some (/ (get rating-sum kitchen-info) (get rating-count kitchen-info)))
        none
      )
    none
  )
)

(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment { equipment-id: equipment-id })
)
