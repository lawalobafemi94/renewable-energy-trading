;; Energy Trading Contract
;; Facilitate energy production registration, consumption tracking, and automated trading between peers

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u6001))
(define-constant ERR-PRODUCER-NOT-FOUND (err u6002))
(define-constant ERR-INSUFFICIENT-ENERGY (err u6003))
(define-constant ERR-INVALID-AMOUNT (err u6004))
(define-constant ERR-TRADE-NOT-FOUND (err u6005))
(define-constant ERR-ALREADY-EXISTS (err u6006))

;; Energy Types
(define-constant ENERGY-SOLAR u1)
(define-constant ENERGY-WIND u2)
(define-constant ENERGY-HYDRO u3)
(define-constant ENERGY-BIOMASS u4)

;; Data Variables
(define-data-var next-producer-id uint u1)
(define-data-var next-trade-id uint u1)
(define-data-var platform-fee-rate uint u200) ;; 2% fee
(define-data-var total-energy-traded uint u0)

;; Energy Producers
(define-map energy-producers
    { producer-id: uint }
    {
        owner: principal,
        name: (string-ascii 256),
        energy-type: uint,
        capacity: uint,
        location: (string-ascii 256),
        certification: (string-ascii 128),
        is-active: bool,
        total-produced: uint,
        total-sold: uint
    }
)

;; Energy Inventory
(define-map energy-inventory
    { producer-id: uint }
    {
        available-energy: uint,
        price-per-unit: uint,
        last-updated: uint
    }
)

;; Energy Trades
(define-map energy-trades
    { trade-id: uint }
    {
        producer-id: uint,
        buyer: principal,
        energy-amount: uint,
        price-per-unit: uint,
        total-cost: uint,
        trade-date: uint,
        is-settled: bool
    }
)

;; User Energy Balance
(define-map user-energy-balance
    { user: principal }
    {
        total-purchased: uint,
        total-consumed: uint,
        carbon-offset: uint
    }
)

;; Public Functions

;; Register as energy producer
(define-public (register-producer (name (string-ascii 256)) (energy-type uint) (capacity uint) (location (string-ascii 256)) (certification (string-ascii 128)))
    (let
        (
            (producer-id (var-get next-producer-id))
        )
        (asserts! (<= energy-type u4) ERR-INVALID-AMOUNT)
        (asserts! (> capacity u0) ERR-INVALID-AMOUNT)
        (asserts! (> (len name) u0) ERR-INVALID-AMOUNT)
        
        ;; Create producer record
        (map-set energy-producers
            { producer-id: producer-id }
            {
                owner: tx-sender,
                name: name,
                energy-type: energy-type,
                capacity: capacity,
                location: location,
                certification: certification,
                is-active: true,
                total-produced: u0,
                total-sold: u0
            }
        )
        
        ;; Initialize inventory
        (map-set energy-inventory
            { producer-id: producer-id }
            {
                available-energy: u0,
                price-per-unit: u0,
                last-updated: block-height
            }
        )
        
        ;; Increment counter
        (var-set next-producer-id (+ producer-id u1))
        
        (ok producer-id)
    )
)

;; Add energy to inventory
(define-public (add-energy-production (producer-id uint) (energy-amount uint) (price-per-unit uint))
    (let
        (
            (producer-data (unwrap! (map-get? energy-producers { producer-id: producer-id }) ERR-PRODUCER-NOT-FOUND))
            (current-inventory (default-to
                { available-energy: u0, price-per-unit: u0, last-updated: u0 }
                (map-get? energy-inventory { producer-id: producer-id })))
        )
        (asserts! (is-eq tx-sender (get owner producer-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active producer-data) ERR-NOT-AUTHORIZED)
        (asserts! (> energy-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> price-per-unit u0) ERR-INVALID-AMOUNT)
        
        ;; Update inventory
        (map-set energy-inventory
            { producer-id: producer-id }
            {
                available-energy: (+ (get available-energy current-inventory) energy-amount),
                price-per-unit: price-per-unit,
                last-updated: block-height
            }
        )
        
        ;; Update producer stats
        (map-set energy-producers
            { producer-id: producer-id }
            (merge producer-data { total-produced: (+ (get total-produced producer-data) energy-amount) })
        )
        
        (ok true)
    )
)

;; Purchase energy
(define-public (purchase-energy (producer-id uint) (energy-amount uint))
    (let
        (
            (producer-data (unwrap! (map-get? energy-producers { producer-id: producer-id }) ERR-PRODUCER-NOT-FOUND))
            (inventory (unwrap! (map-get? energy-inventory { producer-id: producer-id }) ERR-PRODUCER-NOT-FOUND))
            (trade-id (var-get next-trade-id))
            (total-cost (* energy-amount (get price-per-unit inventory)))
            (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
            (producer-payment (- total-cost platform-fee))
            (user-balance (default-to
                { total-purchased: u0, total-consumed: u0, carbon-offset: u0 }
                (map-get? user-energy-balance { user: tx-sender })))
        )
        (asserts! (>= (get available-energy inventory) energy-amount) ERR-INSUFFICIENT-ENERGY)
        (asserts! (> energy-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (get is-active producer-data) ERR-NOT-AUTHORIZED)
        
        ;; Create trade record
        (map-set energy-trades
            { trade-id: trade-id }
            {
                producer-id: producer-id,
                buyer: tx-sender,
                energy-amount: energy-amount,
                price-per-unit: (get price-per-unit inventory),
                total-cost: total-cost,
                trade-date: block-height,
                is-settled: true
            }
        )
        
        ;; Update inventory
        (map-set energy-inventory
            { producer-id: producer-id }
            (merge inventory { available-energy: (- (get available-energy inventory) energy-amount) })
        )
        
        ;; Update producer stats
        (map-set energy-producers
            { producer-id: producer-id }
            (merge producer-data { total-sold: (+ (get total-sold producer-data) energy-amount) })
        )
        
        ;; Update user balance
        (map-set user-energy-balance
            { user: tx-sender }
            (merge user-balance { total-purchased: (+ (get total-purchased user-balance) energy-amount) })
        )
        
        ;; Update global stats
        (var-set total-energy-traded (+ (var-get total-energy-traded) energy-amount))
        (var-set next-trade-id (+ trade-id u1))
        
        (ok { trade-id: trade-id, cost: total-cost, fee: platform-fee })
    )
)

;; Record energy consumption
(define-public (record-energy-consumption (energy-amount uint))
    (let
        (
            (user-balance (default-to
                { total-purchased: u0, total-consumed: u0, carbon-offset: u0 }
                (map-get? user-energy-balance { user: tx-sender })))
        )
        (asserts! (> energy-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= (get total-purchased user-balance) (+ (get total-consumed user-balance) energy-amount)) ERR-INSUFFICIENT-ENERGY)
        
        ;; Update consumption
        (map-set user-energy-balance
            { user: tx-sender }
            (merge user-balance {
                total-consumed: (+ (get total-consumed user-balance) energy-amount),
                carbon-offset: (+ (get carbon-offset user-balance) energy-amount)
            })
        )
        
        (ok energy-amount)
    )
)

;; Update energy pricing
(define-public (update-energy-price (producer-id uint) (new-price uint))
    (let
        (
            (producer-data (unwrap! (map-get? energy-producers { producer-id: producer-id }) ERR-PRODUCER-NOT-FOUND))
            (inventory (unwrap! (map-get? energy-inventory { producer-id: producer-id }) ERR-PRODUCER-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get owner producer-data)) ERR-NOT-AUTHORIZED)
        (asserts! (> new-price u0) ERR-INVALID-AMOUNT)
        
        ;; Update pricing
        (map-set energy-inventory
            { producer-id: producer-id }
            (merge inventory {
                price-per-unit: new-price,
                last-updated: block-height
            })
        )
        
        (ok true)
    )
)

;; Read-only functions

;; Get producer information
(define-read-only (get-producer-info (producer-id uint))
    (map-get? energy-producers { producer-id: producer-id })
)

;; Get energy inventory
(define-read-only (get-energy-inventory (producer-id uint))
    (map-get? energy-inventory { producer-id: producer-id })
)

;; Get trade information
(define-read-only (get-trade-info (trade-id uint))
    (map-get? energy-trades { trade-id: trade-id })
)

;; Get user energy balance
(define-read-only (get-user-energy-balance (user principal))
    (map-get? user-energy-balance { user: user })
)

;; Get platform statistics
(define-read-only (get-platform-stats)
    {
        total-producers: (- (var-get next-producer-id) u1),
        total-trades: (- (var-get next-trade-id) u1),
        total-energy-traded: (var-get total-energy-traded),
        platform-fee-rate: (var-get platform-fee-rate)
    }
)

;; Calculate carbon offset
(define-read-only (calculate-carbon-offset (energy-amount uint) (energy-type uint))
    (let
        (
            ;; Carbon offset factors per unit (simplified)
            (offset-factor (if (is-eq energy-type ENERGY-SOLAR) u5
                          (if (is-eq energy-type ENERGY-WIND) u4
                          (if (is-eq energy-type ENERGY-HYDRO) u3
                          (if (is-eq energy-type ENERGY-BIOMASS) u2
                          u1)))))
        )
        (ok (* energy-amount offset-factor))
    )
)
