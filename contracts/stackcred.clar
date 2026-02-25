;; Define admin variable
(define-data-var admin principal tx-sender)

;; Define categories list
(define-data-var categories (list 20 (string-ascii 32)) (list))

;; Define user scores map
(define-map user-scores
  {user: principal, category: (string-ascii 32)}
  {score: int}
)

;; Define whitelist as a map (since Clarity doesn't have a native set type)
(define-map whitelist principal bool)

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_CATEGORY_EXISTS (err u101))
(define-constant ERR_NOT_WHITELISTED (err u102))
(define-constant ERR_INVALID_CATEGORY (err u103))

;; ---- Helper Functions ----

(define-private (is-whitelisted (caller principal))
  (default-to false (map-get? whitelist caller))
)

(define-private (category-exists? (cat (string-ascii 32)))
  (is-some (index-of (var-get categories) cat))
)

;; ---- Admin-only: Add new scoring category ----

(define-public (add-category (cat (string-ascii 32)))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (asserts! (not (category-exists? cat)) ERR_CATEGORY_EXISTS)
    (var-set categories (unwrap! (as-max-len? (append (var-get categories) cat) u20) ERR_CATEGORY_EXISTS))
    (ok true)
  )
)

;; ---- Admin-only: Manage whitelist ----

(define-public (add-to-whitelist (who principal))
  (if (is-eq tx-sender (var-get admin))
    (begin
      (map-set whitelist who true)
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (remove-from-whitelist (who principal))
  (if (is-eq tx-sender (var-get admin))
    (begin
      (map-delete whitelist who)
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

;; ---- Whitelisted: Assign/Update score ----

(define-public (assign-score (user principal) (cat (string-ascii 32)) (amount int))
  (if (is-whitelisted tx-sender)
    (if (category-exists? cat)
      (begin
        (map-set user-scores {user: user, category: cat} {score: amount})
        (ok true)
      )
      ERR_INVALID_CATEGORY
    )
    ERR_NOT_WHITELISTED
  )
)

;; ---- Public: Read user's score ----

(define-read-only (get-score (user principal) (cat (string-ascii 32)))
  (default-to 0
    (match (map-get? user-scores {user: user, category: cat})
      score-data (some (get score score-data))
      none
    )
  )
)

;; Helper function for calculating individual category scores
(define-private (get-category-score (user principal) (cat (string-ascii 32)))
  (match (map-get? user-scores {user: user, category: cat})
    score-data (get score score-data)
    0
  )
)

;; Public: Get total score across all categories
(define-read-only (get-total-score (user principal))
  (let ((cats (var-get categories)))
    (+ 
      (+ 
        (+ 
          (if (>= (len cats) u1) (get-category-score user (unwrap-panic (element-at cats u0))) 0)
          (if (>= (len cats) u2) (get-category-score user (unwrap-panic (element-at cats u1))) 0)
        )
        (+ 
          (if (>= (len cats) u3) (get-category-score user (unwrap-panic (element-at cats u2))) 0)
          (if (>= (len cats) u4) (get-category-score user (unwrap-panic (element-at cats u3))) 0)
        )
      )
      (+
        (+
          (if (>= (len cats) u5) (get-category-score user (unwrap-panic (element-at cats u4))) 0)
          (if (>= (len cats) u6) (get-category-score user (unwrap-panic (element-at cats u5))) 0)
        )
        (+
          (if (>= (len cats) u7) (get-category-score user (unwrap-panic (element-at cats u6))) 0)
          (if (>= (len cats) u8) (get-category-score user (unwrap-panic (element-at cats u7))) 0)
        )
      )
      (+
        (+
          (if (>= (len cats) u9) (get-category-score user (unwrap-panic (element-at cats u8))) 0)
          (if (>= (len cats) u10) (get-category-score user (unwrap-panic (element-at cats u9))) 0)
        )
        (+
          (if (>= (len cats) u11) (get-category-score user (unwrap-panic (element-at cats u10))) 0)
          (if (>= (len cats) u12) (get-category-score user (unwrap-panic (element-at cats u11))) 0)
        )
      )
      (+
        (+
          (if (>= (len cats) u13) (get-category-score user (unwrap-panic (element-at cats u12))) 0)
          (if (>= (len cats) u14) (get-category-score user (unwrap-panic (element-at cats u13))) 0)
        )
        (+
          (if (>= (len cats) u15) (get-category-score user (unwrap-panic (element-at cats u14))) 0)
          (if (>= (len cats) u16) (get-category-score user (unwrap-panic (element-at cats u15))) 0)
        )
      )
      (+
        (+
          (if (>= (len cats) u17) (get-category-score user (unwrap-panic (element-at cats u16))) 0)
          (if (>= (len cats) u18) (get-category-score user (unwrap-panic (element-at cats u17))) 0)
        )
        (+
          (if (>= (len cats) u19) (get-category-score user (unwrap-panic (element-at cats u18))) 0)
          (if (>= (len cats) u20) (get-category-score user (unwrap-panic (element-at cats u19))) 0)
        )
      )
    )
  )
)

;; ---- Additional helper functions ----

(define-read-only (get-categories)
  (ok (var-get categories))
)

(define-read-only (get-admin)
  (ok (var-get admin))
)

(define-read-only (is-user-whitelisted (user principal))
  (ok (is-whitelisted user))
)

;; ---- Admin: Change admin ----

(define-public (change-admin (new-admin principal))
  (if (is-eq tx-sender (var-get admin))
    (begin
      (var-set admin new-admin)
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

;; ---- Additional utility functions ----

;; Check if a category exists (public version)
(define-read-only (check-category-exists (cat (string-ascii 32)))
  (ok (category-exists? cat))
)

;; Get number of categories
(define-read-only (get-category-count)
  (ok (len (var-get categories)))
)
