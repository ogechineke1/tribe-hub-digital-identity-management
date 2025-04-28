;; The Tribe Hub - Digital Identity Management Smart Contract
;; A comprehensive solution for managing digital identities in a decentralized community platform

;; =============================
;; STATE MANAGEMENT & STORAGE
;; =============================

;; Core identity profile storage
;; Stores all essential user information in the community hub
(define-map identity-records
  { identity-serial: uint }
  {
    display-name: (string-ascii 50),
    account-address: principal,
    onboarding-timestamp: uint,
    personal-description: (string-ascii 160),
    interest-tags: (list 5 (string-ascii 30))
  }
)

;; User engagement metrics tracker
;; Monitors platform usage patterns and engagement
(define-map user-engagement-metrics
  { identity-serial: uint }
  {
    previous-visit: uint,
    visit-counter: uint,
    recent-activity: (string-ascii 50)
  }
)

;; Digital identity access controls
;; Determines who can view whose profile information
(define-map access-permissions
  { identity-serial: uint, accessor-address: principal }
  { access-allowed: bool }
)

;; =============================
;; STATE VARIABLES
;; =============================

;; Total count of registered identities in the system
(define-data-var identity-counter uint u0)

;; =============================
;; SYSTEM CONSTANTS
;; =============================

;; Error condition identifiers
(define-constant ERROR-NOT-AUTHORIZED (err u500))
(define-constant ERROR-ENTITY-NOT-FOUND (err u501))
(define-constant ERROR-ALREADY-REGISTERED (err u502))
(define-constant ERROR-DATA-FORMAT-INVALID (err u503))
(define-constant ERROR-ACCESS-DENIED (err u504))

;; Administrative identifiers
(define-constant PLATFORM-ADMINISTRATOR tx-sender)

;; =============================
;; INTERNAL UTILITY FUNCTIONS
;; =============================

;; Verifies if an identity exists in the system
;; @param identity-serial - The unique identifier for the identity
;; @returns - Boolean indicating existence
(define-private (identity-registered? (identity-serial uint))
  (is-some (map-get? identity-records { identity-serial: identity-serial }))
)

;; Validates ownership of an identity record
;; @param identity-serial - The unique identifier for the identity
;; @param address - Principal address to check ownership against
;; @returns - Boolean indicating ownership status
(define-private (validate-identity-ownership? (identity-serial uint) (address principal))
  (match (map-get? identity-records { identity-serial: identity-serial })
    profile-data (is-eq (get account-address profile-data) address)
    false
  )
)

;; Validates format of a single interest tag
;; @param tag - Interest tag to validate
;; @returns - Boolean indicating validity
(define-private (validate-interest-tag? (tag (string-ascii 30)))
  (and
    (> (len tag) u0)
    (< (len tag) u31)
  )
)

;; Validates complete list of interest tags
;; @param tags - List of interest tags to validate
;; @returns - Boolean indicating validity of all tags
(define-private (validate-interest-tags-collection? (tags (list 5 (string-ascii 30))))
  (and
    (> (len tags) u0)
    (<= (len tags) u5)
    (is-eq (len (filter validate-interest-tag? tags)) (len tags))
  )
)

;; =============================
;; PUBLIC INTERFACE FUNCTIONS
;; =============================

;; Creates a new identity in the community hub
;; @param display-name - User's chosen display name
;; @param description - Personal bio or description
;; @param interests - List of user interests/tags
;; @returns - Response with new identity serial number or error
(define-public (create-identity 
    (display-name (string-ascii 50)) 
    (description (string-ascii 160)) 
    (interests (list 5 (string-ascii 30))))
  (let
    (
      (new-serial (+ (var-get identity-counter) u1))
    )
    ;; Data validation checks
    (asserts! (and (> (len display-name) u0) (< (len display-name) u51)) ERROR-DATA-FORMAT-INVALID)
    (asserts! (and (> (len description) u0) (< (len description) u161)) ERROR-DATA-FORMAT-INVALID)
    (asserts! (validate-interest-tags-collection? interests) ERROR-DATA-FORMAT-INVALID)

    ;; Store the identity record
    (map-insert identity-records
      { identity-serial: new-serial }
      {
        display-name: display-name,
        account-address: tx-sender,
        onboarding-timestamp: block-height,
        personal-description: description,
        interest-tags: interests
      }
    )

    ;; Initialize default access permission
    (map-insert access-permissions
      { identity-serial: new-serial, accessor-address: tx-sender }
      { access-allowed: true }
    )

    ;; Update identity counter
    (var-set identity-counter new-serial)
    (ok new-serial)
  )
)

;; Updates a user's interest tags
;; @param identity-serial - The identity to update
;; @param updated-interests - New list of interest tags
;; @returns - Success or error response
(define-public (modify-interest-tags (identity-serial uint) (updated-interests (list 5 (string-ascii 30))))
  (let
    (
      (profile-data (unwrap! (map-get? identity-records { identity-serial: identity-serial }) ERROR-ENTITY-NOT-FOUND))
    )
    ;; Validation checks
    (asserts! (identity-registered? identity-serial) ERROR-ENTITY-NOT-FOUND)
    (asserts! (is-eq (get account-address profile-data) tx-sender) ERROR-ACCESS-DENIED)
    (asserts! (validate-interest-tags-collection? updated-interests) ERROR-DATA-FORMAT-INVALID)

    ;; Update the interest tags
    (map-set identity-records
      { identity-serial: identity-serial }
      (merge profile-data { interest-tags: updated-interests })
    )
    (ok true)
  )
)

;; Alternative identity creation method
;; Provides the same functionality as create-identity for backward compatibility
;; @param display-name - User's chosen display name
;; @param description - Personal bio or description
;; @param interests - List of user interests/tags
;; @returns - Response with new identity serial number or error
(define-public (register-community-member 
    (display-name (string-ascii 50)) 
    (description (string-ascii 160)) 
    (interests (list 5 (string-ascii 30))))
  (let
    (
      (new-serial (+ (var-get identity-counter) u1))
    )
    ;; Data validation
    (asserts! (and (> (len display-name) u0) (< (len display-name) u51)) ERROR-DATA-FORMAT-INVALID)
    (asserts! (and (> (len description) u0) (< (len description) u161)) ERROR-DATA-FORMAT-INVALID)
    (asserts! (validate-interest-tags-collection? interests) ERROR-DATA-FORMAT-INVALID)

    ;; Store identity data
    (map-insert identity-records
      { identity-serial: new-serial }
      {
        display-name: display-name,
        account-address: tx-sender,
        onboarding-timestamp: block-height,
        personal-description: description,
        interest-tags: interests
      }
    )

    ;; Set self-viewing permission
    (map-insert access-permissions
      { identity-serial: new-serial, accessor-address: tx-sender }
      { access-allowed: true }
    )

    ;; Update counter
    (var-set identity-counter new-serial)
    (ok new-serial)
  )
)

;; Updates a user's display name
;; @param identity-serial - The identity to update
;; @param new-display-name - Updated display name
;; @returns - Success or error response
(define-public (update-display-name (identity-serial uint) (new-display-name (string-ascii 50)))
  (let
    (
      (profile-data (unwrap! (map-get? identity-records { identity-serial: identity-serial }) ERROR-ENTITY-NOT-FOUND))
    )
    ;; Validation checks
    (asserts! (identity-registered? identity-serial) ERROR-ENTITY-NOT-FOUND)
    (asserts! (is-eq (get account-address profile-data) tx-sender) ERROR-ACCESS-DENIED)

    ;; Update display name
    (map-set identity-records
      { identity-serial: identity-serial }
      (merge profile-data { display-name: new-display-name })
    )
    (ok true)
  )
)

;; =============================
;; ENHANCED FUNCTIONALITY
;; =============================

;; Optimized interests update function with simplified logic
;; @param identity-serial - The identity to update
;; @param updated-interests - New list of interest tags
;; @returns - Success or error response
(define-public (streamlined-interests-update (identity-serial uint) (updated-interests (list 5 (string-ascii 30))))
  (begin
    (asserts! (identity-registered? identity-serial) ERROR-ENTITY-NOT-FOUND)
    (asserts! (validate-interest-tags-collection? updated-interests) ERROR-DATA-FORMAT-INVALID)
    (map-set identity-records
      { identity-serial: identity-serial }
      (merge (unwrap! (map-get? identity-records { identity-serial: identity-serial }) ERROR-ENTITY-NOT-FOUND) 
             { interest-tags: updated-interests })
    )
    (ok "Interests successfully updated")
  )
)

;; Restricts profile access to specific users
;; @param identity-serial - The identity to check
;; @param address - Address requesting access
;; @returns - Success or error response
(define-public (restrict-profile-visibility (identity-serial uint) (address principal))
  (let
    (
      (profile-data (unwrap! (map-get? identity-records { identity-serial: identity-serial }) ERROR-ENTITY-NOT-FOUND))
    )
    ;; Check if address has permission to view profile
    (asserts! (is-eq (get account-address profile-data) address) ERROR-ACCESS-DENIED)
    (ok true)
  )
)

;; Comprehensive profile update with enhanced validation
;; @param identity-serial - The identity to update
;; @param new-display-name - Updated display name
;; @param new-description - Updated personal description
;; @param new-interests - Updated interest tags
;; @returns - Success or error response
(define-public (comprehensive-profile-update (identity-serial uint) 
                                           (new-display-name (string-ascii 50)) 
                                           (new-description (string-ascii 160)) 
                                           (new-interests (list 5 (string-ascii 30))))
  (let
    (
      (profile-data (unwrap! (map-get? identity-records { identity-serial: identity-serial }) ERROR-ENTITY-NOT-FOUND))
    )
    ;; Thorough validation
    (asserts! (identity-registered? identity-serial) ERROR-ENTITY-NOT-FOUND)
    (asserts! (is-eq (get account-address profile-data) tx-sender) ERROR-ACCESS-DENIED)
    (asserts! (> (len new-display-name) u0) ERROR-DATA-FORMAT-INVALID)
    (asserts! (< (len new-display-name) u51) ERROR-DATA-FORMAT-INVALID)
    (asserts! (validate-interest-tags-collection? new-interests) ERROR-DATA-FORMAT-INVALID)

    ;; Update all profile fields
    (map-set identity-records
      { identity-serial: identity-serial }
      (merge profile-data { 
        display-name: new-display-name, 
        personal-description: new-description, 
        interest-tags: new-interests 
      })
    )
    (ok true)
  )
)

;; Verifies ownership of an identity profile
;; @param identity-serial - The identity to check
;; @param claimed-owner - Address claiming ownership
;; @returns - Boolean response indicating ownership status
(define-public (confirm-identity-ownership (identity-serial uint) (claimed-owner principal))
  (let
    (
      (profile-data (unwrap! (map-get? identity-records { identity-serial: identity-serial }) ERROR-ENTITY-NOT-FOUND))
    )
    (ok (is-eq claimed-owner (get account-address profile-data)))
  )
)

;; Records user platform activity
;; @param identity-serial - The identity to update
;; @returns - Success or error response
(define-public (record-platform-activity (identity-serial uint))
  (let
    (
      (current-metrics (default-to 
        { previous-visit: u0, visit-counter: u0, recent-activity: "None" }
        (map-get? user-engagement-metrics { identity-serial: identity-serial })))
    )
    (asserts! (identity-registered? identity-serial) ERROR-ENTITY-NOT-FOUND)
    (map-set user-engagement-metrics
      { identity-serial: identity-serial }
      {
        previous-visit: block-height,
        visit-counter: (+ (get visit-counter current-metrics) u1),
        recent-activity: "platform-visit"
      }
    )
    (ok true)
  )
)

