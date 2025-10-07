;; ReputationDAO - Liquid Reputation-Weighted Governance Protocol
;; A sophisticated governance system with contextual expertise domains

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-reputation (err u103))
(define-constant err-proposal-expired (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-invalid-domain (err u106))
(define-constant err-insufficient-stake (err u107))

;; Governance Domains
(define-constant DOMAIN-TECHNICAL u1)
(define-constant DOMAIN-TREASURY u2)
(define-constant DOMAIN-COMMUNITY u3)

;; Reputation decay parameters (blocks)
(define-constant REPUTATION-HALF-LIFE u52560) ;; ~1 year in blocks
(define-constant PROPOSAL-DURATION u1008) ;; ~1 week in blocks
(define-constant MIN-PROPOSAL-BOND u1000)

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var total-reputation uint u0)

;; Data Maps

;; Member reputation across different domains
(define-map member-reputation
    { member: principal, domain: uint }
    {
        reputation-score: uint,
        last-activity-block: uint,
        contributions: uint,
        successful-votes: uint,
        failed-votes: uint
    }
)

;; Proposals
(define-map proposals
    { proposal-id: uint }
    {
        proposer: principal,
        domain: uint,
        title: (string-ascii 256),
        description: (string-utf8 1024),
        start-block: uint,
        end-block: uint,
        reputation-stake: uint,
        votes-for: uint,
        votes-against: uint,
        executed: bool,
        passed: bool
    }
)

;; Voting records
(define-map votes
    { proposal-id: uint, voter: principal }
    {
        vote-power: uint,
        support: bool,
        staked-reputation: uint
    }
)

;; Delegation system
(define-map delegations
    { delegator: principal, domain: uint }
    {
        delegate: principal,
        delegation-block: uint,
        time-lock: uint
    }
)

;; Expertise verification challenges
(define-map expertise-challenges
    { challenge-id: uint, member: principal }
    {
        challenger: principal,
        domain: uint,
        evidence: (string-utf8 512),
        resolved: bool,
        outcome: bool
    }
)

;; Read-only functions

;; Get member reputation in a specific domain
(define-read-only (get-member-reputation (member principal) (domain uint))
    (let
        (
            (reputation-data (map-get? member-reputation { member: member, domain: domain }))
        )
        (match reputation-data
            rep-data (ok (calculate-decayed-reputation 
                (get reputation-score rep-data)
                (get last-activity-block rep-data)))
            (ok u0)
        )
    )
)

;; Calculate reputation with decay
(define-read-only (calculate-decayed-reputation (base-reputation uint) (last-block uint))
    (let
        (
            (blocks-elapsed (- block-height last-block))
            (decay-periods (/ blocks-elapsed REPUTATION-HALF-LIFE))
        )
        (if (<= decay-periods u0)
            base-reputation
            (/ base-reputation (pow u2 decay-periods))
        )
    )
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

;; Get vote details
(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter })
)

;; Get delegation
(define-read-only (get-delegation (delegator principal) (domain uint))
    (map-get? delegations { delegator: delegator, domain: domain })
)

;; Check if proposal is active
(define-read-only (is-proposal-active (proposal-id uint))
    (match (map-get? proposals { proposal-id: proposal-id })
        proposal (and 
            (>= block-height (get start-block proposal))
            (<= block-height (get end-block proposal))
            (not (get executed proposal))
        )
        false
    )
)

;; Public functions

;; Initialize or update member reputation
(define-public (mint-reputation (member principal) (domain uint) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (or (is-eq domain DOMAIN-TECHNICAL) 
                      (is-eq domain DOMAIN-TREASURY) 
                      (is-eq domain DOMAIN-COMMUNITY)) err-invalid-domain)
        (let
            (
                (current-rep (default-to 
                    { reputation-score: u0, last-activity-block: block-height, 
                      contributions: u0, successful-votes: u0, failed-votes: u0 }
                    (map-get? member-reputation { member: member, domain: domain })
                ))
            )
            (map-set member-reputation
                { member: member, domain: domain }
                {
                    reputation-score: (+ (get reputation-score current-rep) amount),
                    last-activity-block: block-height,
                    contributions: (+ (get contributions current-rep) u1),
                    successful-votes: (get successful-votes current-rep),
                    failed-votes: (get failed-votes current-rep)
                }
            )
            (var-set total-reputation (+ (var-get total-reputation) amount))
            (ok true)
        )
    )
)

;; Create a governance proposal
(define-public (create-proposal 
    (domain uint) 
    (title (string-ascii 256)) 
    (description (string-utf8 1024))
    (reputation-stake uint))
    (let
        (
            (proposal-id (+ (var-get proposal-nonce) u1))
            (proposer-rep (unwrap! (get-member-reputation tx-sender domain) err-insufficient-reputation))
        )
        (asserts! (>= proposer-rep MIN-PROPOSAL-BOND) err-insufficient-reputation)
        (asserts! (>= reputation-stake MIN-PROPOSAL-BOND) err-insufficient-stake)
        (asserts! (or (is-eq domain DOMAIN-TECHNICAL) 
                      (is-eq domain DOMAIN-TREASURY) 
                      (is-eq domain DOMAIN-COMMUNITY)) err-invalid-domain)
        
        (map-set proposals
            { proposal-id: proposal-id }
            {
                proposer: tx-sender,
                domain: domain,
                title: title,
                description: description,
                start-block: block-height,
                end-block: (+ block-height PROPOSAL-DURATION),
                reputation-stake: reputation-stake,
                votes-for: u0,
                votes-against: u0,
                executed: false,
                passed: false
            }
        )
        (var-set proposal-nonce proposal-id)
        (ok proposal-id)
    )
)

;; Cast a vote on a proposal
(define-public (vote (proposal-id uint) (support bool) (stake-amount uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
            (domain (get domain proposal))
            (voter-rep (unwrap! (get-member-reputation tx-sender domain) err-insufficient-reputation))
            (existing-vote (map-get? votes { proposal-id: proposal-id, voter: tx-sender }))
        )
        (asserts! (is-none existing-vote) err-already-voted)
        (asserts! (is-proposal-active proposal-id) err-proposal-expired)
        (asserts! (>= voter-rep stake-amount) err-insufficient-reputation)
        
        ;; Record vote
        (map-set votes
            { proposal-id: proposal-id, voter: tx-sender }
            {
                vote-power: voter-rep,
                support: support,
                staked-reputation: stake-amount
            }
        )
        
        ;; Update proposal vote counts
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal {
                votes-for: (if support 
                    (+ (get votes-for proposal) voter-rep) 
                    (get votes-for proposal)),
                votes-against: (if support 
                    (get votes-against proposal) 
                    (+ (get votes-against proposal) voter-rep))
            })
        )
        
        (ok true)
    )
)

;; Execute a proposal after voting period
(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
        )
        (asserts! (> block-height (get end-block proposal)) err-proposal-expired)
        (asserts! (not (get executed proposal)) err-unauthorized)
        
        (let
            (
                (passed (> (get votes-for proposal) (get votes-against proposal)))
            )
            (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal {
                    executed: true,
                    passed: passed
                })
            )
            
            ;; Reward proposer if proposal passed
            (if passed
                (unwrap! (mint-reputation (get proposer proposal) (get domain proposal) u100) err-unauthorized)
                true
            )
            
            (ok passed)
        )
    )
)

;; Delegate voting power to another member
(define-public (delegate-votes (delegate principal) (domain uint) (time-lock uint))
    (begin
        (asserts! (or (is-eq domain DOMAIN-TECHNICAL) 
                      (is-eq domain DOMAIN-TREASURY) 
                      (is-eq domain DOMAIN-COMMUNITY)) err-invalid-domain)
        
        (map-set delegations
            { delegator: tx-sender, domain: domain }
            {
                delegate: delegate,
                delegation-block: block-height,
                time-lock: time-lock
            }
        )
        (ok true)
    )
)

;; Revoke delegation
(define-public (revoke-delegation (domain uint))
    (let
        (
            (delegation (unwrap! (map-get? delegations 
                { delegator: tx-sender, domain: domain }) err-not-found))
        )
        (asserts! (> block-height (+ (get delegation-block delegation) (get time-lock delegation))) 
            err-unauthorized)
        
        (map-delete delegations { delegator: tx-sender, domain: domain })
        (ok true)
    )
)

;; Challenge member expertise
(define-public (challenge-expertise 
    (member principal) 
    (domain uint) 
    (evidence (string-utf8 512)))
    (let
        (
            (challenger-rep (unwrap! (get-member-reputation tx-sender domain) err-insufficient-reputation))
        )
        (asserts! (>= challenger-rep u500) err-insufficient-reputation)
        
        (map-set expertise-challenges
            { challenge-id: (var-get proposal-nonce), member: member }
            {
                challenger: tx-sender,
                domain: domain,
                evidence: evidence,
                resolved: false,
                outcome: false
            }
        )
        (ok true)
    )
)

;; Resolve expertise challenge (owner only for now)
(define-public (resolve-challenge (challenge-id uint) (member principal) (outcome bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (let
            (
                (challenge (unwrap! (map-get? expertise-challenges 
                    { challenge-id: challenge-id, member: member }) err-not-found))
            )
            (map-set expertise-challenges
                { challenge-id: challenge-id, member: member }
                (merge challenge {
                    resolved: true,
                    outcome: outcome
                })
            )
            
            ;; Penalize if challenge upheld
            (if outcome
                (let
                    (
                        (member-rep (unwrap! (map-get? member-reputation 
                            { member: member, domain: (get domain challenge) }) err-not-found))
                    )
                    (map-set member-reputation
                        { member: member, domain: (get domain challenge) }
                        (merge member-rep {
                            reputation-score: (/ (get reputation-score member-rep) u2)
                        })
                    )
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

;; Initialize contract
(begin
    (var-set proposal-nonce u0)
    (var-set total-reputation u0)
)