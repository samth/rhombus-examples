#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         2htdp/universe
         rhombus/private/expression
         rhombus/private/parse)

(provide bigbang
         (rename-out [to-draw draw]
                     [on-tick tick]
                     [on-key key]
                     [on-mouse mouse]
                     [stop-when stop]))

(begin-for-syntax
  (define-syntax-class :binding
    #:datum-literals (block group)
    #:attributes (name expr)
    (pattern (group name:id (block (~and expr (group e ...)))))))

(define-syntax bigbang
  (expression-transformer
   #'rhombus-parameterize
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block group parens)
       [(form-id init
                 (block
                  (group i (block (group rhs ...))) ...)
                 . tail)
        (values
         #'(big-bang (rhombus-expression (group init))
                     (i (rhombus-expression (group rhs ...)))
                     ...)
         #'tail)]
       [_
        (raise-syntax-error #f
                            "big-bang wrongly"
                            stx)]))))
