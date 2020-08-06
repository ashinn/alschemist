(define-library (jkode sassy)
  (export sassy sassy-make-elf sassy-text-list sassy-data-list
          sassy-symbol-table sassy-reloc-list
          sassy-entry-point sassy-data-stack sassy-text-stack
          sassy-heap-size sassy-text-org
          sassy-heap-align sassy-data-align sassy-text-align
          sassy-symbol-name sassy-symbol-scope
          sassy-symbol-section sassy-symbol-offset
          sassy-symbol-size sassy-symbol-unres
          sassy-reloc-name sassy-reloc-section sassy-reloc-target-section
          sassy-reloc-offset sassy-reloc-type sassy-reloc-patcher
          sassy-reloc-value sassy-reloc-width
          sassy-text-size sassy-data-size sassy-symbol-exists?
          sassy-hexdump sassy-print-symbols sassy-print-relocs
          make-pushdown-stack make-pushup-stack
          push-stack-empty? push-stack-push
          push-stack-pointer push-stack-items
          push-stack-patch push-stack-push->patcher
          push-stack-save push-stack-direction
          push-stack-size push-stack-append! push-stack-align)
  (import (scheme base) (scheme char) (scheme cxr) (scheme eval)
          (scheme file) (scheme read) (scheme repl) (scheme write)
          (srfi 1) (srfi 142) (srfi 69) (srfi 98))
  (begin
    (define write-byte write-u8)
    (define ash arithmetic-shift)
    (define logior bitwise-ior)
    (define logand bitwise-and)
    (define inexact->exact inexact)
    (define exact->inexact exact))
  (include
   "sassy/extras.scm"
   "sassy/units.scm"
   "sassy/push-stacks.scm"
   "sassy/api.scm"
   "sassy/intern.scm"
   "sassy/macros.scm"
   "sassy/numbers.scm"
   "sassy/operands.scm"
   "sassy/text-block.scm"
   "sassy/opcodes.scm"
   "sassy/other/srfi-56-pieces.scm"
   "sassy/text.scm"
   "sassy/parse.scm"
   "sassy/main.scm"
   "sassy/flat-bin.scm"
   "sassy/elf.scm"))
